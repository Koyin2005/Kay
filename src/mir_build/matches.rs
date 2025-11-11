use indexmap::IndexMap;

type Map<K, V> = IndexMap<K, V>;
use crate::{
    MirBuilder,
    frontend::{
        hir::{DefId, IntType},
        thir::{Arm, ArmId, Expr, ExprId, Pattern, PatternKind},
    },
    indexvec::Idx,
    mir::{BasicBlock, Constant, Place, Value},
    mir_build::PlaceBuilder,
    types::{FieldIndex, Type, VariantCaseIndex},
};
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TestBranch {
    Success,
    Equal(Constant),
    Case(VariantCaseIndex),
    Fail,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TestCase {
    Equal(Constant),
    Case(DefId, VariantCaseIndex),
}
#[derive(Clone)]
pub enum Test {
    Switch(DefId),
    Equal(Constant),
    SwitchInt,
    If,
}
#[derive(Debug, Clone)]
pub struct PlaceTest {
    place: Place,
    test: TestCase,
}
#[derive(Debug, Clone)]
struct Branch {
    tests: Vec<PlaceTest>,
    arm: ArmId,
    pre_binding_block: Option<BasicBlock>,
    otherwise_block: Option<BasicBlock>,
    false_edge_start: Option<BasicBlock>,
}
fn pattern_into_tests(pattern: &Pattern, place: PlaceBuilder) -> Vec<PlaceTest> {
    match pattern.kind {
        PatternKind::Wilcard | PatternKind::Binding(..) => Vec::new(),
        PatternKind::Tuple(ref fields) => fields
            .into_iter()
            .enumerate()
            .flat_map(|(i, field)| {
                let field_index = FieldIndex::new(i);
                pattern_into_tests(field, place.clone().field(field_index))
            })
            .collect(),
        PatternKind::Lit(literal_kind) => vec![PlaceTest {
            place: place.to_place(),
            test: TestCase::Equal(MirBuilder::constant_from_literal(literal_kind)),
        }],
        PatternKind::Case(id, case, _, ref fields) => std::iter::once(PlaceTest {
            place: place.clone().to_place(),
            test: TestCase::Case(id, case),
        })
        .chain(fields.into_iter().enumerate().flat_map(|(i, field)| {
            let field_index = FieldIndex::new(i);
            pattern_into_tests(field, place.clone().field_with_variant(field_index, case))
        }))
        .collect(),
    }
}
pub struct MatchBuilder<'a, 'body> {
    mir_builder: &'a mut MirBuilder<'body>,
    destination: Place,
    matched_place: PlaceBuilder,
    branches: Vec<Branch>,
    expr: &'a Expr,
}

impl<'a, 'body> MatchBuilder<'a, 'body> {
    pub fn new(
        mir_builder: &'a mut MirBuilder<'body>,
        place: Place,
        matched_expr: ExprId,
        arms: &'a [ArmId],
        expr: &'a Expr,
    ) -> Self {
        let matched_place = mir_builder.lower_expr_as_place(matched_expr);
        let branches = arms
            .into_iter()
            .copied()
            .map(|arm| {
                let Arm { pattern, body: _ } = mir_builder.thir.arm(arm);
                Branch {
                    tests: pattern_into_tests(pattern, matched_place.clone()),
                    arm,
                    pre_binding_block: None,
                    otherwise_block: None,
                    false_edge_start: None,
                }
            })
            .collect();
        Self {
            mir_builder,
            matched_place,
            destination: place,
            branches,
            expr,
        }
    }
    fn take_branch(
        &mut self,
        test: &Test,
        place: &Place,
        branch: &mut Branch,
    ) -> Option<TestBranch> {
        let (index, place_test) = branch
            .tests
            .iter()
            .enumerate()
            .find(|(_, test)| test.place == *place)?;
        let ret = match (test, &place_test.test) {
            (Test::If, TestCase::Equal(Constant::Bool(value))) => {
                if *value {
                    Some(TestBranch::Success)
                } else {
                    Some(TestBranch::Fail)
                }
            }
            (Test::Switch(switch_id), TestCase::Case(id, variant)) if switch_id == id => {
                Some(TestBranch::Case(*variant))
            }
            (Test::SwitchInt, TestCase::Equal(Constant::Int(value))) => {
                Some(TestBranch::Equal(Constant::Int(*value)))
            }
            (Test::Equal(test_constant), TestCase::Equal(constant))
                if test_constant == constant =>
            {
                Some(TestBranch::Equal(constant.clone()))
            }
            _ => None,
        };
        ret.map(|test_branch| {
            branch.tests.remove(index);
            test_branch
        })
    }
    fn group_branches<'b, 'c>(
        &mut self,
        test: &Test,
        place: &Place,
        mut branches: &'b mut [&'c mut Branch],
    ) -> (
        Map<TestBranch, Vec<&'b mut Branch>>,
        &'b mut [&'c mut Branch],
    ) {
        let mut targets = Map::default();
        while let Some(branch) = branches.first_mut() {
            let Some(test_branch) = self.take_branch(test, place, branch) else {
                break;
            };
            let (first, rest) = branches.split_first_mut().unwrap();
            targets
                .entry(test_branch)
                .or_insert(Vec::new())
                .push(&mut **first);
            branches = rest;
        }
        (targets, branches)
    }
    fn perform_test(
        &mut self,
        place: Place,
        test: &Test,
        start_block: BasicBlock,
        targets: &Map<TestBranch, BasicBlock>,
        otherwise_block: BasicBlock,
    ) {
        let get_branch = |target| targets.get(&target).cloned().unwrap_or(otherwise_block);
        match test {
            Test::If => {
                self.mir_builder.switch_to_block(start_block);
                self.mir_builder.if_then_else(
                    self.expr.span,
                    Value::Load(place),
                    get_branch(TestBranch::Success),
                    get_branch(TestBranch::Fail),
                );
            }
            Test::Equal(test_const) => {
                self.mir_builder.switch_to_block(start_block);
                let equal_tmp = self
                    .mir_builder
                    .push_equal_tmp(Value::Constant(test_const.clone()), self.expr.span);
                self.mir_builder.if_then_else(
                    self.expr.span,
                    Value::Load(equal_tmp.into()),
                    get_branch(TestBranch::Success),
                    get_branch(TestBranch::Fail),
                );
            }
            Test::Switch(_) => {
                let targets = targets
                    .iter()
                    .filter_map(|(branch, &block)| {
                        if let TestBranch::Case(case) = branch {
                            Some((
                                Constant::Int(
                                    case.into_index().try_into().expect("Cannot use discrimant"),
                                ),
                                block,
                            ))
                        } else {
                            None
                        }
                    })
                    .collect();
                self.mir_builder.switch_to_block(start_block);
                let discr_tmp = self.mir_builder.new_temp(Type::new_int(IntType::Unsigned));
                self.mir_builder.push_assign(
                    discr_tmp.into(),
                    crate::mir::Rvalue::Discrimant(place),
                    self.expr.span,
                );
                self.mir_builder.switch(
                    self.expr.span,
                    Value::Load(discr_tmp.into()),
                    targets,
                    get_branch(TestBranch::Fail),
                );
            }
            Test::SwitchInt => {
                let targets = targets
                    .iter()
                    .filter_map(|(branch, &block)| {
                        if let TestBranch::Equal(Constant::Int(value)) = branch {
                            Some((Constant::Int(*value), block))
                        } else {
                            None
                        }
                    })
                    .collect();
                self.mir_builder.switch_to_block(start_block);
                self.mir_builder.switch(
                    self.expr.span,
                    Value::Load(place),
                    targets,
                    get_branch(TestBranch::Fail),
                );
            }
        }
    }
    fn build_match_tree(&mut self, branches: &mut [&mut Branch]) -> BasicBlock {
        let start_block = self.mir_builder.current_block;
        if let [first, ..] = branches
            && first.false_edge_start.is_none()
        {
            first.false_edge_start = Some(start_block);
        }
        match branches {
            [] => start_block,
            [first, rest @ ..] if first.tests.is_empty() => {
                let otherwise_block = self.mir_builder.new_block();
                first.pre_binding_block = Some(start_block);
                first.otherwise_block = Some(otherwise_block);
                self.mir_builder.switch_to_block(otherwise_block);
                self.build_match_tree(rest)
            }
            branches => {
                let place_test = &branches[0].tests[0];
                let place = place_test.place.clone();
                let test = match place_test.test {
                    TestCase::Equal(Constant::Bool(_)) => Test::If,
                    TestCase::Case(id, _) => Test::Switch(id),
                    TestCase::Equal(Constant::Int(_)) => Test::SwitchInt,
                    TestCase::Equal(ref val) => Test::Equal(val.clone()),
                };
                let (targets, rest) = self.group_branches(&test, &place, branches);

                let rest_block = self.mir_builder.new_block();
                let targets = targets
                    .into_iter()
                    .map(|(branch, mut branches)| {
                        let branch_start = self.mir_builder.switch_to_new_block();
                        let branch_otherwise = self.build_match_tree(&mut branches);
                        self.mir_builder.switch_to_block(branch_otherwise);
                        self.mir_builder.goto(self.expr.span, rest_block);
                        (branch, branch_start)
                    })
                    .collect::<Map<_, _>>();
                self.mir_builder.switch_to_block(rest_block);
                let otherwise_block = self.build_match_tree(rest);
                self.perform_test(place, &test, start_block, &targets, rest_block);
                otherwise_block
            }
        }
    }
    pub fn match_expr(mut self) {
        let mut branches = std::mem::replace(&mut self.branches, Vec::new());
        let mut branches = branches.iter_mut().collect::<Vec<_>>();
        let otherwise_block = self.build_match_tree(&mut branches);
        self.mir_builder.switch_to_block(otherwise_block);
        self.mir_builder.unreachable(self.expr.span);
        let mut next_candidate_start_block = None;
        for branch in branches.iter_mut().rev() {
            if let Some(next_candidate_start) = next_candidate_start_block {
                let pre_binding_block = branch.pre_binding_block.unwrap();
                let actual_binding_block = self.mir_builder.new_block();
                self.mir_builder.switch_to_block(pre_binding_block);
                self.mir_builder.false_edge(
                    self.expr.span,
                    actual_binding_block,
                    next_candidate_start,
                );
                branch.pre_binding_block = Some(actual_binding_block);
            }
            next_candidate_start_block = branch.false_edge_start;
        }
        let arm_end_blocks = branches
            .iter_mut()
            .map(|branch| {
                let block = branch.pre_binding_block.unwrap();
                self.mir_builder.switch_to_block(block);
                let Arm { pattern, body } = self.mir_builder.thir.arm(branch.arm);
                self.mir_builder
                    .place_into_pattern(self.matched_place.clone(), pattern);
                self.mir_builder
                    .expr_into_place(*body, self.destination.clone());
                self.mir_builder.current_block
            })
            .collect::<Vec<_>>();
        let exit_block = self.mir_builder.new_block();
        for block in arm_end_blocks {
            self.mir_builder.switch_to_block(block);
            self.mir_builder.goto(self.expr.span, exit_block);
        }
        self.mir_builder.switch_to_block(exit_block);
    }
}
