/*
To implement brain f***,
I need:
1 To handle characters / bytes
2 To handle memory management
3 To handle input
*4 A little bit of generics 
*/

type Expr =
    | Int(int)
    | Add(ref Expr, ref Expr)
    | Print(Expr)

type Reg = {
    index : uint
}

type Instr =
    | Load(Reg, int)
    | Move(Reg, Reg)
    | Add(Reg, Reg, Reg)
    | Print(Reg)

type Registers = {
    vals : ref [int]
}

fun make_reg(index: uint) -> Reg = 
    Reg{ index = index}

fun new_reg(next_reg : ref mut uint) -> Reg = do
    let old_val = next_reg^;
    next_reg^ = next_reg^ +  1 as uint;
    make_reg(old_val)
end



fun at(regs: ref Registers, reg: Reg) -> int = loop end
fun at_mut(regs: ref Registers, reg: Reg) -> ref mut int = loop end

fun concat(instrs: ref [Instr], next: ref [Instr]) -> ref [Instr]=  loop end

fun lower(expr: ref Expr, mut next_reg : uint) -> (ref [Instr],Reg) = match expr^ with
    Expr.Int(value) => do
        let target = make_reg(next_reg);
        (ref [Instr.Load(target,value)],target)
    end,
    Expr.Add(left,right) => do 
        let left_reg = new_reg(ref mut next_reg);
        let (left_instrs,left_reg) = lower(left,left_reg.index);

        let right_reg = new_reg(ref mut next_reg);
        let (right_instrs,right_reg) = lower(right,right_reg.index); 

        (concat(left_instrs,concat(right_instrs,ref [Instr.Add(left_reg,left_reg,right_reg)])),left_reg)
    end,
    Expr.Print(expr) => do
        let (instrs,res) = lower(expr, next_reg);
        (concat(instrs,ref [Instr.Print(res)]),res)
    end

end
fun exec(registers: ref mut Registers, inst: Instr) = 
    match inst with
        Instr.Load(reg,val) => at_mut(registers,reg)^ = val,
        Instr.Add(dst,src1,src2) => at_mut(registers,dst)^ = at(registers,src1) + at(registers,src2),
        Instr.Print(output) => println(at(registers,output))
    end

fun main() = do
    let a = [Instr.Load(make_reg(0),1),Instr.Load(make_reg(1),2),Instr.Add(make_reg(2),make_reg(0),make_reg(1)),Instr.Print(make_reg(2))];
    exec(ref [0,0,0,0],a);
end