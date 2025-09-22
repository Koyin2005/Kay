use std::{borrow::Cow, cell::RefCell};

use crate::span::{SourceFilesRef, SourceLocation, Span};

pub struct DiagnosticReporter(RefCell<DiagnosticReporterInner>);
struct DiagnosticReporterInner {
    source_info: SourceFilesRef,
    all_diagnostics: Vec<Diagnostic>,
}

impl DiagnosticReporter {
    pub fn new(source_info: SourceFilesRef) -> Self {
        Self(RefCell::new(DiagnosticReporterInner {
            source_info,
            all_diagnostics: Vec::new(),
        }))
    }
    pub fn emit_diag(&self, message: impl IntoDiagnosticMessage, span: Span) {
        self.add(Diagnostic::new(message, span));
    }
    pub fn had_error(&self) -> bool {
        !self.0.borrow().all_diagnostics.is_empty()
    }
    pub fn add(&self, message: Diagnostic) {
        self.0.borrow_mut().all_diagnostics.push(message);
    }
    pub fn span_info(&self, span: Span) -> (SourceLocation,SourceLocation){
        let span = span.info();
        let inner = self.0.borrow();
        let info = inner.source_info.get_source_files()[span.file as usize].as_ref();
        let start = info.location_at(span.start_offset);
        let end = info.location_at(span.end_offset);
        (start,end)
    }
    pub fn emit(&self) {
        let mut inner = self.0.borrow_mut();
        for diagnostic in std::mem::take(&mut inner.all_diagnostics) {
            let span_info = diagnostic.span.info();
            let file = span_info.file;
            let source_info = inner.source_info.get_source_files()[file as usize].as_ref();
            let start = source_info.location_at(span_info.start_offset);
            let end = source_info.location_at(span_info.end_offset);
            eprintln!(
                "Error on line {}, column {} in {}:\n {}",
                start.line,
                start.column,
                source_info.name(),
                diagnostic.message
            );
            let lines = &source_info.line_info()[start.line as usize - 1..end.line as usize];

            let max_digits = lines
                .last()
                .expect("There should be a line at the end")
                .line_number()
                .ilog10()
                + 1;
            for line in lines {
                let line_source = source_info.source_within(line.start_offset(), line.end_offset());
                for _ in 0..max_digits - (line.line_number().ilog10() + 1) {
                    eprint!(" ");
                }
                eprint!("{}", line.line_number());
                eprintln!("  {line_source}");

                let start_of_line = if line.line_number() == start.line {
                    start
                } else {
                    let first_non_space_char = line_source
                        .char_indices()
                        .filter_map(|(i, c)| (!c.is_ascii_whitespace()).then_some(i))
                        .next()
                        .map(|offset| offset as u32 + line.start_offset())
                        .unwrap_or(line.start_offset());
                    source_info.location_at(first_non_space_char)
                };

                let end_of_line = if line.line_number() == end.line {
                    end
                } else {
                    line.location_within(line.end_offset(), source_info.source())
                };

                let start_column = start_of_line.column; //if is_whitespace_line { start_of_line.column.min(start.column)} else { start_of_line.column};
                let end_column = end_of_line.column.max(start_column + 1); //if is_whitespace_line {end_of_line.column.max(start.column+1)} else { end_of_line.column};

                for _ in 0..max_digits {
                    eprint!(" ");
                }
                eprint!("  ");
                for _ in 1..start_column {
                    eprint!(" ");
                }
                for _ in start_column..end_column {
                    eprint!("^");
                }
                eprintln!()
            }
            eprintln!()
        }
    }
}

pub struct Diagnostic {
    message: Cow<'static, str>,
    span: Span,
}
impl Diagnostic {
    pub fn new(message: impl IntoDiagnosticMessage, span: Span) -> Self {
        Self {
            message: message.into_message(),
            span,
        }
    }
}

pub trait IntoDiagnosticMessage {
    fn into_message(self) -> Cow<'static, str>;
}

impl IntoDiagnosticMessage for String {
    fn into_message(self) -> Cow<'static, str> {
        self.into()
    }
}
impl IntoDiagnosticMessage for Box<str> {
    fn into_message(self) -> Cow<'static, str> {
        self.into_string().into()
    }
}

impl IntoDiagnosticMessage for &'static str {
    fn into_message(self) -> Cow<'static, str> {
        self.into()
    }
}
