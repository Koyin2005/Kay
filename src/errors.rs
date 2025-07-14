use std::{borrow::Cow, cell::RefCell};

use crate::{
    SourceInfo,
    span::Span,
};

pub struct DiagnosticReporter<'a>(RefCell<DiagnosticReporterInner<'a>>);
struct DiagnosticReporterInner<'a> {
    source_info: &'a SourceInfo,
    all_diagnostics: Vec<Diagnostic>,
}

impl<'a> DiagnosticReporter<'a> {
    pub fn new(source_info: &'a SourceInfo) -> Self {
        Self(RefCell::new(DiagnosticReporterInner {
            source_info,
            all_diagnostics: Vec::new(),
        }))
    }
    pub fn had_error(&self) -> bool {
        !self.0.borrow().all_diagnostics.is_empty()
    }
    pub fn add(&self, message: Diagnostic) {
        self.0.borrow_mut().all_diagnostics.push(message);
    }
    pub fn emit(&self) {
        let mut inner = self.0.borrow_mut();
        for diagnostic in std::mem::take(&mut inner.all_diagnostics) {
            let span_info = diagnostic.span.info();

            let start = inner.source_info.location_at(span_info.start_offset);
            let end = inner.source_info.location_at(span_info.end_offset);
            eprintln!(
                "Error on line {}, column {}:\n {}",
                start.line, start.column, diagnostic.message
            );
            if start.line == end.line {
                let line = inner.source_info.line_info()[start.line as usize - 1];
                eprintln!(
                    "  {}",
                    &inner.source_info.source()
                        [line.start_offset() as usize..line.end_offset() as usize]
                );
                eprint!("  ");
                for _ in 1..start.column {
                    eprint!(" ");
                }
                for _ in start.column..=end.column {
                    eprint!("^");
                }
                eprintln!()
            } else {
                let lines = &inner.source_info.line_info()[start.line as usize - 1 .. end.line as usize];
                for line in lines{
                    eprintln!("  {}",inner.source_info.source_within(line.start_offset(), line.end_offset()));

                    let start_of_line = if line.line_number() == start.line { start } else { 
                        let first_non_space_char =  inner.source_info
                        .source_within(line.start_offset(), line.end_offset())
                        .char_indices().filter_map(|(i,c)| (!c.is_ascii_whitespace()).then_some(i))
                        .next().unwrap_or(0) as u32 + line.start_offset();
                        inner.source_info.location_at(first_non_space_char)
                    };
                    eprint!("  ");
                    for _ in 1..start_of_line.column {
                        eprint!(" ");
                    }
                    let end_of_line = line.location_within(line.end_offset(), inner.source_info.source());
                    for _ in start_of_line.column..=end_of_line.column {
                        eprint!("^");
                    }
                    eprintln!()
                    
                }
                eprintln!()
                
            }
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
    fn into_message(self) -> Cow<'static,str>;
}

impl IntoDiagnosticMessage for String{
    fn into_message(self) -> Cow<'static,str> {
        self.into()
    }
}
impl IntoDiagnosticMessage for Box<str>{
    fn into_message(self) -> Cow<'static,str> {
        self.into_string().into()
    }
}

impl IntoDiagnosticMessage for &'static str{
    fn into_message(self) -> Cow<'static,str> {
        self.into()
    }
}