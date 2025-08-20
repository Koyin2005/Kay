use std::{
    fmt::Debug,
    hash::Hash,
    rc::Rc,
    str::Lines,
    sync::{LazyLock, Mutex},
};

use indexmap::IndexSet;

pub mod symbol;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Hash)]
pub struct SpanInfo {
    pub file : u16,
    pub start_offset: u32,
    pub end_offset: u32,
}
impl SpanInfo {
    pub fn len(&self) -> u32 {
        self.end_offset - self.start_offset
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Hash, Debug)]
pub struct SourceLocation {
    pub line: u32,
    pub column: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Hash)]
pub struct LineInfo {
    line_number: u32,
    start_offset: u32,
    end_offset: u32,
}
impl LineInfo {
    pub fn line_number(&self) -> u32 {
        self.line_number
    }

    pub fn start_offset(&self) -> u32 {
        self.start_offset
    }
    pub fn end_offset(&self) -> u32 {
        self.end_offset
    }
    pub fn location_within(&self, offset: u32, source: &str) -> SourceLocation {
        let start_offset = self.start_offset as usize;
        let mut last_column = 0;
        let Some(column) = source[start_offset..self.end_offset as usize]
            .char_indices()
            .enumerate()
            .find_map(|(i, (rel_offset, char))| {
                last_column = i + char.len_utf8();
                (start_offset + rel_offset <= offset as usize
                    && (offset as usize) < start_offset + rel_offset + char.len_utf8())
                .then_some(i)
            })
        else {
            return SourceLocation {
                line: self.line_number,
                column: last_column as u32 + 1,
            };
        };
        SourceLocation {
            line: self.line_number,
            column: column as u32 + 1,
        }
    }
}

static SPAN_INTERNER: LazyLock<Mutex<SpanInterner>> =
    LazyLock::new(|| Mutex::new(SpanInterner::new()));
#[derive(Clone, Copy, Eq)]
pub struct Span {
    index_or_offset: u32,
    len_or_marker: u16,
    file_index : u16
}
impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        self.info() == other.info()
    }
}
impl Span {
    pub const EMPTY: Self = Self {
        index_or_offset: 0,
        len_or_marker: 0,
        file_index : 0
    };
    pub fn new(start_offset: u32, len: u32, file: u16) -> Self {
        let (index_or_offset, len_or_marker) = if len >= u16::MAX as u32 {
            (
                SPAN_INTERNER.lock().unwrap().intern(&SpanInfo {
                    start_offset,
                    end_offset: start_offset + len,
                    file
                }),
                u16::MAX,
            )
        } else {
            (start_offset, len as u16)
        };
        Self {
            index_or_offset,
            len_or_marker,
            file_index: file
        }
    }
    pub fn is_empty(self) -> bool {
        self.info().len() == 0
    }
    pub fn info(self) -> SpanInfo {
        if self.len_or_marker == u16::MAX {
            SPAN_INTERNER.lock().unwrap().seen[self.index_or_offset as usize]
        } else {
            SpanInfo {
                start_offset: self.index_or_offset,
                end_offset: self.index_or_offset + self.len_or_marker as u32,
                file : self.file_index
            }
        }
    }
    pub fn combined(self, other: Self) -> Self {
        let self_info = self.info();
        let other_info = other.info();
        let start_offset = self_info.start_offset.min(other_info.start_offset);
        let end_offset = self_info.end_offset.max(other_info.end_offset);
        Self::new(start_offset, end_offset - start_offset, self_info.file)
    }
    pub fn with_lower(self, other_offset: u32) -> Self {
        let info = self.info();
        if other_offset < info.start_offset {
            Self::new(other_offset, info.end_offset - other_offset, info.file)
        } else {
            self
        }
    }

    pub fn with_low(self, low: u32) -> Self {
        let info = self.info();
        Self::new(low, info.end_offset - low, info.file)
    }
    pub fn with_high(self, high: u32) -> Self {
        let info = self.info();
        Self::new(info.start_offset, high - info.start_offset, info.file)
    }
    pub fn start(self) -> Self {
        let info = self.info();
        Self::new(info.start_offset, 0,info.file)
    }
    pub fn end(self) -> Self {
        let info = self.info();
        Self::new(info.end_offset, 0, info.file)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let info = self.info();
        f.debug_struct("Span")
            .field("start", &info.start_offset)
            .field("end", &info.end_offset)
            .finish()
    }
}
impl Hash for Span {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.info().hash(state);
    }
}
struct SpanInterner {
    seen: IndexSet<SpanInfo>,
}
impl SpanInterner {
    fn new() -> Self {
        Self {
            seen: Default::default(),
        }
    }

    fn intern(&mut self, data: &SpanInfo) -> u32 {
        let (index, _) = self.seen.insert_full(*data);
        index
            .try_into()
            .expect("There should be less than u32::MAX spans")
    }
}
pub type SourceFilesRef = Rc<SourceFiles>;

pub type SourceRef = Rc<SourceInfo>;
pub struct SourceTooLarge;
#[derive(Clone)]
pub struct SourceInfo {
    name : Box<str>,
    source: Box<str>,
    lines: Box<[LineInfo]>,
}

impl SourceInfo {
    pub fn new(name: String,source: String) -> Result<Self, SourceTooLarge> {
        if source.len() >= u32::MAX as usize {
            return Err(SourceTooLarge);
        }
        let mut has_newline_at_end = false;
        let mut source_offset = 0;
        let mut lines = source
            .split_inclusive('\n')
            .enumerate()
            .map(|(line_index, line_src)| {
                let start = source_offset as u32;
                let end = if line_src.ends_with("\r\n") {
                    has_newline_at_end = true;
                    source_offset + line_src.len() - 2
                } else if line_src.ends_with('\n') {
                    has_newline_at_end = true;
                    source_offset + line_src.len() - 1
                } else {
                    has_newline_at_end = false;
                    source_offset + line_src.len()
                } as u32;
                source_offset += line_src.len();
                LineInfo {
                    line_number: (line_index + 1) as u32,
                    start_offset: start,
                    end_offset: end,
                }
            })
            .collect::<Vec<_>>();
        if has_newline_at_end {
            lines.push(LineInfo {
                line_number: (lines.len() + 1) as u32,
                start_offset: source_offset as u32 - 1,
                end_offset: source_offset as u32,
            });
        }
        Ok(SourceInfo {
            name : name.into_boxed_str(),
            source: source.into_boxed_str(),
            lines: lines.into_boxed_slice(),
        })
    }
    pub fn name(&self) -> &str{
        &self.name
    }
    pub fn source(&self) -> &str {
        &self.source
    }
    pub fn source_lines(&self) -> Lines<'_> {
        self.source.lines()
    }
    pub fn line_info(&self) -> &[LineInfo] {
        &self.lines
    }
    pub fn line_at(&self, offset: u32) -> &LineInfo {
        let Some(line) = self
            .lines
            .iter()
            .find(|line| line.start_offset <= offset && offset <= line.end_offset)
        else {
            panic!("'{offset}' is not within any line.")
        };
        line
    }
    pub fn source_within(&self, start_offset: u32, end_offset: u32) -> &str {
        &self.source[start_offset as usize..end_offset as usize]
    }
    pub fn location_at(&self, offset: u32) -> SourceLocation {
        let Some(line) = self
            .lines
            .iter()
            .find(|line| line.start_offset <= offset && offset <= line.end_offset)
        else {
            let last_line = self.lines.last().expect("There should be at least 1 line");
            return SourceLocation {
                line: last_line.line_number,
                column: {
                    self.source_within(last_line.start_offset, last_line.end_offset)
                        .char_indices()
                        .map(|(i, c)| i + c.len_utf8())
                        .next_back()
                        .unwrap_or(1) as u32
                },
            };
        };
        line.location_within(offset, &self.source)
    }
}

pub struct SourceFiles {
    info: Box<[Rc<SourceInfo>]>,
}
impl SourceFiles {
    pub fn new(sources: impl IntoIterator<Item = (String,String)>) -> Result<Self, SourceTooLarge> {
        sources
            .into_iter()
            .map(|(mut name,source)| {
                if let Some(index) = name.find(|c| c == '.'){
                    name.drain(0..index);
                };
                SourceInfo::new(name,source)
            })
            .collect::<Result<Box<[_]>, _>>()
            .map(|info| Self {
                info: info.into_iter().map(Rc::new).collect(),
            })
    }
    pub fn get_source_files(&self) -> &[Rc<SourceInfo>] {
        &self.info
    }
}
