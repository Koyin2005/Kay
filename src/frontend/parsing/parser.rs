use crate::Lexer;

pub struct Parser<'source>{
    lexer : Lexer<'source>
}

impl<'source> Parser<'source>{
    pub fn new(lexer : Lexer<'source>) -> Self{
        Self { lexer }
    }
}