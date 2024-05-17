use logos::Logos;
use std::fmt; // to implement the Display trait later
use std::num::ParseFloatError;
use std::num::ParseIntError;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidInteger(ParseIntError),
    InvalidFloat(ParseFloatError),
    #[default]
    InvalidToken,
}

impl From<ParseFloatError> for LexicalError {
    fn from(err: ParseFloatError) -> Self {
        LexicalError::InvalidFloat(err)
    }
}

impl From<ParseIntError> for LexicalError {
    fn from(err: ParseIntError) -> Self {
        LexicalError::InvalidInteger(err)
    }
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+", skip r"//.*\n?", error = LexicalError)]
pub enum Token {
    // XXX: Keywords in the language
    #[token("emit")]
    Emit,
    #[token("pause")]
    Pause,
    #[token("signal")]
    Signal,
    #[token("loop")]
    Loop,
    #[token("abort")]
    Abort,
    #[token("suspend")]
    Suspend,
    #[token("weak")]
    Weak,
    #[token("immediate")]
    Immediate,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("input")]
    Input,
    #[token("output")]
    Output,

    // XXX: Varaible, signal, etc name
    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice().to_string())]
    Symbol(String),

    // XXX: Numbers
    #[regex("-?[0-9]+\\.[0-9]+", |lex| lex.slice().parse())]
    Float(f64),
    #[regex("-?[0-9]+", |lex| lex.slice().parse())]
    Integer(i64),

    // XXX: Braces
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    // XXX: Semicolon for statement completion
    #[token(";")]
    Semicolon,

    // XXX: Operators in the language
    #[token("?")]
    OpQ,
    #[token("+")]
    OpAdd,
    #[token("-")]
    OpSub,
    #[token("*")]
    OpMul,
    #[token("/")]
    OpDiv,
    #[token("^^")]
    OpPow,
    #[token("%")]
    OpMod,
    #[token("=")]
    OpEqual,
    #[token("==")]
    OpEqualEqual,
    #[token("<=")]
    OpLEEqual,
    #[token(">=")]
    OpGEEqual,
    #[token("<")]
    OpLT,
    #[token(">")]
    OpGT,
    #[token("!")]
    OpNot,
    #[token("nothing")]
    OpNothing,
    #[token(":")]
    OpColon,

    // XXX: Combination tokens
    #[token("and")]
    OpAnd,
    #[token("or")]
    OpOr,
    #[token("||")]
    OpPar,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "HUHU: {:?}", self)
    }
}