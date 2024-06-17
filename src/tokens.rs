use logos::Logos;
use std::fmt; // to implement the Display trait later
use std::num::ParseFloatError;
use std::num::ParseIntError;

#[derive(Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidInteger(ParseIntError),
    InvalidFloat(ParseFloatError),
    InvalidToken,
}

impl Default for LexicalError {
    fn default() -> Self {
	LexicalError::InvalidToken
    }
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
    #[token("halt")]
    Halt,
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
    #[token("await")]
    Await,
    #[token("sustain")]
    Sustain,

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
    #[token(",")]
    OpComma,

    #[token("false")]
    False,
    #[token("true")]
    True,
    // XXX: Combination tokens
    #[token("and")]
    OpAnd,
    #[token("or")]
    OpOr,
    #[token("||")]
    OpPar,
    #[token("int")]
    TInt,
    #[token("float")]
    TFloat,
    #[token("extern")]
    TExtern,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
