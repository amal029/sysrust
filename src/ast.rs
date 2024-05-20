type Pos = (usize, usize);

#[derive(Clone, Debug, PartialEq)]
pub enum Symbol {
    Symbol(String, Pos),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprOp {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Pow,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SimpleDataExpr {
    SimpleBinaryOp(Box<SimpleDataExpr>, ExprOp, Box<SimpleDataExpr>, Pos),
    VarRef(Symbol, Pos),
    SignalRef(Symbol, Pos),
    ConstI(i64, Pos),
    ConstF(f64, Pos),
}

#[derive(Clone, Debug, PartialEq)]
pub enum RelDataExpr {
    LessThan(SimpleDataExpr, SimpleDataExpr, Pos),
    GreaterThan(SimpleDataExpr, SimpleDataExpr, Pos),
    LessThanEqual(SimpleDataExpr, SimpleDataExpr, Pos),
    GreaterThanEqual(SimpleDataExpr, SimpleDataExpr, Pos),
    EqualTo(SimpleDataExpr, SimpleDataExpr, Pos),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    True(Pos),
    False(Pos),
    Esymbol(Symbol, Pos),
    And(Box<Expr>, Box<Expr>, Pos),
    Or(Box<Expr>, Box<Expr>, Pos),
    Not(Box<Expr>, Pos),
    Brackets(Box<Expr>, Pos),
    DataExpr(RelDataExpr, Pos),
}

#[derive(Clone, Debug, PartialEq)]
pub enum IO {
    Input,
    Output,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ASQual {
    Weak,
    Immediate,
    WeakImmediate,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>, Pos),
    Pause(Symbol, Pos),
    Emit(Symbol, Option<SimpleDataExpr>, Pos),
    Present(Expr, Box<Stmt>, Option<Box<Stmt>>, Pos),
    Signal(Symbol, Option<IO>, Pos),
    Abort(Symbol, Option<ASQual>, Box<Stmt>, Pos),
    Suspend(Symbol, Option<ASQual>, Box<Stmt>, Pos),
    Loop(Box<Stmt>, Pos),
    Assign(Symbol, SimpleDataExpr, Pos),
    Spar(Vec<Stmt>, Pos),
    Noop(Pos)
}
