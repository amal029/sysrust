use std::collections::HashMap;

use crate::error::print_bytes;
use itertools::join;
use pretty::RcDoc;
pub type Pos = (usize, usize);

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Symbol {
    Symbol(String, Pos),
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Type {
    Int,
    Float,
    Struct(StructTypeT),
    Array(Box<Type>, ArrayAccessType),
    None,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprOp {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Pow,
    LShift,
    RShift
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructRefT {
    StructRef(Symbol, Symbol, Pos)
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArrayRefT{
    ArrayRef(Symbol, Vec<SimpleDataExpr>, Pos)
}

#[derive(Clone, Debug, PartialEq)]
pub enum SimpleDataExpr {
    SimpleBinaryOp(Box<SimpleDataExpr>, ExprOp, Box<SimpleDataExpr>, Pos),
    VarRef(Symbol, Pos),
    SignalRef(Symbol, Pos),
    ConstI(i64, Pos),
    ConstF(f64, Pos),
    Call(Symbol, Vec<SimpleDataExpr>, Pos),
    AggregateAssign(InitializerList, Pos),
    StructRef(StructRefT),
    ArrayRef(ArrayRefT),
    Cast(Type, Box<SimpleDataExpr>, Pos),
}

// The array size/dims for defining an array
#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum ArrayAccessType {
    ArrayAccessInt(i64, Pos),
    ArrayAccessSymbol(Symbol, Pos),
}

// ArrayType
#[derive(Clone, Debug, PartialEq)]
pub enum ArrayType {
    ArrayPrimTypeT(Type, Vec<ArrayAccessType>, Pos),
    ArrayStructTypeT(Symbol, Vec<ArrayAccessType>, Pos),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StructTypeT {
    StructTypeT(Symbol, Pos)
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrimitiveAndStructAndArraytype {
    PrimitiveType(Type, Pos),
    StructType(StructTypeT, Pos),
    ArrayType(ArrayType, Pos)
}

#[derive(Clone, Debug, PartialEq)]
pub enum InitializerList {
    AggregateAssign(Vec<SimpleDataExpr>, Pos)
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructDef {
    Struct(Symbol, Vec<(PrimitiveAndStructAndArraytype, Symbol, Pos)>, Pos)
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
    // XXX: This symbol has to be a signal
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

impl IO {
    pub fn is_output(&self) -> bool {
        match self {
            IO::Input => false,
            IO::Output => true,
        }
    }

    pub fn is_input(&self) -> bool {
        match self {
            IO::Input => true,
            IO::Output => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ASQual {
    Weak,
    Immediate,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    VInt(i64),
    VFloat(f64),
    InitList(InitializerList),
}

impl Val {
    pub fn to_string(&self) -> String {
        match self {
            Val::VFloat(x) => x.to_string(),
            Val::VInt(x) => x.to_string(),
	    Val::InitList(_) => todo!()
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(Vec<Stmt>, Pos),
    Pause(Symbol, Pos),
    Emit(Symbol, Option<SimpleDataExpr>, Pos),
    Present(Expr, Box<Stmt>, Option<Box<Stmt>>, Pos),
    Signal(Symbol, Option<IO>, Pos),
    DataSignal(Symbol, Option<IO>, Type, Val, ExprOp, Pos),
    Variable(Symbol, Type, Val, Pos),
    Abort(Expr, Option<ASQual>, Box<Stmt>, Pos),
    Suspend(Expr, Option<ASQual>, Box<Stmt>, Pos),
    Loop(Box<Stmt>, Pos),
    Assign(Symbol, SimpleDataExpr, Pos),
    Spar(Vec<Stmt>, Pos),
    Noop(Pos),
    // The next 2 are definition of a new struct and array variable
    StructDecl(Symbol, StructTypeT, InitializerList, Pos),
    ArrayDecl(Symbol, ArrayType, InitializerList, Pos),
    // The next 2 are assigning to structure and member of struct of
    // some defined variable.
    // StructAssign(Symbol, SimpleDataExpr, Pos),
    StructMemberAssign(StructRefT, SimpleDataExpr, Pos),
    // The next 2 are array assign or assigning to index/slice of an
    // already defined array variable.
    // ArrayAssign(Symbol, SimpleDataExpr, Pos),
    ArrayIndexAssign(ArrayRefT, SimpleDataExpr, Pos),
    // This is definition of a new struct type
    StructDef(StructDef),
}

impl Symbol {
    pub fn get_string(&self) -> &String {
        match &self {
            Symbol::Symbol(_s, _) => _s,
        }
    }
}

impl Expr {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>) -> RcDoc {
        match self {
            Expr::True(_pos) => RcDoc::as_string("true"),
            Expr::False(_pos) => RcDoc::as_string("false"),
            Expr::And(_l, _r, _pos) => {
                let _lm = RcDoc::as_string("(")
                    .append(_l.codegen(_tid, _smpt))
                    .append(RcDoc::as_string(")"));
                let _rm = RcDoc::as_string("(")
                    .append(_r.codegen(_tid, _smpt))
                    .append(RcDoc::as_string(")"));
                RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" and ").append(_rm)))
                    .append(RcDoc::as_string(")"))
            }
            Expr::Or(_l, _r, _pos) => {
                let _lm = RcDoc::as_string("(")
                    .append(_l.codegen(_tid, _smpt))
                    .append(RcDoc::as_string(")"));
                let _rm = RcDoc::as_string("(")
                    .append(_r.codegen(_tid, _smpt))
                    .append(RcDoc::as_string(")"));
                RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" or ").append(_rm)))
                    .append(RcDoc::as_string(")"))
            }
            Expr::Brackets(_e, _pos) => RcDoc::as_string("(")
                .append(_e.codegen(_tid, _smpt))
                .append(RcDoc::as_string(")")),
            Expr::Not(_e, _pos) => RcDoc::as_string("(not ")
                .append(_e.codegen(_tid, _smpt))
                .append(RcDoc::as_string(")")),
            Expr::Esymbol(_sy, _pos) => {
                let _s = format!("{}_prev.status", _sy.get_string());
                RcDoc::as_string(_s)
            }
            Expr::DataExpr(_rexpr, _) => _rexpr.codegen(_tid, _smpt),
        }
    }
}

impl RelDataExpr {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>) -> RcDoc {
        match self {
            RelDataExpr::EqualTo(_l, _r, _) => {
                let _lm = _l.codegen(_tid, _smpt);
                let _rm = _r.codegen(_tid, _smpt);
                RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" == ").append(_rm)))
                    .append(RcDoc::as_string(")"))
            }
            RelDataExpr::GreaterThan(_l, _r, _) => {
                let _lm = _l.codegen(_tid, _smpt);
                let _rm = _r.codegen(_tid, _smpt);
                RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" > ").append(_rm)))
                    .append(RcDoc::as_string(")"))
                // _lm.append(RcDoc::as_string(" > ").append(_rm))
            }
            RelDataExpr::GreaterThanEqual(_l, _r, _) => {
                let _lm = _l.codegen(_tid, _smpt);
                let _rm = _r.codegen(_tid, _smpt);
                RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" >= ").append(_rm)))
                    .append(RcDoc::as_string(")"))
                // _lm.append(RcDoc::as_string(" >= ").append(_rm))
            }
            RelDataExpr::LessThan(_l, _r, _) => {
                let _lm = _l.codegen(_tid, _smpt);
                let _rm = _r.codegen(_tid, _smpt);
                RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" < ").append(_rm)))
                    .append(RcDoc::as_string(")"))
                // _lm.append(RcDoc::as_string(" < ").append(_rm))
            }
            RelDataExpr::LessThanEqual(_l, _r, _) => {
                let _lm = _l.codegen(_tid, _smpt);
                let _rm = _r.codegen(_tid, _smpt);
                RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" <= ").append(_rm)))
                    .append(RcDoc::as_string(")"))
                // _lm.append(RcDoc::as_string(" <= ").append(_rm))
            }
        }
    }
}

impl SimpleDataExpr {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>) -> RcDoc {
        match self {
            SimpleDataExpr::ConstI(_i, _) => RcDoc::as_string(_i),
            SimpleDataExpr::ConstF(_i, _) => RcDoc::as_string(_i),
            SimpleDataExpr::VarRef(_sy, _) => {
                let _s = format!("{}_{}", _sy.get_string(), _tid);
                RcDoc::as_string(_s)
            }
            SimpleDataExpr::SimpleBinaryOp(_l, _op, _r, _) => {
                let _lm = _l.codegen(_tid, _smpt);
                let _rm = _r.codegen(_tid, _smpt);
                let _opm = _op.codegen();
                RcDoc::as_string("(")
                    .append(_lm.append(_opm).append(_rm))
                    .append(RcDoc::as_string(")"))
            }
            SimpleDataExpr::SignalRef(_sy, _) => {
                let _s = format!("{}_prev.value", _sy.get_string());
                RcDoc::as_string(_s)
            }
            SimpleDataExpr::Call(_sy, _v, _pos) => {
                let _s = RcDoc::<()>::as_string(_sy.get_string());
                let _vrs = _v.iter().map(|x| x.codegen(_tid, _smpt));
                let _as = RcDoc::intersperse(_vrs, RcDoc::as_string(", ")).group();
                _s.append("(").append(_as).append(")")
            }
	    SimpleDataExpr::AggregateAssign(_, _) => todo!(),
	    SimpleDataExpr::StructRef(_) => todo!(),
	    SimpleDataExpr::ArrayRef(_) => todo!(),
	    SimpleDataExpr::Cast(_, _, _) => todo!()
        }
    }
}

impl ExprOp {
    pub fn codegen(&self) -> RcDoc {
        match self {
            ExprOp::Div => RcDoc::as_string(" / "),
            ExprOp::Plus => RcDoc::as_string(" + "),
            ExprOp::Minus => RcDoc::as_string(" - "),
            ExprOp::Mul => RcDoc::as_string(" * "),
            ExprOp::Mod => RcDoc::as_string(" % "),
            ExprOp::Pow => todo!("Pow currently not supported"),
	    ExprOp::LShift => todo!(),
	    ExprOp::RShift => todo!()
        }
    }
}

fn _get_string_arg<'a>(_s: &'a str, _smpt: &'a HashMap<&str, usize>) ->
    Option<&'a usize> {
    if _smpt.contains_key(_s) {
        _smpt.get(_s)
    } else {
        None
    }
}

impl Stmt {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>) -> RcDoc {
        match self {
            Stmt::Emit(_sy, _sexpr, _pos) => {
                let _s = format!(
                    "_{}.status = true;",
                    _get_string_arg(_sy.get_string(), _smpt)
                        .expect("Could not find args for: {self}")
                );
                let _m = match _sexpr {
                    Some(__sexpr) => {
                        let __s = format!(
                            "_{}.value = ",
                            _get_string_arg(_sy.get_string(), _smpt)
                                .expect("Could not find args for: {self}")
                        );
                        RcDoc::as_string(__s)
                            .append(__sexpr.codegen(_tid, _smpt))
                            .append(RcDoc::as_string(";"))
                            .append(RcDoc::hardline())
                    }
                    None => RcDoc::nil(),
                };
                RcDoc::as_string(_s).append(RcDoc::hardline()).append(_m)
            }
            Stmt::Assign(_sy, _sexpr, _pos) => {
                let _s = format!("{}_{} = ", _sy.get_string(), _tid);
                let _m = _sexpr.codegen(_tid, _smpt);
                RcDoc::as_string(_s)
                    .append(_m)
                    .append(RcDoc::as_string(";"))
                    .append(RcDoc::hardline())
            }
            Stmt::Variable(_, _, _, _) => RcDoc::nil(),
            Stmt::Signal(_, _, _) => RcDoc::nil(),
            Stmt::DataSignal(_, _, _, _, _, _) => RcDoc::nil(),
            Stmt::Noop(_) => RcDoc::as_string(";"),
            _ => panic!("Can never reach to this statement in FSM graph: {:?}", self),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct CallNameType {
    pub _sy: String,
    pub _rtype: Type,
    pub _arg_types: Vec<Type>,
}

impl Type {
    pub fn _to_string(&self) -> &str {
        match self {
            Type::Int => "int",
            Type::Float => "float",
            Type::None => "void",
	    Type::Struct(_) => todo!(),
	    Type::Array(_, _) => todo!()
        }
    }
}

impl CallNameType {
    pub fn _get_doc(&self) -> RcDoc {
        let _ret = RcDoc::<()>::as_string(self._rtype._to_string());
        let _toret = RcDoc::<()>::as_string(self._sy.clone());
        let _args = join(self._arg_types.iter().map(|x| x._to_string()), ", ");
        let _args = RcDoc::<()>::as_string(format!("({});", _args));
        _ret.append(" ").append(_toret).append(_args)
    }
}

impl Type {
    fn _type_string(&self, _pos: &Pos, ff: &str) -> &str {
        match self {
            Type::Int => "int",
            Type::Float => "float",
            Type::None => {
                let _ = print_bytes(ff, _pos.0, _pos.1);
                panic!("Cannot write an empty type")
            },
	    Type::Array(_, _) => todo!(),
	    Type::Struct(_) => todo!()
        }
    }
}

impl Stmt {
    pub fn _input_rc_doc(&self, _ff: &str) -> (RcDoc, RcDoc) {
        match self {
            Stmt::Signal(_sy, _io, _pos) => {
                if let Some(IO::Input) = _io {
                    let _m = format!("typedef struct signal_{}", _sy.get_string());
                    let _m = format!(
                        "{} {{unsigned char status;}} signal_{};",
                        _m,
                        _sy.get_string()
                    );
                    let _a = RcDoc::<()>::as_string(_m).append(RcDoc::hardline());
                    let sname = _sy.get_string();
                    let u = format!("extern signal_{} {}_curr, {}_prev;", sname, sname, sname);
                    let u1 = format!("signal_{} {}_curr, {}_prev;", sname, sname, sname);
                    (
                        _a.append(RcDoc::as_string(u)).append(RcDoc::hardline()),
                        RcDoc::as_string(u1),
                    )
                } else {
                    (RcDoc::nil(), RcDoc::nil())
                }
            }
            Stmt::DataSignal(_sy, _io, _ty, _iv, _op, _pos) => {
                if let Some(IO::Input) = _io {
                    let _m = format!("typedef struct signal_{}", _sy.get_string());
                    let _m = format!(
                        "{} {{{} value; unsigned char status;}} signal_{};",
                        _m,
                        _ty._type_string(_pos, _ff),
                        _sy.get_string()
                    );
                    let a = RcDoc::<()>::as_string(_m).append(RcDoc::hardline());
                    let sname = _sy.get_string();
                    let u = format!("extern signal_{} {}_curr, {}_prev;", sname, sname, sname);
                    let u1 = format!("signal_{} {}_curr, {}_prev;", sname, sname, sname);
                    (a.append(u).append(RcDoc::hardline()), RcDoc::as_string(u1))
                } else {
                    (RcDoc::nil(), RcDoc::nil())
                }
            }
            _ => panic!("Got a non signal when generating input signal type"),
        }
    }
}
