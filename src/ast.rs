use std::collections::HashMap;

use pretty::RcDoc;

type Pos = (usize, usize);

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Symbol {
    Symbol(String, Pos),
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Type {
    Int,
    Float,
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
}

#[derive(Clone, Debug, PartialEq)]
pub enum SimpleDataExpr {
    SimpleBinaryOp(Box<SimpleDataExpr>, ExprOp, Box<SimpleDataExpr>, Pos),
    VarRef(Symbol, Pos),
    SignalRef(Symbol, Pos),
    ConstI(i64, Pos),
    ConstF(f64, Pos),
    Call(Symbol, Vec<SimpleDataExpr>, Pos),
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

#[derive(Clone, Debug, PartialEq)]
pub enum ASQual {
    Weak,
    Immediate,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    VInt(i64),
    VFloat(f64),
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
        }
    }
}

fn _get_string_arg<'a>(_s: &'a String, _smpt: &'a HashMap<&str, usize>) -> Option<&'a usize> {
    if _smpt.contains_key(_s.as_str()) {
        _smpt.get(_s.as_str())
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
        }
    }
}

impl CallNameType {
    pub fn _get_doc(&self) -> RcDoc {
        let _ret = RcDoc::<()>::as_string(self._rtype._to_string());
        let _toret = RcDoc::<()>::as_string(self._sy.clone());
        let _args = self
            ._arg_types
            .iter()
            .map(|x| x._to_string())
            .collect::<Vec<_>>()
            .join(", ");
        let _args = RcDoc::<()>::as_string(format!("({});", _args));
        _ret.append(" ").append(_toret).append(_args)
    }
}
