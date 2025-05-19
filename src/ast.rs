use std::collections::HashMap;

use crate::error::print_bytes;
use itertools::{join, Itertools};
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
    Array(Box<ArrayTypeT>),
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

impl ArrayAccessType {
    pub fn _type_string (&self, _tid:usize) -> String {
	match self {
	    Self::ArrayAccessInt(_i, _pos) => _i.to_string(),
	    Self::ArrayAccessSymbol(_sy, _) => format!("{}_{_tid}", _sy.get_string())
	}
    }
}


// ArrayType
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ArrayTypeT {
    ArrayPrimTypeT(Type, Vec<ArrayAccessType>, Pos),
    ArrayStructTypeT(Symbol, Vec<ArrayAccessType>, Pos),
}

impl ArrayTypeT {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>) -> RcDoc {
	match self {
	    Self::ArrayPrimTypeT(_ty, _vec, _) => todo!(),
	    Self::ArrayStructTypeT(_sy, _vec, _) => todo!(),
	}
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StructTypeT {
    StructTypeT(Symbol, Pos)
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrimitiveAndStructAndArraytype {
    PrimitiveType(Type, Pos),
    StructType(StructTypeT, Pos),
    ArrayType(ArrayTypeT, Pos)
}

impl PrimitiveAndStructAndArraytype {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>) -> RcDoc {
	match self {
	    Self::PrimitiveType(_type, _) => _type.codegen(_tid, _smpt),
	    Self::StructType(_st, _) => _st.codegen(_tid, _smpt),
	    Self::ArrayType(_at, _) => _at.codegen(_tid, _smpt)
	}
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum InitializerList {
    AggregateAssign(Vec<SimpleDataExpr>, Pos)
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructDef {
    Struct(Symbol, Vec<(PrimitiveAndStructAndArraytype, Symbol, Pos)>, Pos)
}

impl StructDef {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>) -> RcDoc {
	match self {
	    Self::Struct(_sy, _vec, _pos) => {
		let _syd = RcDoc::<()>::as_string(_sy.get_string());
		let _vecd =
		    _vec.iter().map(|(x, y, _)|
				    {
					let _xs = x.codegen(_tid, _smpt);
					let _ys = RcDoc::<()>::as_string(
					    y.get_string());
					_xs.append(RcDoc::space()).append(_ys)
					    .append(RcDoc::as_string(";"))
					    .append(RcDoc::hardline())
				    }).collect_vec();
		let _vecd = RcDoc::concat(_vecd);
		RcDoc::as_string("struct ")
		    .append(_syd)
		    .append(RcDoc::as_string("{"))
		    .append(_vecd)
		    .append(RcDoc::as_string("};"))
		    .append(RcDoc::hardline())
	    }
	}
    }
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
    pub fn to_string(&self, _tid:usize) -> String {
        match self {
            Val::VFloat(x) => x.to_string(),
            Val::VInt(x) => x.to_string(),
	    Val::InitList(_il) => {
		// FIXME: This is a fake input -- should not be needed
		// in a correct program.
		let _smpt = HashMap::new();
		// _smpt = &HashMap<&str, usize>
		_il.codegen(_tid, &_smpt).pretty(10).to_string()
	    }
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
    StructMemberAssign(StructRefT, SimpleDataExpr, Pos),
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
	    SimpleDataExpr::AggregateAssign(_il, _pos) => _il.codegen(_tid, _smpt),
	    SimpleDataExpr::StructRef(_s) => _s.codegen(_tid, _smpt),
	    SimpleDataExpr::ArrayRef(_s) => _s.codegen(_tid, _smpt),
	    SimpleDataExpr::Cast(_t, _sde, _pos) => {
		let _td = _t.codegen(_tid, _smpt);
		let _sded = _sde.codegen(_tid, _smpt);
		RcDoc::as_string("(")
		    .append(_td)
		    .append(")")
		    .append(_sded)
	    }
        }
    }
}

impl ArrayRefT {
    pub fn codegen(&self, _tid:usize, _smpt: & HashMap<&str, usize>) -> RcDoc {
	match self {
	    ArrayRefT::ArrayRef(_s1, _s2, _pos) => {
		let _s2 = _s2.iter().map(|x|
					 {RcDoc::as_string("[").append(
					     x.codegen(_tid, _smpt)).append(
					     RcDoc::as_string("]"))}).collect_vec();
		let _s2 = _s2.iter().fold(RcDoc::nil(),
					  |acc, x| acc.append(x.to_owned()));
		RcDoc::as_string(format!("{}_{}", _s1.get_string(), _tid)).append(_s2)
	    }		
	}
    }
}


impl StructRefT {
    pub fn codegen(&self, _tid:usize, _smpt: & HashMap<&str, usize>) -> RcDoc {
	match self {
	    StructRefT::StructRef(_s1, _s2, _pos) => {
		let _s1 = format!("{}_{}", _s1.get_string(), _tid);
		let _s2 = _s2.get_string();
		let ss = format!("{_s1}.{_s2}");
		RcDoc::<()>::as_string(ss)
	    }		
	}
    }
}


impl InitializerList {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>) -> RcDoc {
	match self {
	    InitializerList::AggregateAssign(_il, _pos) => {
		let _kk : Vec<_> =
		    _il.iter().map(|x| x.codegen(_tid, _smpt)).collect();
		let _kk = RcDoc::intersperse(_kk, RcDoc::as_string(", ")).group();
		RcDoc::as_string("{").append(_kk).append("}")
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
	    ExprOp::LShift => RcDoc::as_string("<<"),
	    ExprOp::RShift => RcDoc::as_string(">>")
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
	    Stmt::StructMemberAssign(_sy, _m, _) => {
		let _sy = _sy.codegen(_tid, _smpt);
                let _m = _m.codegen(_tid, _smpt);
                _sy
                    .append(RcDoc::as_string(" = "))
                    .append(_m)
                    .append(RcDoc::as_string(";"))
                    .append(RcDoc::hardline())
	    },
	    Stmt::ArrayIndexAssign(_at, _sexpr, _) => {
		let _m = _sexpr.codegen(_tid , _smpt);
		let _at = _at.codegen(_tid, _smpt);
		_at.append(RcDoc::as_string(" = "))
		    .append(_m).append(";").append(RcDoc::hardline())
	    }
	    Stmt::StructDef(_sd) => {
		_sd.codegen(_tid, _smpt).append(";").append(RcDoc::hardline())
	    }
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

impl StructTypeT {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>) -> RcDoc {
	match self {
	    StructTypeT::StructTypeT(_s, _pos) =>
		RcDoc::as_string("struct ").append(RcDoc::as_string(_s.get_string()))
	}
    }
}

impl Type {
    pub fn _to_string(&self) -> String {
        match self {
            Type::Int => String::from("int"),
            Type::Float => String::from("float"),
            Type::None => String::from("void"),
	    Type::Struct(_ss) => {
		match _ss {
		    StructTypeT::StructTypeT(_s, _) =>
			format!("struct {}", _s.get_string())
		}
	    }
	    Type::Array(_) => todo!()
        }
    }

    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>) -> RcDoc {
	match self {
	    Type::Float => RcDoc::as_string(self._to_string()),
	    Type::Int => RcDoc::as_string(self._to_string()),
	    Type::None => RcDoc::as_string(self._to_string()),
	    Type::Struct(_ss) => _ss.codegen(_tid, _smpt),
	    Type::Array(_ss) => _ss.codegen(_tid, _smpt)
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
    fn _type_string(&self, _pos: &Pos, ff: &str) -> String {
        match self {
            Type::Int => String::from("int"),
            Type::Float => String::from("float"),
            Type::None => {
                let _ = print_bytes(ff, _pos.0, _pos.1);
                panic!("Cannot write an empty type")
            },
	    Type::Struct(_ss) => {
		match _ss {
		    StructTypeT::StructTypeT(_s, _) =>
			format!("struct {}", _s.get_string())
		}
	    }
	    Type::Array(_s) => todo!()
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
                    let u = format!("extern signal_{} {}_curr, {}_prev;",
				    sname, sname, sname);
                    let u1 = format!("signal_{} {}_curr, {}_prev;", sname,
				     sname, sname);
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
                    let u = format!("extern signal_{} {}_curr, {}_prev;",
				    sname, sname, sname);
                    let u1 = format!("signal_{} {}_curr, {}_prev;",
				     sname, sname, sname);
                    (a.append(u).append(RcDoc::hardline()), RcDoc::as_string(u1))
                } else {
                    (RcDoc::nil(), RcDoc::nil())
                }
            }
            _ => panic!("Got a non signal when generating input signal type"),
        }
    }
}
