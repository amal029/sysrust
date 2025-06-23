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
	    Self::ArrayAccessSymbol(_sy, _) => {
		// print!("array access symbol?");
		format!("{}_{_tid}", _sy.get_string())
	    }
	}
    }
}


// ArrayType
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ArrayTypeT {
    ArrayPrimTypeT(Type, Vec<ArrayAccessType>, Pos),
    ArrayStructTypeT(Symbol, Vec<ArrayAccessType>, Pos),
}

// This is a private function
fn _type_string<'a>(_ty: &'a Type, _pos: (usize, usize), ff: &'a str,
		    _tid: usize) -> (String, Option<String>) {
    match _ty {
        Type::Int => (String::from("int"), None),
        Type::Float => (String::from("float"), None),
        Type::None => {
            let _ = print_bytes(ff, _pos.0, _pos.1);
            panic!("Cannot write an empty type")
        }
	Type::Struct(_s) => {
	    match _s {
		StructTypeT::StructTypeT(_sy, _pos) =>
		    (format!("struct {}", _sy.get_string()), None)
	    }
	}
	Type::Array(_s) => {
	    match &**_s {
		ArrayTypeT::ArrayPrimTypeT(_ty, _vec, _) => {
		    let (_tys, _su) = _type_string(&_ty, _pos, ff, _tid);
		    match _su {
			Some (_) => {
			    let _ = print_bytes(ff, _pos.0, _pos.1);
			    panic!("Cannot write an empty type")
			}
			None => (),
		    };
		    let _vecs = _vec.iter().map(|x|
						format!("[{}]",
							x._type_string(_tid)));
		    let _vecs = _vecs.fold(String::from(""), |acc, x| acc +
					   x.as_str());
		    (_tys, Some(_vecs))
		}
		ArrayTypeT::ArrayStructTypeT(_sy, _vec, _) => {
		    let _tys = format!("struct {}", _sy.get_string());
		    let _vecs = _vec.iter().map(|x|
						format!("[{}]",
							x._type_string(_tid)));
		    let _vecs = _vecs.fold(String::from(""), |acc, x| acc +
					   x.as_str());
		    (_tys, Some(_vecs))
		}
	    }
	}
    }
}

impl ArrayTypeT {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>,
		   _ff: &str, _sym: &RcDoc) -> RcDoc {
	let (_1, _2) =
	    match self {
		Self::ArrayPrimTypeT(_ty, _vec, _pos) => {
		    let (_tys, _su) = _type_string(_ty, *_pos, _ff, _tid);
		    match _su {
			Some (_) => {
			    let _ = print_bytes(_ff, _pos.0, _pos.1);
			    panic!("Cannot write an empty type")
			}
			None => (),
		    };
		    let _vecs = _vec.iter().map(|x|
						format!("[{}]",
							x._type_string(_tid)));
		    let _vecs = _vecs.fold(String::from(""), |acc, x| acc +
					   x.as_str());
		    (_tys, Some(_vecs))
		}
		Self::ArrayStructTypeT(_sy, _vec, _) => {
		    let _tys = format!("struct {}", _sy.get_string());
		    let _vecs = _vec.iter().map(|x|
						format!("[{}]",
							x._type_string(_tid)));
		    let _vecs = _vecs.fold(String::from(""), |acc, x| acc +
					   x.as_str());
		    (_tys, Some(_vecs))
		}
	    };
	let _m = format!(
	    "{} {} {}",
	    _1,
	    _sym.pretty(10),
	    (match _2 {Some(x) => x, None => String::from("")}));
	RcDoc::<()>::as_string(_m).append(RcDoc::hardline())
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
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>,
		   _ff: &str, _sym: &RcDoc) -> RcDoc {
	match self {
	    Self::PrimitiveType(_type, _) => _type.codegen(_tid, _smpt, _ff),
	    Self::StructType(_st, _) => _st.codegen(_tid, _smpt),
	    Self::ArrayType(_at, _) => _at.codegen(_tid, _smpt, _ff, _sym)
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
    pub fn codegen(&self, _ff:&str) -> RcDoc {
	// XXX: There should be no need for the _tid and _smpt here.
	let _tid = 0;
	let _smpt = &HashMap::new();
	match self
	{
	    Self::Struct(_sy, _vec, _pos) =>
	    {
		let _syd = RcDoc::<()>::as_string(_sy.get_string());
		let _vecd =
		    _vec.iter()
		    .map(|(x, y, _)|
			 {
			     let _ys = RcDoc::<()>::as_string(y.get_string());
			     let _xs = x.codegen(_tid, _smpt, _ff, &_ys);
			     match x {
				 PrimitiveAndStructAndArraytype::ArrayType(_, _) =>
				     _xs // don't put the symbol in this case
				     .append(RcDoc::as_string(";"))
				     .append(RcDoc::hardline()),
				 _ =>
				     _xs.append(RcDoc::space()).append(_ys)
				     .append(RcDoc::as_string(";"))
				     .append(RcDoc::hardline())
			     }
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
    pub fn get_field(&self, _field: &Symbol) ->
	&(PrimitiveAndStructAndArraytype, Symbol, (usize, usize)){
	    match self {
		Self::Struct(_, _vec, _) => {
		    let _res = _vec.iter().find(|(_, _x, _)| {
			_x.get_string() == _field.get_string()
		    });
		    if _res.is_none() {
			panic!(
			    "Struct: {:?} not found in {:?} during type inference",
			    _field, self);
		    }
		    _res.unwrap()
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
    pub fn to_string(&self, _tid:usize, _ptids:&[i64],
		     _vars: &[Vec<Stmt>], _ff: &str) -> String {
        match self {
            Val::VFloat(x) => x.to_string(),
            Val::VInt(x) => x.to_string(),
	    Val::InitList(_il) => {
		// FIXME: This is a fake input -- should not be needed
		// in a correct program.
		let _smpt = HashMap::new();
		// _smpt = &HashMap<&str, usize>
		_il.codegen(_tid, &_smpt, _ptids, _vars, _ff).pretty(10).to_string()
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
    ExternDef(Vec<CallNameType>, Pos),
}

// XXX: This function gets the correct thread id for the declared
// variable. We are now allowing this, because we are compiling from
// timetide, where we do have variables declared in the parent threads
// and being used in the child thread.

fn get_correct_thread_id_for_var(_ptids: &[i64], _vars:&[Vec<Stmt>],
				 _sy: &str, _mtid: i64, _pos:&(usize, usize)) -> i64 {
    if _mtid == -1 {
	print!("\x1B[43mWarning\x1B[0m : \n");
	println!("Cannot find the used variable: {}", _sy);
	_mtid
    }
    else {
	// Does the _sy exist in the current thread?
	if _vars[_mtid as usize].iter().
	    find(|_x|
		 match _x {
		     Stmt::Variable(_sys, _, _, _) => _sy == _sys.get_string(),
		     _x =>
			 panic!("Unexpected Statement in \
				 getting parent thread id: {:?}", _x)
		 }
	    ).is_some() {
		_mtid
	    }
	else {
	    // We need to get the parent tid and check there
	    get_correct_thread_id_for_var(_ptids, _vars, _sy,
					  _ptids[_mtid as usize], _pos)
	}
    }
}

impl Symbol {
    pub fn get_string(&self) -> &String {
        match &self {
            Symbol::Symbol(_s, _) => _s,
        }
    }
}

impl Expr {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>,
		   _ptids: &[i64], _vars: &[Vec<Stmt>], _ff: &str) -> RcDoc {
        match self {
            Expr::True(_pos) => RcDoc::as_string("true"),
            Expr::False(_pos) => RcDoc::as_string("false"),
            Expr::And(_l, _r, _pos) => {
                let _lm = RcDoc::as_string("(")
                    .append(_l.codegen(_tid, _smpt, _ptids, _vars, _ff))
                    .append(RcDoc::as_string(")"));
                let _rm = RcDoc::as_string("(")
                    .append(_r.codegen(_tid, _smpt, _ptids, _vars, _ff))
                    .append(RcDoc::as_string(")"));
                RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" and ").append(_rm)))
                    .append(RcDoc::as_string(")"))
            }
            Expr::Or(_l, _r, _pos) => {
                let _lm = RcDoc::as_string("(")
                    .append(_l.codegen(_tid, _smpt, _ptids, _vars, _ff))
                    .append(RcDoc::as_string(")"));
                let _rm = RcDoc::as_string("(")
                    .append(_r.codegen(_tid, _smpt, _ptids, _vars, _ff))
                    .append(RcDoc::as_string(")"));
                RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" or ").append(_rm)))
                    .append(RcDoc::as_string(")"))
            }
            Expr::Brackets(_e, _pos) => RcDoc::as_string("(")
                .append(_e.codegen(_tid, _smpt, _ptids, _vars, _ff))
                .append(RcDoc::as_string(")")),
            Expr::Not(_e, _pos) => RcDoc::as_string("(not ")
                .append(_e.codegen(_tid, _smpt, _ptids, _vars, _ff))
                .append(RcDoc::as_string(")")),
            Expr::Esymbol(_sy, _pos) => {
                let _s = format!("{}_prev.status", _sy.get_string());
                RcDoc::as_string(_s)
            }
            Expr::DataExpr(_rexpr, _) =>
		_rexpr.codegen(_tid, _smpt, _ptids, _vars, _ff),
        }
    }
}

impl RelDataExpr {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>,
		   _ptids:&[i64], _vars: &[Vec<Stmt>], _ff: &str) -> RcDoc {
        match self {
            RelDataExpr::EqualTo(_l, _r, _) => {
                let _lm = _l.codegen(_tid, _smpt, _ptids, _vars, _ff);
                let _rm = _r.codegen(_tid, _smpt, _ptids, _vars, _ff);
                RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" == ").append(_rm)))
                    .append(RcDoc::as_string(")"))
            }
            RelDataExpr::GreaterThan(_l, _r, _) => {
                let _lm = _l.codegen(_tid, _smpt, _ptids, _vars, _ff);
                let _rm = _r.codegen(_tid, _smpt, _ptids, _vars, _ff);
                RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" > ").append(_rm)))
                    .append(RcDoc::as_string(")"))
                // _lm.append(RcDoc::as_string(" > ").append(_rm))
            }
            RelDataExpr::GreaterThanEqual(_l, _r, _) => {
                let _lm = _l.codegen(_tid, _smpt, _ptids, _vars, _ff);
                let _rm = _r.codegen(_tid, _smpt, _ptids, _vars, _ff);
                RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" >= ").append(_rm)))
                    .append(RcDoc::as_string(")"))
                // _lm.append(RcDoc::as_string(" >= ").append(_rm))
            }
            RelDataExpr::LessThan(_l, _r, _) => {
                let _lm = _l.codegen(_tid, _smpt, _ptids, _vars, _ff);
                let _rm = _r.codegen(_tid, _smpt, _ptids, _vars, _ff);
                RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" < ").append(_rm)))
                    .append(RcDoc::as_string(")"))
                // _lm.append(RcDoc::as_string(" < ").append(_rm))
            }
            RelDataExpr::LessThanEqual(_l, _r, _) => {
                let _lm = _l.codegen(_tid, _smpt, _ptids, _vars, _ff);
		let _rm = _r.codegen(_tid, _smpt, _ptids, _vars, _ff);
		RcDoc::as_string("(")
                    .append(_lm.append(RcDoc::as_string(" <= ").append(_rm)))
                    .append(RcDoc::as_string(")"))
		// _lm.append(RcDoc::as_string(" <= ").append(_rm))
            }
	}
    }
}

impl SimpleDataExpr {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>,
		   _ptids:&[i64], _vars: &[Vec<Stmt>], _ff: &str) -> RcDoc {
        match self {
            SimpleDataExpr::ConstI(_i, _) => RcDoc::as_string(_i),
            SimpleDataExpr::ConstF(_i, _) => RcDoc::as_string(_i),
            SimpleDataExpr::VarRef(_sy, _pos) => {
		let _rtid = get_correct_thread_id_for_var(_ptids, _vars,
							  &_sy.get_string(),
							  _tid as i64, _pos);
		let _s =
		    if _rtid == -1 {
			format!("{}_{}", _sy.get_string(), _tid)
		    } else {
			format!("{}_{}", _sy.get_string(), _rtid)
		    };
                RcDoc::as_string(_s)
            }
            SimpleDataExpr::SimpleBinaryOp(_l, _op, _r, _) => {
                let _lm = _l.codegen(_tid, _smpt, _ptids, _vars, _ff);
                let _rm = _r.codegen(_tid, _smpt, _ptids, _vars, _ff);
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
                let _vrs = _v.iter().map(|x|
					 x.codegen(_tid, _smpt, _ptids, _vars, _ff));
                let _as = RcDoc::intersperse(_vrs, RcDoc::as_string(", ")).group();
                _s.append("(").append(_as).append(")")
            }
	    SimpleDataExpr::AggregateAssign(_il, _pos) =>
		_il.codegen(_tid, _smpt, _ptids, _vars, _ff),
	    SimpleDataExpr::StructRef(_s) => _s.codegen(_tid, _smpt),
	    SimpleDataExpr::ArrayRef(_s) =>
		_s.codegen(_tid, _smpt, _ptids, _vars, _ff),
	    SimpleDataExpr::Cast(_t, _sde, _pos) => {
		let _td = _t.codegen(_tid, _smpt, _ff);
		let _sded = _sde.codegen(_tid, _smpt, _ptids, _vars, _ff);
		RcDoc::as_string("(")
		    .append(_td)
		    .append(")")
		    .append(_sded)
	    }
        }
    }
}

impl ArrayRefT {
    pub fn codegen(&self, _tid:usize, _smpt: & HashMap<&str, usize>,
		   _ptids:&[i64], _vars : &[Vec<Stmt>], _ff: &str) -> RcDoc {
	match self {
	    ArrayRefT::ArrayRef(_s1, _s2, _pos) => {
		let _s2 = _s2.iter().map(|x|
					 {RcDoc::as_string("[").append(
					     x.codegen(_tid, _smpt, _ptids,
						       _vars, _ff)).append(
					     RcDoc::as_string("]"))}).collect_vec();
		let _s2 = _s2.iter().fold(RcDoc::nil(),
					  |acc, x| acc.append(x.to_owned()));
		let _rtid = get_correct_thread_id_for_var(_ptids, _vars,
							  &_s1.get_string(),
							  _tid as i64, _pos);
		if _rtid == -1 {
		    RcDoc::as_string(format!("{}_{}",
					     _s1.get_string(), _tid)).append(_s2)
		} else {
		    RcDoc::as_string(format!("{}_{}",
					     _s1.get_string(), _rtid)).append(_s2)
		}
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
    pub fn get_string(&self) -> &String {
	match self {
	    StructRefT::StructRef(_s1, _s2, _pos) => {
		_s1.get_string()
	    }
	}
    }
    pub fn get_field(&self) -> &Symbol {
	match self {
	    StructRefT::StructRef(_s1, _s2, _pos) => {
		_s2
	    }
	}
    }

}


impl InitializerList {
    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>,
		   _ptids:&[i64], _vars: &[Vec<Stmt>], _ff: &str) -> RcDoc {
	match self {
	    InitializerList::AggregateAssign(_il, _pos) => {
		let _kk : Vec<_> =
		    _il.iter().map(|x| x.codegen(_tid, _smpt, _ptids,
						 _vars, _ff)).collect();
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


fn make_array_var_decl<'a>(_sy: &'a String, _tid: usize, _len:usize,
			   _iv:&'a Val, _pos:&'a (usize, usize),
			   _smpt: &HashMap<&str, usize>,
			   _ptids:&[i64], _vars:&[Vec<Stmt>], _ff: &'a str)
			   -> RcDoc<'a> {
    let mut toret = RcDoc::nil();
    let _sed =
	match _iv {
	    Val::InitList(InitializerList::AggregateAssign(_se, _pos)) => {
		_se.iter().map(|x| x.codegen(_tid, _smpt,
					     _ptids, _vars,
					     _ff)).collect_vec()
	    }
	    _ =>
		panic!("Cannot initialize the varaible {}, {}",
		       _pos.0, _pos.1)
	};
    assert!(1 == _len, "Currenlty only 1D arrays supported");
    for i in 0.._sed.len() {
	let arr = format!("{}_{}[{}] = {};", _sy, _tid,
			  i, _sed[i].pretty(10).to_string());
	toret = toret.append(arr).append(RcDoc::hardline());
    }
    toret
}

impl Stmt {
    pub fn codegen<'a>(&'a self, _tid: usize, _smpt: & HashMap<&str, usize>,
		   _ptids: &[i64], _vars: &[Vec<Stmt>],
		   _ff: &'a str) -> RcDoc {
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
                            .append(__sexpr.codegen(_tid, _smpt, _ptids, _vars, _ff))
                            .append(RcDoc::as_string(";"))
                            .append(RcDoc::hardline())
                    }
                    None => RcDoc::nil(),
                };
                RcDoc::as_string(_s).append(RcDoc::hardline()).append(_m)
            }
            Stmt::Assign(_sy, _sexpr, _pos) => {
		// XXX: Looking up the parent thread id where the
		// varaible might have been declared.
		let _rtid = get_correct_thread_id_for_var(_ptids, _vars,
							  &_sy.get_string(),
							  _tid as i64, _pos);
                let _s =
		    if _rtid == -1 {
			format!("{}_{} = ", _sy.get_string(), _tid)
		    } else {
			format!("{}_{} = ", _sy.get_string(), _rtid)
		    };
                let _m = _sexpr.codegen(_tid, _smpt, _ptids, _vars, _ff);
                RcDoc::as_string(_s)
                    .append(_m)
                    .append(RcDoc::as_string(";"))
                    .append(RcDoc::hardline())
            }
	    // XXX: We update this to assign the output to the variable
	    // here.
            Stmt::Variable(_sy, _ty, _iv, _pos) => {
		match _ty {
		    Type::Array(_at) => {
			match &**_at {
			    ArrayTypeT::ArrayPrimTypeT(_tt, _vec, _) =>
				make_array_var_decl(_sy.get_string(),
						    _tid, _vec.len(), _iv,
						    _pos, _smpt, _ptids, _vars, _ff),
			    ArrayTypeT::ArrayStructTypeT(_tt, _vec, _) =>
				make_array_var_decl(_sy.get_string(),
						    _tid, _vec.len(), _iv,
						    _pos, _smpt, _ptids, _vars, _ff),

			}
		    }
		    _ => {
			// print!("Variable codegen");
			let _m = format!(
			    "{}_{} = {};",
			    _sy.get_string(),
			    _tid,
			    _iv.to_string(_tid, _ptids, _vars, _ff),
			);
			RcDoc::<()>::as_string(_m).append(RcDoc::hardline())
		    }
		}
	    }
            Stmt::Signal(_, _, _) => RcDoc::nil(),
            Stmt::DataSignal(_, _, _, _, _, _) => RcDoc::nil(),
            Stmt::Noop(_) => RcDoc::as_string(";"),
	    Stmt::StructMemberAssign(_sy, _m, _) => {
		let _sy = _sy.codegen(_tid, _smpt);
                let _m = _m.codegen(_tid, _smpt, _ptids, _vars, _ff);
                _sy
                    .append(RcDoc::as_string(" = "))
                    .append(_m)
                    .append(RcDoc::as_string(";"))
                    .append(RcDoc::hardline())
	    },
	    Stmt::ArrayIndexAssign(_at, _sexpr, _) => {
		// print!("Array assign codegen");
		let _m = _sexpr.codegen(_tid , _smpt, _ptids, _vars, _ff);
		let _at = _at.codegen(_tid, _smpt, _ptids, _vars, _ff);
		_at.append(RcDoc::as_string(" = "))
		    .append(_m).append(";").append(RcDoc::hardline())
	    }
	    Stmt::StructDef(_sd) => RcDoc::nil(), // _sd.codegen(_tid, _smpt),
	    Stmt::ExternDef(_, _) => RcDoc::nil(),
            _ => panic!("Can never reach to this statement in FSM graph: {:?}", self),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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

    pub fn codegen(&self, _tid: usize, _smpt: &HashMap<&str, usize>,
		   _ff: &str) -> RcDoc {
	match self {
	    Type::Float => RcDoc::as_string(self._to_string()),
	    Type::Int => RcDoc::as_string(self._to_string()),
	    Type::None => RcDoc::as_string(self._to_string()),
	    Type::Struct(_ss) => _ss.codegen(_tid, _smpt),
	    Type::Array(_ss) => _ss.codegen(_tid, _smpt, _ff, &RcDoc::nil())
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
    pub fn _input_output_rc_doc(&self, _ff: &str) -> (RcDoc, RcDoc) {
        match self {
            Stmt::Signal(_sy, _io, _pos) => {
		if let Some(IO::Output) = _io {
                    let _m = format!("typedef struct signal_{}", _sy.get_string());
                    let _m = format!(
                        "{} {{bool status;\n \
			 bool tag;}} signal_{};",
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
                }
                else if let Some(IO::Input) = _io {
                    let _m = format!("typedef struct signal_{}", _sy.get_string());
                    let _m = format!(
                        "{} {{bool status;\n \
			 bool tag;}} signal_{};",
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
		if let Some(IO::Output) = _io {
                    let _m = format!("typedef struct signal_{}", _sy.get_string());
                    let _m = format!(
                        "{} {{{} value; bool status;\n\
			 bool tag;}} signal_{};",
                        _m,
                        _ty._type_string(_pos, _ff),
                        _sy.get_string());
                    let a = RcDoc::<()>::as_string(_m).append(RcDoc::hardline());
                    let sname = _sy.get_string();
                    let u = format!("extern signal_{} {}_curr, {}_prev;",
				    sname, sname, sname);
                    let u1 = format!("signal_{} {}_curr, {}_prev;",
				     sname, sname, sname);
                    (a.append(u).append(RcDoc::hardline()), RcDoc::as_string(u1))
                }
                else if let Some(IO::Input) = _io {
                    let _m = format!("typedef struct signal_{}", _sy.get_string());
                    let _m = format!(
                        "{} {{{} value; bool status;\n\
			 bool tag;}} signal_{};",
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
