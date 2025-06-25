use std::{collections::HashMap, process::exit};

use sysrust::ast::*;

use crate::error::print_bytes;
// use crate::error::print_bytes_warn;

// pub enum SignalVarType {
//     Signal,
//     Var(usize),
// }

// enum ValuedSignalDataCheck {
//     Check,
//     NoCheck,
// }

// type HT = HashMap<String, (Type, SignalVarType, Option<IO>)>;
type Pos = (usize, usize);
// type Pos1 = (usize, usize, String);

// pub fn _analyse_var_signal_uses(
//     ff: &str,
//     _stmts: &[Stmt],
//     stack: Vec<HT>,
//     rets: &mut Vec<Pos1>,
//     tid: usize,
// ) -> Vec<HT> {
//     let mut u = stack;
//     for i in _stmts.iter() {
//         u = _analyse_var_signal_use(ff, i, u, rets, tid);
//     }
//     u
// }

// fn _check_sym_in_map(
//     _ff: &str,
//     s: &Symbol,
//     vmap: &[HT],
//     pos: Pos,
//     rets: &mut Vec<Pos1>,
//     _svt: SignalVarType,
//     _dvt: ValuedSignalDataCheck,
//     _io: Option<IO>,
// ) {
//     fn match_signal_var_type(_svt1: &SignalVarType, _svt2: &SignalVarType) -> bool {
//         match (_svt1, _svt2) {
//             (SignalVarType::Signal, SignalVarType::Signal) => true,
//             (SignalVarType::Var(x), SignalVarType::Var(y)) if (x == y) => true,
//             (_, _) => false,
//         }
//     }

//     // XXX: Check of the given _io matches with declared signal type
//     fn match_signal_io_type(_io1: &Option<IO>, _io2: &Option<IO>) -> bool {
//         match (_io1, _io2) {
//             (Some(IO::Output), Some(IO::Output)) => true,
//             (Some(IO::Input), Some(IO::Input)) => true,
//             (Some(IO::Output), Some(IO::Input)) => false,
//             (Some(IO::Input), Some(IO::Output)) => false,
//             _ => true,
//         }
//     }

//     let mut there = false;
//     let mut _dcheck = false;
//     for map in vmap.iter() {
//         if map.contains_key(s.get_string()) {
//             let _svt2 = &map.get(s.get_string()).unwrap().1;
//             let _svt3 = &map.get(s.get_string()).unwrap().2;
//             if match_signal_var_type(&_svt, _svt2) &&
// 		match_signal_io_type(&_io, _svt3) {
//                     // XXX: Check if the signal ref being used is a valued signal.
//                     _dcheck = match _dvt {
// 			ValuedSignalDataCheck::Check => {
//                             let _dvt2 = &map.get(s.get_string()).unwrap().0;
//                             match _dvt2 {
// 				Type::Float | Type::Int => true,
// 				Type::Struct(_) => true,
// 				| Type::Array(_) => true,
// 				Type::None => false,
//                             }
// 			}
// 			ValuedSignalDataCheck::NoCheck => true,
//                     };
//                     there = true;
//                     break;
// 		}
//         }
//     }
//     if !there {
//         let ss = format!(
//             "Signal/Variable: {} either not declared, \
// 	     or input signal(s) are being emitted.\
// 	     \nSignal and variable declarations \
// 	     can shadow each other.\nVariables cannot be \
// 	     shared between threads.",
//             s.get_string()
//         );
//         rets.push((pos.0, pos.1, ss));
//         return;
//     }
//     if !_dcheck {
//         let ss = format!("Signal {} is not a valued signal.", s.get_string());
//         rets.push((pos.0, pos.1, ss));
//     }
// }

// fn _check_sym_in_simple_expr(
//     _ff: &str,
//     _expr: &SimpleDataExpr,
//     _vmap: &[HT],
//     _pos: Pos,
//     rets: &mut Vec<Pos1>,
//     tid: usize,
// ) {
//     match _expr {
//         SimpleDataExpr::SimpleBinaryOp(_l, _, _r, _) => {
//             _check_sym_in_simple_expr(_ff, _l, _vmap, _pos, rets, tid);
//             _check_sym_in_simple_expr(_ff, _r, _vmap, _pos, rets, tid);
//         }
//         SimpleDataExpr::VarRef(_sy, _) => _check_sym_in_map(
//             _ff,
//             _sy,
//             _vmap,
//             _pos,
//             rets,
//             SignalVarType::Var(tid),
//             ValuedSignalDataCheck::NoCheck,
//             None,
//         ),
//         SimpleDataExpr::SignalRef(_sy, _) => _check_sym_in_map(
//             _ff,
//             _sy,
//             _vmap,
//             _pos,
//             rets,
//             SignalVarType::Signal,
//             ValuedSignalDataCheck::Check,
//             None,
//         ),
//         SimpleDataExpr::Call(_sy, _vexp, _) => _vexp
//             .iter()
//             .for_each(|x| _check_sym_in_simple_expr(_ff, x, _vmap, _pos, rets, tid)),
//         SimpleDataExpr::ConstI(_, _) | SimpleDataExpr::ConstF(_, _) => (),
// 	// XXX: Just by passing because all checking is happening in Ocaml
// 	SimpleDataExpr::AggregateAssign(_, _) =>(),
// 	SimpleDataExpr::StructRef(_stref) => (),
// 	SimpleDataExpr::ArrayRef(_) => (),
// 	SimpleDataExpr::Cast(_, _sde, _pos) => {
// 	    _check_sym_in_simple_expr(_ff, &_sde, _vmap, *_pos, rets, tid)
// 	}
//     }
// }

// fn _check_sym_in_rel_expr(
//     _ff: &str,
//     _expr: &RelDataExpr,
//     _vmap: &[HT],
//     _pos: Pos,
//     rets: &mut Vec<Pos1>,
//     tid: usize,
// ) {
//     match _expr {
//         RelDataExpr::LessThan(_l, _r, _)
//         | RelDataExpr::LessThanEqual(_l, _r, _)
//         | RelDataExpr::GreaterThan(_l, _r, _)
//         | RelDataExpr::GreaterThanEqual(_l, _r, _)
//         | RelDataExpr::EqualTo(_l, _r, _) => {
//             _check_sym_in_simple_expr(_ff, _l, _vmap, _pos, rets, tid);
//             _check_sym_in_simple_expr(_ff, _r, _vmap, _pos, rets, tid);
//         }
//     }
// }

// fn _check_sym_in_expr(
//     _ff: &str,
//     _expr: &Expr,
//     _vmap: &[HT],
//     _pos: Pos,
//     rets: &mut Vec<Pos1>,
//     tid: usize,
// ) {
//     match _expr {
//         Expr::True(_) | Expr::False(_) => (),
//         Expr::Esymbol(_sy, _) => _check_sym_in_map(
//             _ff,
//             _sy,
//             _vmap,
//             _pos,
//             rets,
//             SignalVarType::Signal,
//             ValuedSignalDataCheck::NoCheck,
//             None,
//         ),
//         Expr::And(_l, _r, _) | Expr::Or(_l, _r, _) => {
//             _check_sym_in_expr(_ff, _l, _vmap, _pos, rets, tid);
//             _check_sym_in_expr(_ff, _r, _vmap, _pos, rets, tid);
//         }
//         Expr::Not(_e, _) => _check_sym_in_expr(_ff, _e, _vmap, _pos, rets, tid),
//         Expr::Brackets(_e, _) => _check_sym_in_expr(_ff, _e, _vmap, _pos, rets, tid),
//         Expr::DataExpr(_re, _) =>
// 	    _check_sym_in_rel_expr(_ff, _re, _vmap, _pos, rets, tid),
//     }
// }

// pub fn _analyse_var_signal_use(
//     ff: &str,
//     stmt: &Stmt,
//     _stack: Vec<HT>,
//     rets: &mut Vec<Pos1>,
//     tid: usize,
// ) -> Vec<HT> {
//     fn _signal_rep(_vmap: &[HT], _sy: &Symbol, start: usize, end: usize, ff: &str) {
//         for m in _vmap.iter() {
//             if m.contains_key(_sy.get_string()) {
//                 let _ = print_bytes(ff, start, end);
//                 println!("Signal names have to be unique throughout program");
//                 exit(1);
//             }
//         }
//     }
//     match stmt {
//         Stmt::Block(_stmts, _pos) => {
//             // XXX: Push a new hashmap on the stack
//             let mut _stack = _stack;
//             let vmap: HT = HashMap::with_capacity(1000);
//             _stack.push(vmap);
//             _stack = _analyse_var_signal_uses(ff, _stmts, _stack, rets, tid);
//             _stack.pop();
//             _stack
//         }
//         Stmt::Pause(_, _) => _stack,
//         Stmt::Emit(_sy, _expr, _pos) => {
//             // XXX: Check if the _sy is in the hashmap
//             _check_sym_in_map(
//                 ff,
//                 _sy,
//                 &_stack,
//                 *_pos,
//                 rets,
//                 SignalVarType::Signal,
//                 ValuedSignalDataCheck::NoCheck,
//                 Some(IO::Output),
//             );
//             if let Some(__expr) = _expr {
//                 _check_sym_in_simple_expr(ff, __expr, &_stack, *_pos, rets, tid);
//             }
//             _stack
//         }
//         Stmt::Present(_sy, _st, None, _pos) => {
//             _check_sym_in_expr(ff, _sy, &_stack, *_pos, rets, tid);
//             _analyse_var_signal_use(ff, _st, _stack, rets, tid)
//         }
//         Stmt::Present(_sy, _st1, Some(_st), _pos) => {
//             _check_sym_in_expr(ff, _sy, &_stack, *_pos, rets, tid);
//             let ss = _analyse_var_signal_use(ff, _st1, _stack, rets, tid);
//             _analyse_var_signal_use(ff, _st, ss, rets, tid)
//         }
//         Stmt::Signal(_sy, _io, _pos) => {
//             // XXX: Check if the signal is already declared in the stack
//             _signal_rep(&_stack, _sy, _pos.0, _pos.1, ff);
//             // XXX: Push the signal Symbol into the hashmap
//             let mut ss = _stack;
//             let idx = ss.len() - 1;
//             ss[idx].insert(
//                 _sy.get_string().to_string(),
//                 (Type::None, SignalVarType::Signal, _io.clone()),
//             );
//             ss
//         }
//         Stmt::Variable(_sy, _type, _val, _pos) => {
//             // XXX: Push the signal Symbol into the hashmap
//             let mut ss = _stack;
//             let idx = ss.len() - 1;
//             ss[idx].insert(
//                 _sy.get_string().to_string(),
//                 (_type.clone(), SignalVarType::Var(tid), None),
//             );
//             ss
//         }
//         Stmt::DataSignal(_sy, _io, _stype, _sval, _sop, _pos) => {
//             // XXX: Check if signal is already declared
//             _signal_rep(&_stack, _sy, _pos.0, _pos.1, ff);
//             // XXX: Push the signal Symbol into the hashmap
//             let mut ss = _stack;
//             let idx = ss.len() - 1;
//             ss[idx].insert(
//                 _sy.get_string().to_string(),
//                 (_stype.clone(), SignalVarType::Signal, _io.clone()),
//             );
//             ss
//         }
//         Stmt::Abort(_expr, _, _body, _pos) | Stmt::Suspend(_expr, _, _body, _pos) => {
//             _check_sym_in_expr(ff, _expr, &_stack, *_pos, rets, tid);
//             // XXX: Check the body
//             _analyse_var_signal_use(ff, _body, _stack, rets, tid)
//         }
//         Stmt::Loop(_body, _pos) =>
// 	    _analyse_var_signal_use(ff, _body, _stack, rets, tid),
//         Stmt::Spar(_bodies, _pos) => {
//             let mut ss = _stack;
//             for (k, i) in _bodies.iter().enumerate() {
//                 ss = _analyse_var_signal_use(ff, i, ss, rets, tid + k + 1);
//             }
//             ss
//         }
//         Stmt::Noop(_) => _stack,
//         Stmt::Assign(_sy, _expr, _pos) => {
//             _check_sym_in_simple_expr(ff, _expr, &_stack, *_pos, rets, tid);
//             _check_sym_in_map(
//                 ff,
//                 _sy,
//                 &_stack,
//                 *_pos,
//                 rets,
//                 SignalVarType::Var(tid),
//                 ValuedSignalDataCheck::NoCheck,
//                 None,
//             );
//             _stack
//         }
// 	// FIXME: We do not perform any analysis for now
// 	Stmt::StructDef(_) => _stack,
// 	Stmt::StructMemberAssign(_, _, _) => _stack,
// 	Stmt::ArrayIndexAssign(_, _, _) => _stack,
// 	Stmt::ExternDef(_, _) => _stack,
//     }
// }

// XXX: Get all the threads in the program
pub fn get_num_threads(_num_threads: &mut usize, _ast: &[Stmt]) {
    _ast.iter().for_each(|x| _get_num_threads(_num_threads, x));
}

fn _get_num_threads(_num_threads: &mut usize, _ast: &Stmt) {
    match _ast {
        Stmt::Block(_sts, _) => get_num_threads(_num_threads, _sts),
        Stmt::Present(_, _t, Some(_r), _) => {
            _get_num_threads(_num_threads, _t);
            _get_num_threads(_num_threads, _r)
        }
        Stmt::Present(_, _t, None, _) => _get_num_threads(_num_threads, _t),
        Stmt::Abort(_, _, _st, _) | Stmt::Suspend(_, _, _st, _) => {
            _get_num_threads(_num_threads, _st)
        }
        Stmt::Loop(_st, _) => _get_num_threads(_num_threads, _st),
        Stmt::Spar(_sts, _) => {
            *_num_threads += _sts.len();
            get_num_threads(_num_threads, _sts)
        }
        _ => (),
    }
}

// XXX: Get all the states in each thread
pub fn get_states(
    _state: &mut [Vec<(Symbol, Pos)>],
    _ast: &[Stmt],
    tid: &mut usize,
    tot: &mut usize,
) {
    _ast.iter().for_each(|x| _get_states(_state, x, tid, tot));
}

fn _get_states(_state: &mut [Vec<(Symbol, Pos)>], stmt: &Stmt, tid: &mut usize,
	       tot: &mut usize) {
    match stmt {
        Stmt::Pause(_sy, _pos) => _state[*tid].push((_sy.clone(), *_pos)),
        Stmt::Block(_sts, _) => get_states(_state, _sts, tid, tot),
        Stmt::Present(_, _t, Some(_r), _) => {
            _get_states(_state, _t, tid, tot);
            _get_states(_state, _r, tid, tot)
        }
        Stmt::Present(_, _t, None, _) => _get_states(_state, _t, tid, tot),
        Stmt::Abort(_, _, _st, _) => _get_states(_state, _st, tid, tot),
        Stmt::Suspend(_, _, _st, _) => _get_states(_state, _st, tid, tot),
        Stmt::Loop(_st, _) => _get_states(_state, _st, tid, tot),
        Stmt::Spar(_sts, _) => {
            let mtid = *tid;
            _sts.iter().for_each(|x| {
                *tid = *tot;
                *tot += 1;
                _get_states(_state, x, tid, tot);
            });
            *tid = mtid;
        }
        _ => (),
    }
}
// FIXME: Try to remove the clone later
pub fn get_externs(_externs: &mut Vec<CallNameType>, _ast: &[Stmt]) {
    fn _get_externs(_externs: &mut Vec<CallNameType>, _st: &Stmt) {
	match _st {
	    Stmt::ExternDef(s, _) => _externs.append(&mut s.clone()),
	    _ => ()
	}
    }
    _ast.iter().for_each(|x| _get_externs(_externs, x))
}


// XXX: This function will get the struct definitions from the toplevel
// of stmts.
pub fn get_structs(_structs: &mut Vec<StructDef>, _ast: &[Stmt]) {
    fn _get_structs(_structs: &mut Vec<StructDef>, _st: &Stmt) {
	match _st {
	    Stmt::StructDef(s) => _structs.push(s.clone()),
	    _ => ()
	}
    }
    _ast.iter()
        .for_each(|x| _get_structs(_structs, x))
}

// XXX: Get all the signals declared in the program and in each thread
pub fn get_signals(_signals: &mut [Vec<Stmt>], _ast: &[Stmt], tid: &mut usize,
		   tot: &mut usize) {
    _ast.iter()
        .for_each(|x| _get_signals(_signals, x, tid, tot))
}

fn _get_signals(signals: &mut [Vec<Stmt>], st: &Stmt, tid: &mut usize,
		tot: &mut usize) {
    match st {
        Stmt::Block(_sts, _) => get_signals(signals, _sts, tid, tot),
        Stmt::Present(_, _t, Some(_r), _) => {
            _get_signals(signals, _t, tid, tot);
            _get_signals(signals, _r, tid, tot)
        }
        Stmt::Present(_, _t, None, _) => _get_signals(signals, _t, tid, tot),
        Stmt::Abort(_, _, _st, _) => _get_signals(signals, _st, tid, tot),
        Stmt::Suspend(_, _, _st, _) => _get_signals(signals, _st, tid, tot),
        Stmt::Loop(_st, _) => _get_signals(signals, _st, tid, tot),
        Stmt::Spar(_sts, _) => {
            let mtid = *tid;
            _sts.iter().for_each(|x| {
                *tid = *tot;
                *tot += 1;
                _get_signals(signals, x, tid, tot)
            });
            *tid = mtid;
        }
        Stmt::Signal(_, _, _) => signals[*tid].push(st.clone()),
        Stmt::DataSignal(_, _, _, _, _, _) => signals[*tid].push(st.clone()),
        _ => (),
    }
}

// XXX: Get all the variables declared in the program and in each thread
pub fn get_vars(_vars: &mut [Vec<Stmt>], _ast: &[Stmt], tid: &mut usize,
		tot: &mut usize) {
    _ast.iter().for_each(|x| _get_vars(_vars, x, tid, tot))
}

fn _get_vars(vars: &mut [Vec<Stmt>], st: &Stmt, tid: &mut usize, tot: &mut usize) {
    match st {
        Stmt::Block(_sts, _) => get_vars(vars, _sts, tid, tot),
        Stmt::Present(_, _t, Some(_r), _) => {
            _get_vars(vars, _t, tid, tot);
            _get_vars(vars, _r, tid, tot)
        }
        Stmt::Present(_, _t, None, _) => _get_vars(vars, _t, tid, tot),
        Stmt::Abort(_, _, _st, _) => _get_vars(vars, _st, tid, tot),
        Stmt::Suspend(_, _, _st, _) => _get_vars(vars, _st, tid, tot),
        Stmt::Loop(_st, _) => _get_vars(vars, _st, tid, tot),
        Stmt::Spar(_sts, _) => {
            let mtid = *tid;
            _sts.iter().for_each(|x| {
                *tid = *tot;
                *tot += 1;
                _get_vars(vars, x, tid, tot)
            });
            *tid = mtid;
        }
        Stmt::Variable(_, _, _, _) => vars[*tid].push(st.clone()),
        _ => (),
    }
}

// XXX: This function gets all the parent ids for every forked child

// XXX: Get all the variables declared in the program and in each thread
pub fn set_parent_tid(_vars: &mut Vec<i64>, _ast: &[Stmt], tid: &mut usize,
		tot: &mut usize) {
    _ast.iter().for_each(|x| _set_parent_tid(_vars, x, tid, tot))
}

fn _set_parent_tid(ptids: &mut Vec<i64>,
		   st: &Stmt, tid: &mut usize, tot: &mut usize) {
    match st {
        Stmt::Block(_sts, _) => set_parent_tid(ptids, _sts, tid, tot),
        Stmt::Present(_, _t, Some(_r), _) => {
            _set_parent_tid(ptids, _t, tid, tot);
            _set_parent_tid(ptids, _r, tid, tot)
        }
        Stmt::Present(_, _t, None, _) => _set_parent_tid(ptids, _t, tid, tot),
        Stmt::Abort(_, _, _st, _) => _set_parent_tid(ptids, _st, tid, tot),
        Stmt::Suspend(_, _, _st, _) => _set_parent_tid(ptids, _st, tid, tot),
        Stmt::Loop(_st, _) => _set_parent_tid(ptids, _st, tid, tot),
        Stmt::Spar(_sts, _) => {
            let mtid = *tid;
            _sts.iter().for_each(|x| {
                *tid = *tot;
                *tot += 1;
                _set_parent_tid(ptids, x, tid, tot);
		// Post-process and set parent' tid
		ptids[*tid] = mtid as i64;
            });
            *tid = mtid;
        }
        _ => (),
    }
}


// XXX: Get all the signals "used" in each thread
// XXX: Get all the vars "used" in each thread
pub fn get_s_v_ref(
    _sref: &mut [Vec<SimpleDataExpr>],
    _syref: &mut [Vec<Symbol>],
    _vref: &mut [Vec<SimpleDataExpr>],
    _vyref: &mut [Vec<Symbol>],
    _ast: &[Stmt],
    tid: &mut usize,
    tot: &mut usize,
) {
    _ast.iter()
        .for_each(|x| _get_s_v_ref(_sref, _syref, _vref, _vyref, x, tid, tot))
}

fn _get_s_v_ref(
    _sref: &mut [Vec<SimpleDataExpr>],
    _syref: &mut [Vec<Symbol>],
    _vref: &mut [Vec<SimpleDataExpr>],
    _vyref: &mut [Vec<Symbol>],
    _st: &Stmt,
    _tid: &mut usize,
    _tot: &mut usize,
) {
    match _st {
        Stmt::Block(_sts, _) =>
	    get_s_v_ref(_sref, _syref, _vref, _vyref, _sts, _tid, _tot),
        Stmt::Emit(_sy, Some(_expr), _) => {
            _syref[*_tid].push(_sy.clone());
            _get_s_v_ref_expr(_sref, _vref, _expr, *_tid)
        }
        Stmt::Emit(_sy, None, _) => {
            _syref[*_tid].push(_sy.clone());
        }
        Stmt::Present(_expr, _t, Some(_r), _) => {
            _get_s_v_ref(_sref, _syref, _vref, _vyref, _t, _tid, _tot);
            _get_s_v_ref(_sref, _syref, _vref, _vyref, _r, _tid, _tot);
            get_s_v_ref_expr(_sref, _syref, _vref, _expr, *_tid)
        }
        Stmt::Present(_expr, _t, None, _) => {
            _get_s_v_ref(_sref, _syref, _vref, _vyref, _t, _tid, _tot);
            get_s_v_ref_expr(_sref, _syref, _vref, _expr, *_tid)
        }
        Stmt::Abort(_expr, _, _st, _) | Stmt::Suspend(_expr, _, _st, _) => {
            _get_s_v_ref(_sref, _syref, _vref, _vyref, _st, _tid, _tot);
            get_s_v_ref_expr(_sref, _syref, _vref, _expr, *_tid)
        }
        Stmt::Loop(_st, _) =>
	    _get_s_v_ref(_sref, _syref, _vref, _vyref, _st, _tid, _tot),
        Stmt::Assign(_sy, _expr, _) => {
            _vyref[*_tid].push(_sy.clone());
            _get_s_v_ref_expr(_sref, _vref, _expr, *_tid)
        }
        Stmt::Spar(_sts, _) => {
            let mtid = *_tid;
            _sts.iter().for_each(|x| {
                *_tid = *_tot;
                *_tot += 1;
                _get_s_v_ref(_sref, _syref, _vref, _vyref, x, _tid, _tot)
            });
            *_tid = mtid;
        }
	Stmt::StructMemberAssign(_stref, _expr, _) => {
	    match _stref {
		StructRefT::StructRef(_sy, _sy1, _)
		    => _vyref[*_tid].push(_sy.clone()),
		StructRefT::StructRefA(_sy, _sy1, _)
		    => _vyref[*_tid].push(_sy.clone()),
		StructRefT::StructRefI(_sy, _sy1, _)
		    => _vyref[*_tid].push(_sy.clone())
	    }
	    _get_s_v_ref_expr(_sref, _vref, _expr, *_tid)
	}
	Stmt::ArrayIndexAssign(_aref, _expr, _) => {
	    match _aref {
		ArrayRefT::ArrayRef(_sy, _, _) =>_vyref[*_tid].push(_sy.clone())
	    }
	    _get_s_v_ref_expr(_sref, _vref, _expr, *_tid)
	}
        _ => (),
    }
}

fn _get_s_v_ref_expr(
    _sref: &mut [Vec<SimpleDataExpr>],
    _vref: &mut [Vec<SimpleDataExpr>],
    _expr: &SimpleDataExpr,
    _tid: usize,
) {
    match _expr {
        SimpleDataExpr::SimpleBinaryOp(_l, _, _r, _) => {
            _get_s_v_ref_expr(_sref, _vref, _l, _tid);
            _get_s_v_ref_expr(_sref, _vref, _r, _tid)
        }
        SimpleDataExpr::VarRef(_, _) => _vref[_tid].push(_expr.clone()),
        SimpleDataExpr::SignalRef(_, _) => _sref[_tid].push(_expr.clone()),
        SimpleDataExpr::Call(_s, _refs, _) => _refs
            .iter()
            .for_each(|x| _get_s_v_ref_expr(_sref, _vref, x, _tid)),
	SimpleDataExpr::ArrayRef(_aref) => {
	    match _aref {
		ArrayRefT::ArrayRef(_, _refs, _) =>
		    _refs
		    .iter()
		    .for_each(|x| _get_s_v_ref_expr(_sref, _vref, x, _tid))
	    }
	}
	// SimpleDataExpr::StructRef(_) => todo!(),
	SimpleDataExpr::Cast(_, _expr, _) => {
	    _vref[_tid].push(*_expr.clone())
	}
        _ => (),
    }
}

fn get_s_v_rel_expr(
    _sref: &mut [Vec<SimpleDataExpr>],
    _vref: &mut [Vec<SimpleDataExpr>],
    _expr: &RelDataExpr,
    _tid: usize,
) {
    match _expr {
        RelDataExpr::LessThan(_l, _r, _)
        | RelDataExpr::GreaterThan(_l, _r, _)
        | RelDataExpr::LessThanEqual(_l, _r, _)
        | RelDataExpr::GreaterThanEqual(_l, _r, _)
        | RelDataExpr::EqualTo(_l, _r, _) => {
            _get_s_v_ref_expr(_sref, _vref, _l, _tid);
            _get_s_v_ref_expr(_sref, _vref, _r, _tid)
        }
    }
}

fn get_s_v_ref_expr(
    _sref: &mut [Vec<SimpleDataExpr>],
    _syref: &mut [Vec<Symbol>],
    _vref: &mut [Vec<SimpleDataExpr>],
    _expr: &Expr,
    _tid: usize,
) {
    match _expr {
        Expr::Esymbol(_sy, _) => _syref[_tid].push(_sy.clone()),
        Expr::And(_l, _r, _) => {
            get_s_v_ref_expr(_sref, _syref, _vref, _l, _tid);
            get_s_v_ref_expr(_sref, _syref, _vref, _r, _tid)
        }
        Expr::Or(_l, _r, _) => {
            get_s_v_ref_expr(_sref, _syref, _vref, _l, _tid);
            get_s_v_ref_expr(_sref, _syref, _vref, _r, _tid)
        }
        Expr::Not(_l, _) => {
            get_s_v_ref_expr(_sref, _syref, _vref, _l, _tid);
        }
        Expr::Brackets(_l, _) => {
            get_s_v_ref_expr(_sref, _syref, _vref, _l, _tid);
        }
        Expr::DataExpr(_rexpr, _) => get_s_v_rel_expr(_sref, _vref, _rexpr, _tid),
        _ => (),
    }
}

fn _print_error(_ff: &str, _pos: &Pos, hp: &HashMap<&String, Pos>, _sy: &Symbol) {
    let _ = print_bytes(_ff, _pos.0, _pos.1);
    let _ = print_bytes(
        _ff,
        hp.get(_sy.get_string()).unwrap().0,
        hp.get(_sy.get_string()).unwrap().1,
    );
    eprintln!("Signal/State names have to be unique throughout program");
}

pub fn _check_state_repeats<'a>(
    _states: &'a [Vec<(Symbol, Pos)>],
    _ff: &'a str,
    hp: HashMap<&'a String, Pos>,
) {
    let mut hp = hp;
    let mut _exit = false;
    _states.iter().for_each(|x| {
        x.iter().for_each(|y| {
            if !hp.contains_key(y.0.get_string()) {
                hp.insert(y.0.get_string(), y.1);
            } else {
                _print_error(_ff, &y.1, &hp, &y.0);
                _exit = true;
            }
        })
    });
    if _exit {
        exit(1);
    }
}

pub fn _check_signal_repeats<'a>(_sigs: &'a [Vec<Stmt>], _ff: &'a str) ->
    HashMap<&'a String, Pos> {
    let mut hp: HashMap<&String, Pos> =
        HashMap::with_capacity(_sigs.iter().fold(0, |acc, x| acc + x.len()));
    let mut _exit = false;
    _sigs.iter().for_each(|x| {
        x.iter().for_each(|y| match y {
            Stmt::Signal(_sy, _, _pos) => {
                if !hp.contains_key(_sy.get_string()) {
                    hp.insert(_sy.get_string(), *_pos);
                } else {
                    _print_error(_ff, _pos, &hp, _sy);
                    _exit = true;
                }
            }
            Stmt::DataSignal(_sy, _, _, _, _, _pos) => {
                if !hp.contains_key(_sy.get_string()) {
                    hp.insert(_sy.get_string(), *_pos);
                } else {
                    _print_error(_ff, &_pos, &hp, _sy);
                    _exit = true;
                }
            }
            _ => panic!("Got a non signal when detecting signal overlap"),
        });
    });
    if _exit {
        exit(1);
    } else {
        hp
    }
}
