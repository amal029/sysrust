use std::collections::HashMap;

// use crate::error::print_bytes;
use sysrust::ast::*;

#[allow(dead_code)]
// #[derive(Debug)]
// pub enum Type {
//     Float,
//     Int,
//     None,
// }

#[allow(dead_code)]
type HT = HashMap<String, Type>;
#[allow(dead_code)]
type Pos = (usize, usize);
type Pos1 = (usize, usize, String);

fn symbol_string(s: &Symbol) -> String {
    match s {
        Symbol::Symbol(ss, _) => ss.clone(),
    }
}

pub fn _analyse_var_signal_uses(
    ff: &str,
    _stmts: &[Stmt],
    stack: Vec<HT>,
    rets: &mut Vec<Pos1>,
) -> Vec<HT> {
    let mut u = stack;
    for i in _stmts.iter() {
        u = _analyse_var_signal_use(ff, i, u, rets);
    }
    return u;
}

fn _check_sym_in_map(_ff: &str, s: &Symbol, vmap: &[HT], pos: Pos, rets: &mut Vec<Pos1>) {
    let mut there = false;
    for map in vmap.iter() {
        if map.contains_key(&symbol_string(s)) {
            there = true;
            break;
        }
    }
    if !there {
        rets.push((pos.0, pos.1, symbol_string(s)));
        // print_bytes(ff, pos.0, pos.1).unwrap();
    }
}

fn _check_sym_in_simple_expr(
    _ff: &str,
    _expr: &SimpleDataExpr,
    _vmap: &[HT],
    _pos: Pos,
    rets: &mut Vec<Pos1>,
) {
    match _expr {
        SimpleDataExpr::SimpleBinaryOp(_l, _, _r, _) => {
            _check_sym_in_simple_expr(_ff, _l, _vmap, _pos, rets);
            _check_sym_in_simple_expr(_ff, _r, _vmap, _pos, rets);
        }
        SimpleDataExpr::VarRef(_sy, _) => _check_sym_in_map(_ff, _sy, _vmap, _pos, rets),
        SimpleDataExpr::SignalRef(_sy, _) => _check_sym_in_map(_ff, _sy, _vmap, _pos, rets),
        SimpleDataExpr::ConstI(_, _) | SimpleDataExpr::ConstF(_, _) => (),
    }
}

fn _check_sym_in_rel_expr(
    _ff: &str,
    _expr: &RelDataExpr,
    _vmap: &[HT],
    _pos: Pos,
    rets: &mut Vec<Pos1>,
) {
    match _expr {
        RelDataExpr::LessThan(_l, _r, _pos)
        | RelDataExpr::LessThanEqual(_l, _r, _pos)
        | RelDataExpr::GreaterThan(_l, _r, _pos)
        | RelDataExpr::GreaterThanEqual(_l, _r, _pos)
        | RelDataExpr::EqualTo(_l, _r, _pos) => {
            _check_sym_in_simple_expr(_ff, _l, _vmap, *_pos, rets);
            _check_sym_in_simple_expr(_ff, _r, _vmap, *_pos, rets);
        }
    }
}

fn _check_sym_in_expr(_ff: &str, _expr: &Expr, _vmap: &[HT], _pos: Pos, rets: &mut Vec<Pos1>) {
    match _expr {
        Expr::True(_) | Expr::False(_) => (),
        Expr::Esymbol(_sy, _) => _check_sym_in_map(_ff, _sy, _vmap, _pos, rets),
        Expr::And(_l, _r, _) | Expr::Or(_l, _r, _) => {
            _check_sym_in_expr(_ff, _l, _vmap, _pos, rets);
            _check_sym_in_expr(_ff, _r, _vmap, _pos, rets);
        }
        Expr::Not(_e, _) => _check_sym_in_expr(_ff, _e, _vmap, _pos, rets),
        Expr::Brackets(_e, _pos) => _check_sym_in_expr(_ff, _e, _vmap, *_pos, rets),
        Expr::DataExpr(_re, _pos) => _check_sym_in_rel_expr(_ff, _re, _vmap, *_pos, rets),
    }
}

pub fn _analyse_var_signal_use(
    ff: &str,
    stmt: &Stmt,
    _stack: Vec<HT>,
    rets: &mut Vec<Pos1>,
) -> Vec<HT> {
    match stmt {
        Stmt::Block(_stmts, _pos) => {
            // XXX: Push a new hashmap on the stack
            let mut _stack = _stack;
            let vmap: HT = HashMap::with_capacity(1000);
            _stack.push(vmap);
            _stack = _analyse_var_signal_uses(ff, _stmts, _stack, rets);
            _stack.pop();
            _stack
        }
        Stmt::Pause(_, _) => _stack,
        Stmt::Emit(_sy, _expr, _pos) => {
            // XXX: Check if the _sy is in the hashmap
            _check_sym_in_map(ff, &_sy, &_stack, *_pos, rets);
            _stack
        }
        Stmt::Present(_sy, _st, None, _pos) => {
            _check_sym_in_expr(ff, &_sy, &_stack, *_pos, rets);
            _analyse_var_signal_use(ff, _st, _stack, rets)
        }
        Stmt::Present(_sy, _st1, Some(_st), _pos) => {
            _check_sym_in_expr(ff, &_sy, &_stack, *_pos, rets);
            let ss = _analyse_var_signal_use(ff, _st1, _stack, rets);
            _analyse_var_signal_use(ff, _st, ss, rets)
        }
        Stmt::Signal(_sy, _io, _pos) => {
            // XXX: Push the signal Symbol into the hashmap
            let mut ss = _stack;
            let idx = ss.len() - 1;
            // FIXME: Fix the type later in parser
            ss[idx].insert(symbol_string(_sy), Type::None);
            ss
        }
        Stmt::Variable(_sy, _type, _pos) => {
            // XXX: Push the signal Symbol into the hashmap
            let mut ss = _stack;
            let idx = ss.len() - 1;
            // FIXME: Fix the type later in parser
            ss[idx].insert(symbol_string(_sy), _type.clone());
            ss
        }
        Stmt::DataSignal(_sy, _io, _stype, _sval, _sop, _pos) => {
            // XXX: Push the signal Symbol into the hashmap
            let mut ss = _stack;
            let idx = ss.len() - 1;
            // FIXME: Fix the type later in parser
            ss[idx].insert(symbol_string(_sy), _stype.clone());
            ss
        }
        Stmt::Abort(_expr, _, _body, _pos) | Stmt::Suspend(_expr, _, _body, _pos) => {
            _check_sym_in_expr(ff, _expr, &_stack, *_pos, rets);
            // XXX: Check the body
            _analyse_var_signal_use(ff, &_body, _stack, rets)
        }
        Stmt::Loop(_body, _pos) => _analyse_var_signal_use(ff, &_body, _stack, rets),
        Stmt::Spar(_bodies, _pos) => {
            let mut ss = _stack;
            for i in _bodies {
                ss = _analyse_var_signal_use(ff, i, ss, rets);
            }
            ss
        }
        Stmt::Noop(_) => _stack,
        Stmt::Assign(_sy, _expr, _pos) => {
            _check_sym_in_simple_expr(ff, &_expr, &_stack, *_pos, rets);
	    _check_sym_in_map(ff, _sy, &_stack, *_pos, rets);
            // FIXME: Add variable declaration in parser
            _stack
        }
    }
}
