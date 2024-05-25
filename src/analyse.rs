use std::collections::HashMap;

// use crate::error::print_bytes;
use sysrust::ast::*;

use crate::error::print_bytes;

#[allow(dead_code)]
pub enum Type {
    Float,
    Int,
    None,
}

#[allow(dead_code)]
type HT = HashMap<Symbol, Type>;
#[allow(dead_code)]
type Pos = (usize, usize);

pub fn _analyse_var_signal_uses(ff: &str, _stmts: &[Stmt], stack: Vec<HT>) -> Vec<HT> {
    let mut u = stack;
    for i in _stmts.iter() {
        u = _analyse_var_signal_use(ff, i, u);
    }
    return u;
}

fn _check_sym_in_map(ff: &str, s: &Symbol, vmap: &[HT], pos: Pos) {
    let mut there = false;
    for map in vmap.iter() {
        if map.contains_key(s) {
            there = true;
            break;
        }
    }
    if !there {
        print_bytes(ff, pos.0, pos.1).unwrap();
    }
}

fn _check_sym_in_simple_expr(_ff: &str, _expr: &SimpleDataExpr, _vmap: &[HT], _pos: Pos) {
    match _expr {
        SimpleDataExpr::SimpleBinaryOp(_l, _, _r, _pos) => {
            _check_sym_in_simple_expr(_ff, _l, _vmap, *_pos);
            _check_sym_in_simple_expr(_ff, _r, _vmap, *_pos);
        }
        SimpleDataExpr::VarRef(_sy, _pos) => _check_sym_in_map(_ff, _sy, _vmap, *_pos),
        SimpleDataExpr::SignalRef(_sy, _pos) => _check_sym_in_map(_ff, _sy, _vmap, *_pos),
        SimpleDataExpr::ConstI(_, _) | SimpleDataExpr::ConstF(_, _) => (),
    }
}

fn _check_sym_in_rel_expr(_ff: &str, _expr: &RelDataExpr, _vmap: &[HT], _pos: Pos) {
    match _expr {
        RelDataExpr::LessThan(_l, _r, _pos)
        | RelDataExpr::LessThanEqual(_l, _r, _pos)
        | RelDataExpr::GreaterThan(_l, _r, _pos)
        | RelDataExpr::GreaterThanEqual(_l, _r, _pos)
        | RelDataExpr::EqualTo(_l, _r, _pos) => {
            _check_sym_in_simple_expr(_ff, _l, _vmap, *_pos);
            _check_sym_in_simple_expr(_ff, _r, _vmap, *_pos);
        }
    }
}

fn _check_sym_in_expr(_ff: &str, _expr: &Expr, _vmap: &[HT], _pos: Pos) {
    match _expr {
        Expr::True(_) | Expr::False(_) => (),
        Expr::Esymbol(_sy, _pos) => _check_sym_in_map(_ff, _sy, _vmap, *_pos),
        Expr::And(_l, _r, _pos) | Expr::Or(_l, _r, _pos) => {
            _check_sym_in_expr(_ff, _l, _vmap, *_pos);
            _check_sym_in_expr(_ff, _r, _vmap, *_pos);
        }
        Expr::Not(_e, _) => _check_sym_in_expr(_ff, _e, _vmap, _pos),
        Expr::Brackets(_e, _pos) => _check_sym_in_expr(_ff, _e, _vmap, *_pos),
        Expr::DataExpr(_re, _pos) => _check_sym_in_rel_expr(_ff, _re, _vmap, *_pos),
    }
}

pub fn _analyse_var_signal_use(ff: &str, stmt: &Stmt, _stack: Vec<HT>) -> Vec<HT> {
    match stmt {
        Stmt::Block(_stmts, _pos) => {
            // XXX: Push a new hashmap on the stack
            let mut _stack = _stack;
            let vmap: HT = HashMap::with_capacity(1000);
            _stack.push(vmap);
            _stack = _analyse_var_signal_uses(ff, _stmts, _stack);
            _stack.pop();
            _stack
        }
        Stmt::Pause(_, _) => _stack,
        Stmt::Emit(_sy, _expr, _pos) => {
            // XXX: Check if the _sy is in the hashmap
            _check_sym_in_map(ff, &_sy, &_stack, *_pos);
            _stack
        }
        Stmt::Present(_sy, _st, None, _pos) => {
            _check_sym_in_expr(ff, &_sy, &_stack, *_pos);
            _analyse_var_signal_use(ff, _st, _stack)
        }
        Stmt::Present(_sy, _st1, Some(_st), _pos) => {
            _check_sym_in_expr(ff, &_sy, &_stack, *_pos);
            let ss = _analyse_var_signal_use(ff, _st1, _stack);
            _analyse_var_signal_use(ff, _st, ss)
        }
        Stmt::Signal(_sy, _io, _pos) => {
            // XXX: Push the signal Symbol into the hashmap
            let mut ss = _stack;
            let idx = ss.len() - 1;
            // FIXME: Fix the type later in parser
            ss[idx].insert(_sy.clone(), Type::Int);
            ss
        }
        Stmt::Abort(_expr, _, _body, _pos) | Stmt::Suspend(_expr, _, _body, _pos) => {
            _check_sym_in_expr(ff, _expr, &_stack, *_pos);
            // XXX: Check the body
            _analyse_var_signal_use(ff, &_body, _stack)
        }
        Stmt::Loop(_body, _pos) => _analyse_var_signal_use(ff, &_body, _stack),
        Stmt::Spar(_bodies, _pos) => {
            let mut ss = _stack;
            for i in _bodies {
                ss = _analyse_var_signal_use(ff, i, ss);
            }
            ss
        }
        Stmt::Noop(_) => _stack,
        Stmt::Assign(_sy, _expr, _pos) => {
            _check_sym_in_simple_expr(ff, &_expr, &_stack, *_pos);
            // FIXME: Add variable declaration in parser
            _stack
        }
    }
}
