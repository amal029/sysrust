use std::collections::HashMap;

use sysrust::ast::*;

pub enum SignalVarType {
    Signal,
    Var(usize),
}

enum ValuedSignalDataCheck {
    Check,
    NoCheck,
}

#[allow(dead_code)]
type HT = HashMap<String, (Type, SignalVarType)>;
#[allow(dead_code)]
type Pos = (usize, usize);
type Pos1 = (usize, usize, String);

pub fn symbol_string(s: &Symbol) -> String {
    match s {
        Symbol::Symbol(ss, _) => ss.clone(),
    }
}

pub fn _analyse_var_signal_uses(
    ff: &str,
    _stmts: &[Stmt],
    stack: Vec<HT>,
    rets: &mut Vec<Pos1>,
    tid: usize,
) -> Vec<HT> {
    let mut u = stack;
    for i in _stmts.iter() {
        u = _analyse_var_signal_use(ff, i, u, rets, tid);
    }
    u
}

fn _check_sym_in_map(
    _ff: &str,
    s: &Symbol,
    vmap: &[HT],
    pos: Pos,
    rets: &mut Vec<Pos1>,
    _svt: SignalVarType,
    _dvt: ValuedSignalDataCheck,
) {
    fn match_signal_var_type(_svt1: &SignalVarType, _svt2: &SignalVarType) -> bool {
        match (_svt1, _svt2) {
            (SignalVarType::Signal, SignalVarType::Signal) => true,
            (SignalVarType::Var(x), SignalVarType::Var(y)) if (x == y) => true,
            (_, _) => false,
        }
    }
    let mut there = false;
    let mut _dcheck = false;
    for map in vmap.iter() {
        if map.contains_key(&symbol_string(s)) {
            let _svt2 = &map.get(&symbol_string(s)).unwrap().1;
            if match_signal_var_type(&_svt, _svt2) {
                // XXX: Check if the signal ref being used is a valued signal.
                _dcheck = match _dvt {
                    ValuedSignalDataCheck::Check => {
                        let _dvt2 = &map.get(&symbol_string(s)).unwrap().0;
                        match _dvt2 {
                            Type::Float | Type::Int => true,
                            Type::None => false,
                        }
                    }
                    ValuedSignalDataCheck::NoCheck => true,
                };
                there = true;
                break;
            }
        }
    }
    if !there {
        let ss = format!(
            "Signal/Var {} not declared.\nSignal and variable \
	     declarations can shadow each other.\nVariables cannot be shared between threads.",
            symbol_string(s)
        );
        rets.push((pos.0, pos.1, ss));
        return;
    }
    if !_dcheck {
        let ss = format!("Signal {} is not a valued signal.", symbol_string(s));
        rets.push((pos.0, pos.1, ss));
    }
}

fn _check_sym_in_simple_expr(
    _ff: &str,
    _expr: &SimpleDataExpr,
    _vmap: &[HT],
    _pos: Pos,
    rets: &mut Vec<Pos1>,
    tid: usize,
) {
    match _expr {
        SimpleDataExpr::SimpleBinaryOp(_l, _, _r, _) => {
            _check_sym_in_simple_expr(_ff, _l, _vmap, _pos, rets, tid);
            _check_sym_in_simple_expr(_ff, _r, _vmap, _pos, rets, tid);
        }
        SimpleDataExpr::VarRef(_sy, _) => _check_sym_in_map(
            _ff,
            _sy,
            _vmap,
            _pos,
            rets,
            SignalVarType::Var(tid),
            ValuedSignalDataCheck::NoCheck,
        ),
        SimpleDataExpr::SignalRef(_sy, _) => _check_sym_in_map(
            _ff,
            _sy,
            _vmap,
            _pos,
            rets,
            SignalVarType::Signal,
            ValuedSignalDataCheck::Check,
        ),
        SimpleDataExpr::Call(_sy, _vexp, _) => _vexp
            .iter()
            .for_each(|x| _check_sym_in_simple_expr(_ff, x, _vmap, _pos, rets, tid)),
        SimpleDataExpr::ConstI(_, _) | SimpleDataExpr::ConstF(_, _) => (),
    }
}

fn _check_sym_in_rel_expr(
    _ff: &str,
    _expr: &RelDataExpr,
    _vmap: &[HT],
    _pos: Pos,
    rets: &mut Vec<Pos1>,
    tid: usize,
) {
    match _expr {
        RelDataExpr::LessThan(_l, _r, _)
        | RelDataExpr::LessThanEqual(_l, _r, _)
        | RelDataExpr::GreaterThan(_l, _r, _)
        | RelDataExpr::GreaterThanEqual(_l, _r, _)
        | RelDataExpr::EqualTo(_l, _r, _) => {
            _check_sym_in_simple_expr(_ff, _l, _vmap, _pos, rets, tid);
            _check_sym_in_simple_expr(_ff, _r, _vmap, _pos, rets, tid);
        }
    }
}

fn _check_sym_in_expr(
    _ff: &str,
    _expr: &Expr,
    _vmap: &[HT],
    _pos: Pos,
    rets: &mut Vec<Pos1>,
    tid: usize,
) {
    match _expr {
        Expr::True(_) | Expr::False(_) => (),
        Expr::Esymbol(_sy, _) => _check_sym_in_map(
            _ff,
            _sy,
            _vmap,
            _pos,
            rets,
            SignalVarType::Signal,
            ValuedSignalDataCheck::NoCheck,
        ),
        Expr::And(_l, _r, _) | Expr::Or(_l, _r, _) => {
            _check_sym_in_expr(_ff, _l, _vmap, _pos, rets, tid);
            _check_sym_in_expr(_ff, _r, _vmap, _pos, rets, tid);
        }
        Expr::Not(_e, _) => _check_sym_in_expr(_ff, _e, _vmap, _pos, rets, tid),
        Expr::Brackets(_e, _) => _check_sym_in_expr(_ff, _e, _vmap, _pos, rets, tid),
        Expr::DataExpr(_re, _) => _check_sym_in_rel_expr(_ff, _re, _vmap, _pos, rets, tid),
    }
}

pub fn _analyse_var_signal_use(
    ff: &str,
    stmt: &Stmt,
    _stack: Vec<HT>,
    rets: &mut Vec<Pos1>,
    tid: usize,
) -> Vec<HT> {
    match stmt {
        Stmt::Block(_stmts, _pos) => {
            // XXX: Push a new hashmap on the stack
            let mut _stack = _stack;
            let vmap: HT = HashMap::with_capacity(1000);
            _stack.push(vmap);
            _stack = _analyse_var_signal_uses(ff, _stmts, _stack, rets, tid);
            _stack.pop();
            _stack
        }
        Stmt::Pause(_, _) => _stack,
        Stmt::Emit(_sy, _expr, _pos) => {
            // XXX: Check if the _sy is in the hashmap
            _check_sym_in_map(
                ff,
                _sy,
                &_stack,
                *_pos,
                rets,
                SignalVarType::Signal,
                ValuedSignalDataCheck::NoCheck,
            );
            _stack
        }
        Stmt::Present(_sy, _st, None, _pos) => {
            _check_sym_in_expr(ff, _sy, &_stack, *_pos, rets, tid);
            _analyse_var_signal_use(ff, _st, _stack, rets, tid)
        }
        Stmt::Present(_sy, _st1, Some(_st), _pos) => {
            _check_sym_in_expr(ff, _sy, &_stack, *_pos, rets, tid);
            let ss = _analyse_var_signal_use(ff, _st1, _stack, rets, tid);
            _analyse_var_signal_use(ff, _st, ss, rets, tid)
        }
        Stmt::Signal(_sy, _io, _pos) => {
            // XXX: Push the signal Symbol into the hashmap
            let mut ss = _stack;
            let idx = ss.len() - 1;
            ss[idx].insert(symbol_string(_sy), (Type::None, SignalVarType::Signal));
            ss
        }
        Stmt::Variable(_sy, _type, _val, _pos) => {
            // XXX: Push the signal Symbol into the hashmap
            let mut ss = _stack;
            let idx = ss.len() - 1;
            ss[idx].insert(symbol_string(_sy), (_type.clone(), SignalVarType::Var(tid)));
            ss
        }
        Stmt::DataSignal(_sy, _io, _stype, _sval, _sop, _pos) => {
            // XXX: Push the signal Symbol into the hashmap
            let mut ss = _stack;
            let idx = ss.len() - 1;
            ss[idx].insert(symbol_string(_sy), (_stype.clone(), SignalVarType::Signal));
            ss
        }
        Stmt::Abort(_expr, _, _body, _pos) | Stmt::Suspend(_expr, _, _body, _pos) => {
            _check_sym_in_expr(ff, _expr, &_stack, *_pos, rets, tid);
            // XXX: Check the body
            _analyse_var_signal_use(ff, _body, _stack, rets, tid)
        }
        Stmt::Loop(_body, _pos) => _analyse_var_signal_use(ff, _body, _stack, rets, tid),
        Stmt::Spar(_bodies, _pos) => {
            let mut ss = _stack;
            for (k, i) in _bodies.iter().enumerate() {
                ss = _analyse_var_signal_use(ff, i, ss, rets, tid + k + 1);
            }
            ss
        }
        Stmt::Noop(_) => _stack,
        Stmt::Assign(_sy, _expr, _pos) => {
            _check_sym_in_simple_expr(ff, _expr, &_stack, *_pos, rets, tid);
            _check_sym_in_map(
                ff,
                _sy,
                &_stack,
                *_pos,
                rets,
                SignalVarType::Var(tid),
                ValuedSignalDataCheck::NoCheck,
            );
            _stack
        }
    }
}

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
pub fn get_states(_state: &mut [Vec<Symbol>], _ast: &[Stmt], tid: usize) {
    _ast.iter().for_each(|x| _get_states(_state, x, tid));
}

fn _get_states(_state: &mut [Vec<Symbol>], stmt: &Stmt, tid: usize) {
    match stmt {
        Stmt::Pause(_sy, _) => _state[tid].push(_sy.clone()),
        Stmt::Block(_sts, _) => get_states(_state, _sts, tid),
        Stmt::Present(_, _t, Some(_r), _) => {
            _get_states(_state, _t, tid);
            _get_states(_state, _r, tid)
        }
        Stmt::Present(_, _t, None, _) => _get_states(_state, _t, tid),
        Stmt::Abort(_, _, _st, _) => _get_states(_state, _st, tid),
        Stmt::Suspend(_, _, _st, _) => _get_states(_state, _st, tid),
        Stmt::Loop(_st, _) => _get_states(_state, _st, tid),
        Stmt::Spar(_sts, _) => _sts
            .iter()
            .enumerate()
            .for_each(|(j, x)| _get_states(_state, x, tid + j + 1)),
        _ => (),
    }
}

// XXX: Get all the signals declared in the program and in each thread
pub fn get_signals(_signals: &mut [Vec<Stmt>], _ast: &[Stmt], tid: usize) {
    _ast.iter().for_each(|x| _get_signals(_signals, x, tid))
}

fn _get_signals(signals: &mut [Vec<Stmt>], st: &Stmt, tid: usize) {
    match st {
        Stmt::Block(_sts, _) => get_signals(signals, _sts, tid),
        Stmt::Present(_, _t, Some(_r), _) => {
            _get_signals(signals, _t, tid);
            _get_signals(signals, _r, tid)
        }
        Stmt::Present(_, _t, None, _) => _get_signals(signals, _t, tid),
        Stmt::Abort(_, _, _st, _) => _get_signals(signals, _st, tid),
        Stmt::Suspend(_, _, _st, _) => _get_signals(signals, _st, tid),
        Stmt::Loop(_st, _) => _get_signals(signals, _st, tid),
        Stmt::Spar(_sts, _) => _sts
            .iter()
            .enumerate()
            .for_each(|(j, x)| _get_signals(signals, x, tid + j + 1)),
        Stmt::Signal(_, _, _) => signals[tid].push(st.clone()),
        Stmt::DataSignal(_, _, _, _, _, _) => signals[tid].push(st.clone()),
        _ => (),
    }
}

// XXX: Get all the variables declared in the program and in each thread
pub fn get_vars(_vars: &mut [Vec<Stmt>], _ast: &[Stmt], tid: usize) {
    _ast.iter().for_each(|x| _get_vars(_vars, x, tid))
}

fn _get_vars(vars: &mut [Vec<Stmt>], st: &Stmt, tid: usize) {
    match st {
        Stmt::Block(_sts, _) => get_vars(vars, _sts, tid),
        Stmt::Present(_, _t, Some(_r), _) => {
            _get_vars(vars, _t, tid);
            _get_vars(vars, _r, tid)
        }
        Stmt::Present(_, _t, None, _) => _get_vars(vars, _t, tid),
        Stmt::Abort(_, _, _st, _) => _get_vars(vars, _st, tid),
        Stmt::Suspend(_, _, _st, _) => _get_vars(vars, _st, tid),
        Stmt::Loop(_st, _) => _get_vars(vars, _st, tid),
        Stmt::Spar(_sts, _) => _sts
            .iter()
            .enumerate()
            .for_each(|(j, x)| _get_vars(vars, x, tid + j + 1)),
        Stmt::Variable(_, _, _, _) => vars[tid].push(st.clone()),
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
    tid: usize,
) {
    _ast.iter()
        .for_each(|x| _get_s_v_ref(_sref, _syref, _vref, _vyref, x, tid))
}

fn _get_s_v_ref(
    _sref: &mut [Vec<SimpleDataExpr>],
    _syref: &mut [Vec<Symbol>],
    _vref: &mut [Vec<SimpleDataExpr>],
    _vyref: &mut [Vec<Symbol>],
    _st: &Stmt,
    _tid: usize,
) {
    match _st {
        Stmt::Block(_sts, _) => get_s_v_ref(_sref, _syref, _vref, _vyref, _sts, _tid),
        Stmt::Emit(_sy, Some(_expr), _) => {
            _syref[_tid].push(_sy.clone());
            _get_s_v_ref_expr(_sref, _vref, _expr, _tid)
        }
        Stmt::Emit(_sy, None, _) => {
            _syref[_tid].push(_sy.clone());
        }
        Stmt::Present(_expr, _t, Some(_r), _) => {
            _get_s_v_ref(_sref, _syref, _vref, _vyref, _t, _tid);
            _get_s_v_ref(_sref, _syref, _vref, _vyref, _r, _tid);
            get_s_v_ref_expr(_sref, _syref, _vref, _expr, _tid)
        }
        Stmt::Present(_expr, _t, None, _) => {
            _get_s_v_ref(_sref, _syref, _vref, _vyref, _t, _tid);
            get_s_v_ref_expr(_sref, _syref, _vref, _expr, _tid)
        }
        Stmt::Abort(_expr, _, _st, _) | Stmt::Suspend(_expr, _, _st, _) => {
            _get_s_v_ref(_sref, _syref, _vref, _vyref, _st, _tid);
            get_s_v_ref_expr(_sref, _syref, _vref, _expr, _tid)
        }
        Stmt::Loop(_st, _) => _get_s_v_ref(_sref, _syref, _vref, _vyref, _st, _tid),
        Stmt::Assign(_sy, _expr, _) => {
            _vyref[_tid].push(_sy.clone());
            _get_s_v_ref_expr(_sref, _vref, _expr, _tid)
        }
        Stmt::Spar(_sts, _) => _sts
            .iter()
            .enumerate()
            .for_each(|(j, x)| _get_s_v_ref(_sref, _syref, _vref, _vyref, x, _tid + j + 1)),
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
