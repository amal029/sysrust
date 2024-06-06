use std::{collections::HashSet, iter::zip};

use pretty::RcDoc;
use sysrust::ast::{ExprOp, SimpleDataExpr, Stmt, Symbol, Type, Val};

use crate::error::print_bytes;

fn _symbol_string(_sy: &Symbol) -> &String {
    match _sy {
        Symbol::Symbol(_sy, _) => _sy,
    }
}

fn _type_string<'a>(_ty: &'a Type, _pos: (usize, usize), ff: &'a str) -> &'a str {
    match _ty {
        Type::Int => "int",
        Type::Float => "float",
        Type::None => {
            let _ = print_bytes(ff, _pos.0, _pos.1);
            panic!("Cannot write an empty type")
        }
    }
}

fn _val_string(_val: &Val) -> String {
    match _val {
        Val::VInt(x) => x.to_string(),
        Val::VFloat(x) => x.to_string(),
    }
}

fn _expr_op_std_op<'a>(_expr: &'a ExprOp, _pos: (usize, usize), _ff: &'a str) -> &'a str {
    match _expr {
        ExprOp::Plus => "std::plus",
        ExprOp::Minus => "std::minus",
        ExprOp::Mul => "std::multiplies",
        ExprOp::Div => "std::divides",
        ExprOp::Mod => "std::modulus",
        ExprOp::Pow => {
            let _ = print_bytes(_ff, _pos.0, _pos.1);
            panic!("Power operator not yet supported in C++ backend")
        }
    }
}

fn _sig_decl<'a>(_s: &'a Stmt, _tid: usize, _ff: &'a str) -> RcDoc<'a, ()> {
    match _s {
        Stmt::Signal(_sy, _io, _pos) => {
            let _m = format!("struct signal_{}", _symbol_string(_sy));
            let _m = format!("{} {{bool status;}};", _m);
            let _a = RcDoc::<()>::as_string(_m).append(RcDoc::hardline());
            let sname = _symbol_string(_sy);
            let u = format!("static signal_{} {}_curr, {}_prev;", sname, sname, sname);
            _a.append(RcDoc::as_string(u)).append(RcDoc::hardline())
        }
        Stmt::DataSignal(_sy, _io, _ty, _iv, _op, _pos) => {
            let _m = format!("struct signal_{}", _symbol_string(_sy));
            let _m = format!(
                "{} {{bool status; {} value = {}; {}<{}> op {{}};}};",
                _m,
                _type_string(_ty, *_pos, _ff),
                _val_string(_iv),
                _expr_op_std_op(_op, *_pos, _ff),
                _type_string(_ty, *_pos, _ff)
            );
            let a = RcDoc::<()>::as_string(_m).append(RcDoc::hardline());
            let sname = _symbol_string(_sy);
            let u = format!("static signal_{} {}_curr, {}_prev;", sname, sname, sname);
            a.append(u).append(RcDoc::hardline())
        }
        _ => panic!("Got a non signal when generating C++ backend"),
    }
}

fn _var_decl<'a>(_var: &'a Stmt, _tid: usize, _ff: &'a str) -> RcDoc<'a, ()> {
    match _var {
        Stmt::Variable(_sy, _ty, _iv, _pos) => {
            let _m = format!(
                "{} {}_{} = {};",
                _type_string(_ty, *_pos, _ff),
                _symbol_string(_sy),
                _tid,
                _val_string(_iv)
            );
            RcDoc::<()>::as_string(_m).append(RcDoc::hardline())
        }
        _ => panic!("Got a non variable when generating C++ backend"),
    }
}

fn _get_unique_set(st: &[Vec<Symbol>]) -> Vec<HashSet<&String>> {
    st.iter()
        .map(|x| x.iter().map(|y| _symbol_string(y)).collect::<HashSet<_>>())
        .collect()
}

fn _get_unique_set_sexpr(st: &[Vec<SimpleDataExpr>]) -> Vec<HashSet<&String>> {
    st.iter()
        .map(|i| {
            i.iter()
                .map(|j| match j {
                    SimpleDataExpr::SignalRef(_sy, _) => _symbol_string(_sy),
                    SimpleDataExpr::VarRef(_sy, _) => _symbol_string(_sy),
                    _ => panic!("Got a non signal and variable when making unique names"),
                })
                .collect::<HashSet<_>>()
        })
        .collect()
}

pub fn _prolouge(
    _sigs: &[Vec<Stmt>],
    _vars: &[Vec<Stmt>],
    _nthreads: &usize,
    _states: &[Vec<Symbol>],
    _syref: &[Vec<Symbol>],
    _sref: &[Vec<SimpleDataExpr>],
    _vyref: &[Vec<Symbol>],
    _vref: &[Vec<SimpleDataExpr>],
    _ff: &str,
) -> Vec<u8> {
    let h2 = RcDoc::<()>::as_string("#include <iostream>").append(RcDoc::hardline());
    let h3 = RcDoc::<()>::as_string("#include <variant>").append(RcDoc::hardline());
    let h4 = RcDoc::<()>::as_string("#include <functional>").append(RcDoc::hardline());
    let r = h2.append(h3).append(h4).append(RcDoc::hardline());
    let mut w = Vec::new();
    r.render(8, &mut w).unwrap();

    // XXX: Declare all the signals in the program/thread
    let _m_header = RcDoc::<()>::as_string("// Sig decls").append(RcDoc::hardline());
    _m_header.render(8, &mut w).expect("Cannot write variables");
    for (_i, _s) in _sigs.iter().enumerate() {
        for _ss in _s {
            let _m = _sig_decl(_ss, _i, _ff).append(RcDoc::hardline());
            _m.render(8, &mut w).expect("Cannot declare varaibles");
        }
    }

    // XXX: Declare all the variables in each thread
    let _m_header = RcDoc::<()>::as_string("// Var decls").append(RcDoc::hardline());
    _m_header.render(8, &mut w).expect("Cannot write signals");
    for (_i, _s) in _vars.iter().enumerate() {
        for _ss in _s {
            let _m = _var_decl(_ss, _i, _ff);
            _m.render(8, &mut w).expect("Cannot declare signal");
        }
    }

    // XXX: Now declare the state class
    let _m = RcDoc::<()>::hardline();
    let _m = _m
        .append(RcDoc::as_string("//Decl states"))
        .append(RcDoc::hardline());
    let _m = _m
        .append(RcDoc::as_string("struct State {};"))
        .append(RcDoc::hardline());
    let _m = _m
        .append(RcDoc::as_string("struct E: State {};"))
        .append(RcDoc::hardline());
    let _m = _m
        .append(RcDoc::as_string("struct I: State {};"))
        .append(RcDoc::hardline());
    let _m = _m
        .append(RcDoc::as_string("struct D: State {};"))
        .append(RcDoc::hardline());
    let _m = _m
        .append(RcDoc::as_string("struct ND: State{};"))
        .append(RcDoc::hardline());
    let _ = _m.render(8, &mut w);

    let mut _n = RcDoc::<()>::line();
    for _i in _states {
        for _j in _i {
            let k = format!("struct {} : State {{}};", _symbol_string(_j));
            _n = _n.append(RcDoc::as_string(k)).append(RcDoc::hardline());
        }
    }
    let _ = _n.render(8, &mut w);

    // XXX: Now make the thread classes
    let mut _n = RcDoc::<()>::hardline();
    _n = _n
        .append(RcDoc::as_string("//Threads"))
        .append(RcDoc::hardline());
    for _i in 0..*_nthreads {
        let _k = format!("template <class State> struct Thread{} {{}};", _i);
        _n = _n.append(RcDoc::as_string(_k)).append(RcDoc::hardline());
    }
    let _ = _n.render(8, &mut w);

    let mut _n = RcDoc::<()>::hardline();
    _n = _n
        .append(RcDoc::as_string("//Variant decl"))
        .append(RcDoc::hardline());
    for (_k, _i) in _states.iter().enumerate() {
        let mut _vv: Vec<_> = _i
            .iter()
            .map(|x| format!("Thread{}<{}>", _k, _symbol_string(x)))
            .collect();
        _vv.push(format!("Thread{}<I>", _k));
        _vv.push(format!("Thread{}<E>", _k));
        _vv.push(format!("Thread{}<D>", _k));
        _vv.push(format!("Thread{}<ND>", _k));
        let _vv = _vv.join(", ");
        let _vv = format!("using Thread{}State = std::variant<{}>;", _k, _vv);
        _n = _n.append(RcDoc::as_string(_vv)).append(RcDoc::hardline());
    }
    // XXX: The unique names of signals used in each thread
    let _syref = _get_unique_set(_syref);
    let _sref = _get_unique_set_sexpr(_sref);

    // XXX: Make the string with all the used signals in each thread
    let mut _used_sigs: Vec<String> = Vec::new();
    let mut _used_sigs_vec: Vec<String> = Vec::new();
    let mut _used_sigs_cap: Vec<String> = Vec::new();
    let mut _used_sigs_tick: Vec<String> = Vec::new();
    for i in zip(_syref, _sref) {
        let vv = i.0.union(&i.1).collect::<Vec<_>>();
        let v1 = vv
            .iter()
            .enumerate()
            .map(|(j, x)| format!("signal_{} &_{}", x, j))
            .collect::<Vec<_>>()
            .join(", ");
        _used_sigs_vec.push(v1);

        let v2 = vv
            .iter()
            .enumerate()
            .map(|(j, _)| format!("&_{}", j))
            .collect::<Vec<_>>()
            .join(", ");
        _used_sigs_cap.push(v2);

        let v2 = vv
            .iter()
            .enumerate()
            .map(|(j, _)| format!("_{}", j))
            .collect::<Vec<_>>()
            .join(", ");
        _used_sigs_tick.push(v2);

        let vv = vv
            .iter()
            .map(|x| format!("signal_{} &", x))
            .collect::<Vec<_>>()
            .join(", ");
        _used_sigs.push(vv);
    }

    // XXX: All thread prototypes
    let mut thread_prototypes: Vec<String> = Vec::with_capacity(_states.len() + 100);
    assert!(_used_sigs.len() == _states.len());
    for (i, (k1, k2)) in zip(_used_sigs, _states).enumerate() {
        // XXX: First make I, ND, and D states
        let _ss = format!(
            "template <> struct Thread{}<E>{{\nconstexpr void tick \
			  ({});}};",
            i, k1
        );
        thread_prototypes.push(_ss);
        let _ss = format!(
            "template <> struct Thread{}<I>{{\nconstexpr void tick \
			  ({});}};",
            i, k1
        );
        thread_prototypes.push(_ss);
        let _ss = format!(
            "template <> struct Thread{}<ND>{{\nconstexpr void tick \
			  ({});}};",
            i, k1
        );
        thread_prototypes.push(_ss);
        let _ss = format!(
            "template <> struct Thread{}<D>{{\nconstexpr void tick \
			  ({});}};",
            i, k1
        );
        thread_prototypes.push(_ss);
        for j in k2 {
            let mm = _symbol_string(j);
            let _ss = format!(
                "template <> struct Thread{}<{}>{{\nconstexpr void tick \
			  ({});}};",
                i, mm, k1
            );
            thread_prototypes.push(_ss);
        }
    }
    let _prototypes = thread_prototypes.join("\n");
    _n = _n
        .append(RcDoc::hardline())
        .append(RcDoc::as_string(_prototypes))
        .append(RcDoc::hardline());

    let _threadvar: Vec<_> = (0..*_nthreads)
        .map(|x| format!("static Thread{}State st{};", x, x))
        .collect();
    let _threadvar = _threadvar.join("\n");
    _n = _n
        .append(RcDoc::hardline())
        .append(_threadvar)
        .append(RcDoc::hardline());

    // XXX: Make the initial functions
    let _inits: Vec<_> = (0..*_nthreads)
        .map(|x| {
            format!(
                "constexpr void init{}(){{st{} = Thread{}<I> {{}};}}",
                x, x, x
            )
        })
        .collect();
    let _inits = _inits.join("\n");
    _n = _n
        .append(RcDoc::hardline())
        .append(RcDoc::hardline())
        .append(_inits)
        .append(RcDoc::hardline());

    // XXX: Make the overloaded template metaprogramming
    let _o = "template <class... Ts> struct overloaded: \
	 Ts... {using Ts::operator()...;};"
        .to_string();
    _n = _n.append(RcDoc::hardline()).append(_o);

    // XXX: All the visits
    assert!(*_nthreads == _used_sigs_vec.len());
    assert!(*_nthreads == _used_sigs_cap.len());
    let _hh = (0..*_nthreads)
        .map(|i| {
            let _f1 = if _used_sigs_vec[i] != "" {
                format!(
                    "constexpr void visit{}(Thread{}State &ts, {}){{\
		 std::visit(overloaded{{[{}](auto &t){{return t.tick({});}}}}, ts);}}",
                    i, i, _used_sigs_vec[i], _used_sigs_cap[i], _used_sigs_tick[i]
                )
            } else {
                format!(
                    "constexpr void visit{}(Thread{}State &ts{}){{\
		 std::visit(overloaded{{[{}](auto &t){{return t.tick({});}}}}, ts);}}",
                    i, i, _used_sigs_vec[i], _used_sigs_cap[i], _used_sigs_tick[i]
                )
            };
            _f1
        })
        .collect::<Vec<_>>();
    let _hh = _hh.join("\n");
    _n = _n
        .append(RcDoc::hardline())
        .append(RcDoc::as_string(_hh))
        .append(RcDoc::hardline());

    // TODO: The real code from the FSM

    let _ = _n.render(8, &mut w);
    // String::from_utf8(w).expect("Could not generate the prolouge")
    w
}
