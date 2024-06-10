use std::{
    collections::{HashMap, HashSet, VecDeque},
    iter::zip,
};

use pretty::RcDoc;
use sysrust::ast::{CallNameType, ExprOp, SimpleDataExpr, Stmt, Symbol, Type, Val};

use crate::{
    error::print_bytes,
    rewrite::{GraphNode, NodeT},
};

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

pub fn _codegen(
    _sigs: &[Vec<Stmt>],
    _vars: &[Vec<Stmt>],
    _nthreads: &usize,
    _states: &[Vec<Symbol>],
    _syref: &[Vec<Symbol>],
    _sref: &[Vec<SimpleDataExpr>],
    _vyref: &[Vec<Symbol>],
    _vref: &[Vec<SimpleDataExpr>],
    _ff: &str,
    // XXX: This is for external calls
    _ext_call: &[CallNameType],
    // XXX: The nodes and the graph adj-list of the graph FSM
    _ginode: usize,
    _genode: usize,
    _gnodes: &[GraphNode],
) -> Vec<u8> {
    let h2 = RcDoc::<()>::as_string("#include <iostream>").append(RcDoc::hardline());
    let h3 = RcDoc::<()>::as_string("#include <variant>").append(RcDoc::hardline());
    let h4 = RcDoc::<()>::as_string("#include <functional>").append(RcDoc::hardline());
    let r = h2.append(h3).append(h4).append(RcDoc::hardline());
    let mut w = Vec::new();
    r.render(8, &mut w).unwrap();

    // XXX: Extern calls being put here
    let mut _ec = RcDoc::<()>::as_string("extern \"C\"{");
    let _ecs = _ext_call.into_iter().fold(RcDoc::nil(), |acc, _x| {
        let __x = _x._get_doc();
        acc.append(__x)
    });
    _ec = _ec.append(_ecs).append("}").append(RcDoc::hardline());
    _ec.render(8, &mut w).unwrap();

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
    let mut _sigs_map_per_thread: Vec<HashMap<&str, usize>> = Vec::new();
    let mut _used_sigs_cap: Vec<String> = Vec::new();
    let mut _used_sigs_tick: Vec<String> = Vec::new();
    for i in zip(_syref, _sref) {
        let mut _sigs_map: HashMap<&str, usize> = HashMap::new();
        let vv = i.0.union(&i.1).collect::<Vec<_>>();
        let v1 = vv
            .iter()
            .enumerate()
            .map(|(j, &&x)| {
                _sigs_map.insert(x, j);
                format!("signal_{} &_{}", x, j)
            })
            .collect::<Vec<_>>()
            .join(", ");
        _sigs_map_per_thread.push(_sigs_map);
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
	     ({}){{}}}};",
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

    // XXX: The real code from the FSM
    let _nn = _make_fsm_code(
        _ginode,
        _genode,
        _gnodes,
        *_nthreads,
        &_used_sigs_vec,
        _sigs_map_per_thread,
    );
    let mut w1: Vec<u8> = Vec::with_capacity(1000);
    let _ = _nn.render(8, &mut w1);

    let _ = _n.render(8, &mut w);
    // String::from_utf8(w).expect("Could not generate the prolouge")
    w.append(&mut w1);
    return w;
}

fn _walk_graph_code_gen<'a>(
    _f: usize,
    _l: usize,
    _rets: VecDeque<usize>,
    _nodes: &'a [GraphNode],
    _n: RcDoc<'a>,
    _used_sigs_per_thread: &'a [String],
    _done_nodes: &[usize],
    _sigs_map_per_threads: &Vec<HashMap<&str, usize>>,
) -> (RcDoc<'a>, VecDeque<usize>, usize) {
    fn _gen_code<'a>(
        _f: usize,
        _l: usize,
        _rets: VecDeque<usize>,
        _nodes: &'a [GraphNode],
        _n: RcDoc<'a>,
        _i: usize,
        _ptid: usize,
        _used_sigs_per_thread: &'a [String],
        first: u8,
        _sigs_map_per_threads: &Vec<HashMap<&str, usize>>,
    ) -> (RcDoc<'a>, VecDeque<usize>) {
        // XXX: First != 0 means that we have already reached this node
        // and we want to continue from here
        if _nodes[_i].tag && first != 0 {
            // FIXME: Handle the case when the _tid of stop is different
            // from current thread id

            // FIXME: Also need to handle the join node for parallelism

            // XXX: We have already found where to stop
            if _i != _l {
                // XXX: This must be a pause
                match _nodes[_i].tt {
                    NodeT::PauseStart => (),
                    _ => panic!("Got a non pause stop state: {:?}", _nodes[_i]),
                }
                let mut _rets = _rets;
                // XXX: Push yourself into the queue for codegen later
                _rets.push_back(_i);
                let s = format!(
                    "st{} = Thread{}<{}>{{}};",
                    _nodes[_i]._tid, _nodes[_i]._tid, _nodes[_i].label
                );
                let mut _n = _n;
                _n = _n.append(RcDoc::as_string(s));
                return (_n, _rets);
            } else if _i == _l {
                // XXX: Just make the end node code
                let s = format!("st{} = Thread{}<E>{{}};", _nodes[_i]._tid, _nodes[_i]._tid,);
                let mut _n = _n;
                _n = _n.append(RcDoc::as_string(s));
                return (_n, _rets);
            } else {
                panic!("Reached a deadend: {:?}", _nodes[_i])
            }
            // XXX: Now make the code for this node and return
        }

        // XXX: First do a map of each child branch
        let _cbm = _nodes[_i]
            .children
            .iter()
            .map(|x| {
                _gen_code(
                    _f,
                    _l,
                    _rets.clone(),
                    _nodes,
                    _n.clone(),
                    *x,
                    _nodes[_i]._tid,
                    _used_sigs_per_thread,
                    first + 1,
                    _sigs_map_per_threads,
                )
            })
            .collect::<Vec<_>>();
        let mut _bn: Vec<RcDoc> = Vec::with_capacity(_cbm.len());
        let mut _rn: Vec<VecDeque<usize>> = Vec::with_capacity(_cbm.len());
        for (i, j) in _cbm {
            _bn.push(i);
            _rn.push(j);
        }

        // XXX: Make the guards for each branch
        let _gm = _nodes[_i]
            .guards
            .iter()
            .map(|x| x.codegen(_nodes[_i]._tid, &_sigs_map_per_threads[_nodes[_i]._tid]))
            .collect::<Vec<_>>();

        // XXX: Make the actions for each branch
        let _am = _nodes[_i]
            .actions
            .iter()
            .map(|x| x.codegen(_nodes[_i]._tid, &_sigs_map_per_threads[_nodes[_i]._tid]))
            .collect::<Vec<_>>();

        // XXX: First combine the actions with _bn
        assert!(_am.len() <= _bn.len());
        let _bn = _bn
            .into_iter()
            .enumerate()
            .map(|(i, x)| {
                if i <= _am.len() && !_am.is_empty() {
                    _am[i].clone().append(x)
                } else {
                    x
                }
            })
            .collect::<Vec<_>>();

        // XXX: Confirm we have a guard for each branch
        assert!(_gm.len() == _bn.len());
        let _gm = _gm.into_iter().map(|x| {
            RcDoc::as_string("if(")
                .append(x)
                .append(RcDoc::as_string(")"))
        });
        let mut __n = RcDoc::nil();
        for (c, b) in zip(_gm, _bn) {
            let cb = c
                .append(RcDoc::as_string("{"))
                .append(RcDoc::hardline())
                .append(b)
                .append(RcDoc::as_string("}"))
                .append(RcDoc::hardline());
            __n = __n.append(cb);
        }

        // XXX: Flatten all returns into a VecDeque
        let _rn = _rn
            .into_iter()
            .flatten()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<VecDeque<_>>();
        return (__n.append(_n), _rn);
    }

    let mut _rets = _rets;
    let inode = _rets.pop_front().unwrap();
    // XXX: Return if you have already generated code for this node.
    // Possible in loops.
    if _done_nodes.iter().find(|&&x| x == inode).is_some() {
        return (_n, _rets, inode);
    }
    // XXX: Continue only if this node has not already been done.
    let (_n, _rets) = _gen_code(
        _f,
        _l,
        _rets,
        _nodes,
        _n,
        inode,
        _nodes[inode]._tid,
        _used_sigs_per_thread,
        0, // this indicates if it is the very
        // first call to gen_code. Needed for traversing
        // the graph from pause states.
        _sigs_map_per_threads,
    );
    // XXX: Here we need to put it inside the method!
    let __n = RcDoc::<()>::as_string(format!(
        "constexpr void Thread{}<{}>::tick({}) {{",
        _nodes[inode]._tid, _nodes[inode].label, _used_sigs_per_thread[_nodes[inode]._tid]
    ));
    // XXX: Here we close the method
    let __n = __n
        .append(RcDoc::hardline())
        .append(_n)
        .append(RcDoc::hardline())
        .append(RcDoc::as_string("}"))
        .append(RcDoc::hardline());
    return (__n, _rets, inode);
}

fn _make_fsm_code<'a>(
    _i: usize,
    _e: usize,
    _nodes: &'a [GraphNode],
    _nthreads: usize,
    _used_sigs_per_thread: &'a [String],
    _sigs_map_per_threads: Vec<HashMap<&str, usize>>,
) -> RcDoc<'a> {
    // XXX: Walk graph and generate code
    let mut rets: VecDeque<usize> = VecDeque::with_capacity(_nodes.len());
    let mut _n = RcDoc::<()>::hardline();
    let mut _res: Vec<RcDoc> = Vec::with_capacity(_nodes.len());
    let mut _done_nodes: Vec<usize> = Vec::with_capacity(_nodes.len());
    let mut _inode = 0usize;
    rets.push_back(_i);
    while !rets.is_empty() {
        (_n, rets, _inode) = _walk_graph_code_gen(
            _i,
            _e,
            rets,
            _nodes,
            RcDoc::<()>::nil(),
            _used_sigs_per_thread,
            &_done_nodes,
            &_sigs_map_per_threads,
        );
        _res.push(_n);
        _done_nodes.push(_inode);
    }
    return _res.into_iter().fold(RcDoc::nil(), |acc, x| acc.append(x));
}
