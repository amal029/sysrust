use std::{
    collections::{HashMap, HashSet, VecDeque},
    iter::zip,
};

use itertools::join;
use pretty::RcDoc;
use sysrust::ast::{CallNameType, ExprOp, SimpleDataExpr, Stmt, Symbol, Type};

use crate::{
    error::print_bytes,
    rewrite::{GraphNode, NodeT},
};

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
            let _m = format!("struct signal_{}", _sy.get_string());
            let _m = format!("{} {{bool status;}};", _m);
            let _a = RcDoc::<()>::as_string(_m).append(RcDoc::hardline());
            let sname = _sy.get_string();
            let u = format!("static signal_{} {}_curr, {}_prev;", sname, sname, sname);
            _a.append(RcDoc::as_string(u)).append(RcDoc::hardline())
        }
        Stmt::DataSignal(_sy, _io, _ty, _iv, _op, _pos) => {
            let _m = format!("struct signal_{}", _sy.get_string());
            let _m = format!(
                "{} {{bool status; {} value = {}; {}<{}> op {{}};}};",
                _m,
                _type_string(_ty, *_pos, _ff),
                _iv.to_string(),
                _expr_op_std_op(_op, *_pos, _ff),
                _type_string(_ty, *_pos, _ff)
            );
            let a = RcDoc::<()>::as_string(_m).append(RcDoc::hardline());
            let sname = _sy.get_string();
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
                _sy.get_string(),
                _tid,
                _iv.to_string(),
            );
            RcDoc::<()>::as_string(_m).append(RcDoc::hardline())
        }
        _ => panic!("Got a non variable when generating C++ backend"),
    }
}

fn _get_unique_set(st: &[Vec<Symbol>]) -> Vec<HashSet<&String>> {
    st.iter()
        .map(|x| x.iter().map(|y| y.get_string()).collect::<HashSet<_>>())
        .collect()
}

fn _get_unique_set_sexpr(st: &[Vec<SimpleDataExpr>]) -> Vec<HashSet<&String>> {
    st.iter()
        .map(|i| {
            i.iter()
                .map(|j| match j {
                    SimpleDataExpr::SignalRef(_sy, _) => _sy.get_string(),
                    SimpleDataExpr::VarRef(_sy, _) => _sy.get_string(),
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
    _tidxs: Vec<(usize, usize)>,
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
    _m_header.render(8, &mut w).expect("Cannot write signals");
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
            let k = format!("struct {} : State {{}};", _j.get_string());
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
            .map(|x| format!("Thread{}<{}>", _k, x.get_string()))
            .collect();
        _vv.push(format!("Thread{}<I>", _k));
        _vv.push(format!("Thread{}<E>", _k));
        // _vv.push(format!("Thread{}<D>", _k));
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
    let mut _for_main: Vec<String> = Vec::new();
    let mut _for_fsm: Vec<Vec<String>> = vec![Vec::new(); *_nthreads];
    let mut _counter = 0;
    for i in zip(_syref, _sref) {
        let mut _sigs_map: HashMap<&str, usize> = HashMap::new();
        let vv = i.0.union(&i.1).collect::<Vec<_>>();
        let v1 = join(
            vv.iter().enumerate().map(|(j, &&x)| {
                _sigs_map.insert(x, j);
                if _counter == 0 {
                    _for_main.push(x.to_string());
                }
                _for_fsm[_counter].push(x.to_string());
                format!("signal_{} &_{}", x, j)
            }),
            ", ",
        );
        _sigs_map_per_thread.push(_sigs_map);
        _used_sigs_vec.push(v1);

        let v2 = join(vv.iter().enumerate().map(|(j, _)| format!("&_{}", j)), ", ");
        _used_sigs_cap.push(v2);

        let v2 = join(vv.iter().enumerate().map(|(j, _)| format!("_{}", j)), ", ");
        _used_sigs_tick.push(v2);

        let vv = join(vv.iter().map(|x| format!("signal_{} &", x)), ", ");
        _used_sigs.push(vv);
        _counter += 1;
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
        // let _ss = format!(
        //     "template <> struct Thread{}<D>{{\nconstexpr void tick \
        // 		  ({});}};",
        //     i, k1
        // );
        // thread_prototypes.push(_ss);
        for j in k2 {
            let mm = j.get_string();
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

    let _threadvar = join(
        (0..*_nthreads).map(|x| format!("static Thread{}State st{};", x, x)),
        "\n ",
    );
    _n = _n
        .append(RcDoc::hardline())
        .append(_threadvar)
        .append(RcDoc::hardline());

    // XXX: Make the initial functions
    let _inits = join(
        (0..*_nthreads).map(|x| {
            format!(
                "constexpr void init{}(){{st{} = Thread{}<I> {{}};}}",
                x, x, x
            )
        }),
        "\n ",
    );
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
    let _hh = join(
        (0..*_nthreads).map(|i| {
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
        }),
        "\n ",
    );
    _n = _n
        .append(RcDoc::hardline())
        .append(RcDoc::as_string(_hh))
        .append(RcDoc::hardline());

    // XXX: The real code from the FSM
    let _nn = _make_fsm_code(
        _ginode, // this is i
        _genode, // this is e
        _gnodes, // this is the adj-list
        *_nthreads,
        &_used_sigs_vec,
        &_sigs_map_per_thread,
        &_for_fsm,
        &_sigs,
    );
    let mut w1: Vec<u8> = Vec::with_capacity(5000);
    let _ = _nn.render(8, &mut w1);

    // XXX: Make all other threads here
    _tidxs.into_iter().for_each(|(_ginode, _genode)| {
        let _nn = _make_fsm_code(
            _ginode, // this is i
            _genode, // this is e
            _gnodes, // this is the adj-list
            *_nthreads,
            &_used_sigs_vec,
            &_sigs_map_per_thread,
            &_for_fsm,
            &_sigs,
        );
        let _ = _nn.render(8, &mut w1);
    });

    // XXX: Now make the main function and input/output functions, which
    // are extern.
    let _main = _make_main_code(_sigs, _for_main);
    let mut w2: Vec<u8> = Vec::with_capacity(5000);
    let _ = _main.render(8, &mut w2);
    let _ = _n.render(8, &mut w);
    w.append(&mut w1);
    w.append(&mut w2);

    return w;
}

fn _make_print_ouputs<'a>(_osigs: Vec<(&'a str, Option<&'a Type>)>) -> RcDoc<'a> {
    let mut _n = RcDoc::<()>::nil();
    for (i, j) in _osigs {
        _n = _n
            .append(format!(
                "std::cout << \"Status of signal {}: \" << {}_curr.status << \"\\n\";",
                i, i
            ))
            .append(RcDoc::hardline());
        if j.is_some() {
            _n = _n
                .append(format!(
                    "std::cout << \"Value of signal {}: \" << {}_curr.value << \"\\n\";",
                    i, i
                ))
                .append(RcDoc::hardline());
        }
    }
    let mut _m = RcDoc::<()>::as_string("void print_outputs(){")
        .append(RcDoc::hardline())
        .append(_n)
        .append("}");
    return _m;
}

fn _make_pre_eq_curr<'a>(_sigs: &'a [Vec<Stmt>]) -> RcDoc<'a> {
    let mut _n = RcDoc::<()>::as_string("void pre_eq_curr(){");
    let _sigs = _sigs.into_iter().flatten().collect::<Vec<_>>();
    for i in _sigs {
        match i {
            Stmt::Signal(_sy, _, _) => {
                _n = _n
                    .append(format!(
                        "{}_prev.status = {}_curr.status;",
                        _sy.get_string(),
                        _sy.get_string()
                    ))
                    .append(RcDoc::hardline())
            }
            Stmt::DataSignal(_sy, _, _, _, _, _) => {
                _n = _n
                    .append(format!(
                        "{}_prev.status = {}_curr.status;",
                        _sy.get_string(),
                        _sy.get_string(),
                    ))
                    .append(RcDoc::hardline())
            }
            _ => panic!("Got a non signal building code for pre <- curr status update"),
        }
    }
    _n = _n.append("}");
    return _n;
}

fn _make_curr_reset<'a>(_sigs: &'a [Vec<Stmt>]) -> RcDoc<'a> {
    let mut _n = RcDoc::<()>::as_string("void reset_curr(){");
    let _sigs = _sigs.into_iter().flatten().collect::<Vec<_>>();
    for i in _sigs {
        match i {
            Stmt::Signal(_sy, _, _) => {
                _n = _n
                    .append(format!("{}_curr.status = false;", _sy.get_string()))
                    .append(RcDoc::hardline())
            }
            Stmt::DataSignal(_sy, _, _, _, _, _) => {
                _n = _n
                    .append(format!("{}_curr.status = false;", _sy.get_string()))
                    .append(RcDoc::hardline())
            }
            _ => panic!("Got a non signal building code for pre <- curr status update"),
        }
    }
    _n = _n.append("}");
    return _n;
}

fn _make_main_code<'a>(_sigs: &'a [Vec<Stmt>], _vsigs: Vec<String>) -> RcDoc<'a> {
    let mut _n = RcDoc::nil();
    let sigs_0 = join(_vsigs.into_iter().map(|x| format!("{}_curr", x)), ", ");
    // XXX: Get all output signals
    let __sigs = _sigs.into_iter().flatten().collect::<Vec<_>>();
    let _osigs = __sigs
        .iter()
        .filter(|x| match x {
            Stmt::Signal(_sy, _io, _pos) => _io.is_some() && _io.clone().unwrap().is_output(),
            Stmt::DataSignal(_sy, _io, _t, _v, _expr, _pos) => {
                _io.is_some() && _io.clone().unwrap().is_output()
            }
            _ => panic!("Got a non signal during output signal code generation"),
        })
        .map(|x| match x {
            Stmt::Signal(_sy, _, _) => (_sy.get_string().as_str(), None),
            Stmt::DataSignal(_sy, _, _t, _, _, _) => (_sy.get_string().as_str(), Some(_t)),
            _ => panic!(),
        })
        .collect::<Vec<_>>();
    // XXX: Get all the input signals
    let _isigs = __sigs
        .iter()
        .filter(|x| match x {
            Stmt::Signal(_sy, _io, _pos) => _io.is_some() && _io.clone().unwrap().is_input(),
            Stmt::DataSignal(_sy, _io, _t, _v, _expr, _pos) => {
                _io.is_some() && _io.clone().unwrap().is_input()
            }
            _ => panic!("Got a non signal during input signal code generation"),
        })
        .map(|x| match x {
            Stmt::Signal(_sy, _, _) => (_sy.get_string().as_str(), None),
            Stmt::DataSignal(_sy, _, _t, _, _, _) => (_sy.get_string().as_str(), Some(_t)),
            _ => panic!("Got a non signal during input signal code generation"),
        })
        .collect::<Vec<_>>();
    let __m: RcDoc<()> = if sigs_0.is_empty() {
        RcDoc::as_string(format!("visit0(st0);"))
    } else {
        RcDoc::as_string(format!("visit0(st0, {});", sigs_0))
    };
    _n = _n
        .append(_make_print_ouputs(_osigs))
        .append(RcDoc::hardline())
        .append(_make_pre_eq_curr(&_sigs))
        .append(RcDoc::hardline())
        .append(_make_curr_reset(&_sigs))
        .append(RcDoc::hardline())
        .append("int main (void){")
        .append(RcDoc::hardline())
        .append("init0();")
        .append(RcDoc::hardline())
        .append("while(1){")
        .append(RcDoc::hardline())
        .append("//read_inputs();")
        .append(RcDoc::hardline())
        .append(__m)
        .append(RcDoc::hardline())
        .append("print_outputs();")
        .append(RcDoc::hardline())
        .append("pre_eq_curr();")
        .append(RcDoc::hardline())
        .append("reset_curr();")
        .append(RcDoc::hardline())
        .append("}")
        .append("}");
    return _n;
}

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
    _for_fsm_sigs_thread: &'a [Vec<String>],
    _all_sigs: &'a [Vec<Stmt>],
) -> (RcDoc<'a>, VecDeque<usize>) {
    // XXX: First != 0 means that we have already reached this node
    // and we want to continue from here
    if _nodes[_i]._tid != _ptid {
        // println!("ptid != tid {_ptid} {:?}", _nodes[_i]._tid);
        // XXX: This means we are outside the previous thread
        let s = format!("st{} = Thread{}<E>{{}};", _ptid, _ptid);
        let mut _n = _n;
        _n = _n.append(RcDoc::as_string(s));
        // XXX: Do not push yourself onto _rets
        return (_n, _rets);
    } else if _nodes[_i].tag && first != 0 {
        // XXX: We have already found where to stop
        if _i != _l {
            // XXX: This must be a pause
            let _join = match _nodes[_i].tt {
                NodeT::PauseStart => false,
                NodeT::SparJoin(_) => true,
                _ => panic!("Got a non pause stop state: {:?}, l: {:?}", _nodes[_i], _l),
            };
            if !_join {
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
            } else {
                // XXX: Join node state.
                let mut _n = _n;
                _n = _n
                    .append(RcDoc::as_string(format!(
                        "st{} = Thread{}<ND>{{}};",
                        _nodes[_i]._tid, _nodes[_i]._tid
                    )))
                    .append(RcDoc::hardline());
                return (_n, _rets);
            }
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

    // XXX: If this is a forkNode then we have to do something different
    let _fork = match _nodes[_i].tt {
        NodeT::SparFork(_) => true,
        _ => false,
    };

    if !_fork {
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
                    _for_fsm_sigs_thread,
                    _all_sigs,
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

        // XXX: Add extra guards if it is a Join node here

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
    } else {
	// FIXME: This can have other children too! -- handle them!
        // XXX: This is for the fork node
        let _jnode_idx = match _nodes[_i].tt {
            NodeT::SparFork(x) => x,
            _ => panic!("Got a non join node when building backend"),
        };

        // 1. Then build the code for init.
        let _ctids = _nodes[_i].children.iter().map(|x| _nodes[*x]._tid);
        let mut _n = _n;
        for i in _ctids {
            _n = _n.append(format!("init{}();", i)).append(RcDoc::hardline());
            // XXX: Here we need the signals used in children threads.
            let _csigs = &_for_fsm_sigs_thread[i];
            for _s in _csigs.iter() {
                _n = _n
                    .append(format!("//Copy of signal {}", _s))
                    .append(RcDoc::hardline());
                _n = _n.append(format!("signal_{} {}_{} = {}_curr;", _s, _s, i, _s));
                _n = _n.append(RcDoc::hardline());
            }
            // XXX: Now make the input signals to visit
            let _vsigs = _csigs
                .iter()
                .enumerate()
                .map(|(_jk, x)| format!("{}_{}", x, i))
                .collect::<Vec<_>>();
            if _vsigs.is_empty() {
                _n = _n
                    .append(format!("visit{}(st{});", i, i))
                    .append(RcDoc::hardline());
            } else {
                _n = _n
                    .append(format!("visit{}(st{}, {});", i, i, _vsigs.join(", ")))
                    .append(RcDoc::hardline());
            }
            // XXX: Update the status of the signal copies being sent!
            for _cs in _csigs {
                _n = _n
                    .append(format!(
                        "{}_curr.status = {}_curr.status || {}_{}.status;",
                        _cs, _cs, _cs, i
                    ))
                    .append(RcDoc::hardline());
            }
            // FIXME: Handle data value for signals here.
            _n = _n
                .append("//FIXME: Still need to handle valued signals correctly")
                .append(RcDoc::hardline());
        }
        // FIXME: Here we need signals that are passed to this thread
        _n = _n
            .append(format!(
                "st{} = Thread{}<ND>{{}};",
                _nodes[_i]._tid, _nodes[_i]._tid
            ))
            .append(RcDoc::hardline());
        let _csigs = &_for_fsm_sigs_thread[_nodes[_i]._tid];
        // XXX: Now call the visit for Done node
        let _vsigs = _csigs
            .into_iter()
            .map(|x| {
                format!(
                    "_{}",
                    _sigs_map_per_threads[_nodes[_i]._tid]
                        .get(&x.as_str())
                        .unwrap()
                )
            })
            .collect::<Vec<_>>();
        if _vsigs.is_empty() {
            _n = _n
                .append(format!("visit{}(st{});", _nodes[_i]._tid, _nodes[_i]._tid))
                .append(RcDoc::hardline());
        } else {
            _n = _n
                .append(format!(
                    "visit{}(st{}, {});",
                    _nodes[_i]._tid,
                    _nodes[_i]._tid,
                    _vsigs.join(", ")
                ))
                .append(RcDoc::hardline());
        }

        // 2. Push the join node into _rets
        let mut _rets = _rets;
        _rets.push_back(_jnode_idx);
        // XXX: Fix this output
        return (_n, _rets);
    }
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
    _for_fsm_sigs_thread: &'a [Vec<String>],
    _all_sigs: &'a [Vec<Stmt>],
) -> (RcDoc<'a>, VecDeque<usize>, usize) {
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
        _for_fsm_sigs_thread,
        _all_sigs,
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
    _sigs_map_per_threads: &'a Vec<HashMap<&str, usize>>,
    _for_fsm_sigs_thread: &'a [Vec<String>],
    _all_sigs: &'a [Vec<Stmt>],
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
            _for_fsm_sigs_thread,
            _all_sigs,
        );
        _res.push(_n);
        _done_nodes.push(_inode);
    }
    return _res.into_iter().fold(RcDoc::nil(), |acc, x| acc.append(x));
}
