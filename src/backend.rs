use std::{
    collections::{HashMap, HashSet, VecDeque},
    iter::zip,
};

use itertools::{join, Itertools};
use pretty::RcDoc;
use sysrust::ast::{ArrayTypeT, CallNameType, ExprOp,
		   SimpleDataExpr, Stmt, StructDef, StructTypeT, Symbol, Type, Val, IO};

type Pos = (usize, usize);

use crate::{
    error::print_bytes,
    rewrite::{GraphNode, NodeT},
};

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
	    match *_s.to_owned() {
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

fn _expr_op_std_op<'a>(_expr: &'a ExprOp, _pos: (usize, usize), _ff: &'a str) ->
    &'a str {
	match _expr {
            ExprOp::Plus => "std::plus",
            ExprOp::Minus => "std::minus",
            ExprOp::Mul => "std::multiplies",
            ExprOp::Div => "std::divides",
            ExprOp::Mod => "std::modulus",
            ExprOp::Pow => {
		let _ = print_bytes(_ff, _pos.0, _pos.1);
		panic!("Power operator not yet supported in C++ backend")
            },
	    ExprOp::RShift => todo!(),
	    ExprOp::LShift => todo!()
	}
    }

fn _sig_decl<'a>(_s: &'a Stmt, _tid: usize, _ff: &'a str) -> RcDoc<'a, ()> {
    fn build_sig(_sy: &Symbol) -> RcDoc {
        let _m = format!("typedef struct signal_{}", _sy.get_string());
        let _m = format!("{} {{bool status = false;}} signal_{};",
			 _m, _sy.get_string());
        let _a = RcDoc::<()>::as_string(_m).append(RcDoc::hardline());
        let sname = _sy.get_string();
        let u = format!("signal_{} {}_curr, {}_prev;", sname, sname, sname);
        _a.append(RcDoc::as_string(u)).append(RcDoc::hardline())
    }
    fn build_data_sig<'a>(
        _sy: &'a Symbol,
        _ff: &'a str,
        _pos: &'a Pos,
        _ty: &'a Type,
        _iv: &'a Val,
        _op: &'a ExprOp,
	_tid: usize,
    ) -> RcDoc<'a> {
	// FIXME: What happens when you have an array type signal?
	let (_1, _2) = _type_string(_ty, *_pos, _ff, _tid);
        let _m = format!("typedef struct signal_{}", _sy.get_string());
        let _m = format!(
            "{} {{{} value {} = {}; {}<{}> op {{}}; \n \
	     //tag is for fresh value updates \
	     \nbool tag = false; bool status = false;}} signal_{};",
            _m,
	    _1,
	    (match _2 {Some(x) => x, None => String::from("")}),
            // _type_string(_ty, *_pos, _ff, _tid),
            _iv.to_string(_tid),
            _expr_op_std_op(_op, *_pos, _ff),
	    _1,
            // _type_string(_ty, *_pos, _ff, _tid),
            _sy.get_string()
        );
        let a = RcDoc::<()>::as_string(_m).append(RcDoc::hardline());
        let sname = _sy.get_string();
        let u = format!("signal_{} {}_curr, {}_prev;", sname, sname, sname);
        a.append(u).append(RcDoc::hardline())
    }
    match _s {
        Stmt::Signal(_sy, _io, _pos) => {
            if let Some(IO::Output) = _io {
                build_sig(_sy)
            } else if let None = _io {
                build_sig(_sy)
            } else {
                RcDoc::nil()
            }
        }
        Stmt::DataSignal(_sy, _io, _ty, _iv, _op, _pos) => {
            if let Some(IO::Output) = _io {
                build_data_sig(_sy, _ff, _pos, _ty, _iv, _op, _tid)
            } else if let None = _io {
                build_data_sig(_sy, _ff, _pos, _ty, _iv, _op, _tid)
            } else {
                RcDoc::nil()
            }
        }
        _ => panic!("Got a non signal when generating C++ backend"),
    }
}

fn _var_decl<'a>(_done:&mut Vec<(&'a str, usize)>,  _var: &'a Stmt, _tid: usize, _ff: &'a str) -> RcDoc<'a, ()> {
    match _var {
        Stmt::Variable(_sy, _ty, _iv, _pos) => {
	    let _dd = _done.iter().find(|(x, y)| (*x == _sy.get_string())
					&& (*y == _tid));
	    match _dd {
		Some(_) => RcDoc::nil(),
		None => {
		    _done.push((_sy.get_string(), _tid));
		    let (_1, _2) = _type_string(_ty, *_pos, _ff, _tid);
		    let _m = format!(
			"static {} {}_{} {} = {};",
			_1,
			// _type_string(_ty, *_pos, _ff, _tid),
			_sy.get_string(),
			_tid,
			(match _2 {Some(x) => x, None => String::from("")}),
			_iv.to_string(_tid),
		    );
		    RcDoc::<()>::as_string(_m).append(RcDoc::hardline())
		}
	    }
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
    _states: &[Vec<(Symbol, Pos)>],
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
    // XXX: These are the nodes with a valid ND state
    _ndtidxs: Vec<usize>,
    _ndtidlabs: Vec<String>,
    // XXX: This is the external header file byte array
    _ext_header: &mut Vec<u8>,
    // XXX: The name of the cpp and header file
    _pfile: &str,
    // XXX: This is telling if a gui is needed
    _gui: Option<bool>,
    // XXX: This is for benchmarking
    _bench: Option<usize>,
    _structs: &[StructDef],
) -> Vec<u8> {
    // XXX: Append #pragma once to the external header file
    let mut _pragma = RcDoc::<()>::as_string("#pragma once").append(
	RcDoc::hardline());
    // XXX: Add the number of threads define in external header
    _pragma = _pragma
        .append(format!("#define NTHREADS {}", _nthreads))
        .append(RcDoc::hardline());
    _pragma = _pragma
        .append(format!("extern long long unsigned _pos[NTHREADS][2];"))
        .append(RcDoc::hardline())
        .append(format!("extern const char* _state[NTHREADS];"))
        .append(RcDoc::hardline());
    // XXX: Adding the definition of struct defs
    let _structdefdoc = RcDoc::concat(_structs.iter().map(|x| x.codegen()));
    _pragma = _pragma.append(RcDoc::as_string("//Struct Defs")).
	append(RcDoc::hardline())
	.append(_structdefdoc).append(RcDoc::hardline());

    // XXX: Write the output
    _pragma.render(8, _ext_header).unwrap();

    let h2 = RcDoc::<()>::as_string("#include <iostream>").append(RcDoc::hardline());
    let h3 = RcDoc::<()>::as_string("#include <variant>").append(RcDoc::hardline());
    let h4 = RcDoc::<()>::as_string("#include <cassert>").append(RcDoc::hardline());
    let h5 = RcDoc::<()>::as_string("#include <functional>").append(
	RcDoc::hardline());
    let h6 = RcDoc::<()>::as_string(format!("#include \"{}.h\"",
					    _pfile)).append(RcDoc::hardline());
    let h7 = if let Some(_) = _bench {
        RcDoc::<()>::as_string("#include <ctime>").append(RcDoc::hardline())
    } else {
        RcDoc::nil()
    };
    let r = h2
        .append(h3)
        .append(h4)
        .append(h5)
        .append(h6)
        .append(RcDoc::hardline())
        .append(h7)
        .append(RcDoc::hardline());

    let mut w = Vec::new();
    r.render(8, &mut w).unwrap();

    // XXX: Extern calls being put here
    let mut _ec = RcDoc::<()>::as_string("extern \"C\"{");
    let _ecs = _ext_call.into_iter().fold(RcDoc::nil(), |acc, _x| {
        let __x = _x._get_doc();
        acc.append(__x)
    });
    _ec = _ec
        .append(RcDoc::as_string("char tick();"))
        .append(RcDoc::hardline());
    _ec = _ec.append(_ecs).append("}").append(RcDoc::hardline());
    _ec = _ec
        .append("long long unsigned _pos[NTHREADS][2];")
        .append(RcDoc::hardline())
        .append("const char * _state[NTHREADS];")
        .append(RcDoc::hardline());
    _ec.render(8, &mut w).unwrap();

    // XXX: Declare all the signals in the program/thread
    let _m_header = RcDoc::<()>::as_string("// Sig decls").append(RcDoc::hardline());
    _m_header.render(8, &mut w).expect("Cannot write signals");
    for (_i, _s) in _sigs.iter().enumerate() {
        for _ss in _s {
            let mut _m = _sig_decl(_ss, _i, _ff).append(RcDoc::hardline());
            let (_k, _k1) = _ss._input_rc_doc(_ff);
            _m = _m.append(_k1).append(RcDoc::hardline());
            _m.render(8, &mut w).expect("Cannot declare signals");
            _k.render(8, _ext_header)
                .expect("Cannot write to external header");
        }
    }

    // XXX: Declare all the variables in each thread
    let _m_header = RcDoc::<()>::as_string("// Var decls").append(RcDoc::hardline());
    let mut donedecs : Vec<(&str, usize)> = vec![];
    _m_header.render(8, &mut w).expect("Cannot write variables");
    for (_i, _s) in _vars.iter().enumerate() {
        for _ss in _s {
            let _m = _var_decl(&mut donedecs, _ss, _i, _ff);
            _m.render(8, &mut w).expect("Cannot declare variable");
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
            let k = format!("struct {} : State {{}};", _j.0.get_string());
            _n = _n.append(RcDoc::as_string(k)).append(RcDoc::hardline());
        }
    }
    let _ = _n.render(8, &mut w);

    // Added the extra not done states
    let mut _n = RcDoc::<()>::line();
    _n = _n.append(RcDoc::as_string("//Extra ND states")).append(RcDoc::hardline());
    for _i in &_ndtidlabs {
        let k = format!("struct {} : State {{}};", _i);
        _n = _n.append(RcDoc::as_string(k)).append(RcDoc::hardline());
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
	// This the zip iterator of _ndtidlabs and _ndtidxs
	let _ntidxl = zip(&_ndtidxs, &_ndtidlabs);

        let mut _vv: Vec<_> = _i
            .iter()
            .map(|x| format!("Thread{}<{}>", _k, x.0.get_string()))
            .collect();
        _vv.push(format!("Thread{}<I>", _k));
        _vv.push(format!("Thread{}<E>", _k));
        _vv.push(format!("Thread{}<ND>", _k));
	// We want to add the extra ND states here if they exist for
	// this thread.
	for (_h, _j) in _ntidxl {
	    if *_h == _k {
		_vv.push(format!("Thread{}<{}>", _k, _j));
	    }
	}
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
            "template <> struct Thread{}<E>{{\ninline  void tick \
	     ({}){{}}}};",
            i, k1
        );
        thread_prototypes.push(_ss);
        let _ss = format!(
            "template <> struct Thread{}<I>{{\ninline  void tick \
			  ({});}};",
            i, k1
        );
        thread_prototypes.push(_ss);
        // XXX: Check if this i is in _ndtidxs
        let _sbr = if _ndtidxs.iter().contains(&i) {
            ";"
        } else {
            "{}"
        };
        let _ss = format!(
            "template <> struct Thread{}<ND>{{\ninline  void tick \
	     ({}){}}};",
	    i, k1, "{}"
            // i, k1, "{throw \"Can never reach here\";}"
        );
        thread_prototypes.push(_ss);
	// Add the extra NDs here
	// This the zip iterator of _ndtidlabs and _ndtidxs
	let _ntidxl = zip(&_ndtidxs, &_ndtidlabs);
	for (_h, _j) in _ntidxl {
	    if *_h == i {
		let _ss = format!("template <> struct Thread{}<{}>\
				   {{\ninline  void tick ({});}};", i, _j, k1);
		thread_prototypes.push(_ss);
	    }
	}
        for j in k2 {
            let mm = j.0.get_string();
            let _ss = format!(
                "template <> struct Thread{}<{}>{{\ninline  void tick \
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
                "static inline __attribute__((always_inline))  \
		 void init{}(){{st{} = Thread{}<I> {{}};}}",
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
    // let _o = "template <class... Ts> struct overloaded: \
    // 	 Ts... {using Ts::operator()...;};"
    //     .to_string();
    // let _oo = "// explicit deduction guide (not needed as of C++20)\n\
    // 	       template<class... Ts> \
    // 	       overloaded(Ts...) -> overloaded<Ts...>;"
    //     .to_string();
    _n = _n
        .append(RcDoc::hardline())
        // .append(_o)
        // .append(RcDoc::hardline())
        // .append(_oo)
        .append(RcDoc::hardline());

    // XXX: All the visits
    assert!(*_nthreads == _used_sigs_vec.len());
    assert!(*_nthreads == _used_sigs_cap.len());
    let _hh = join(
        (0..*_nthreads).map(|i| {
            if _used_sigs_vec[i] != "" {
                format!(
                    "static inline __attribute__((always_inline))  \
		     void visit{}(Thread{}State &&ts, {}){{\
		 std::visit([{}](auto &&t){{return t.tick({});}}, ts);}}",
                    i, i, _used_sigs_vec[i], _used_sigs_cap[i], _used_sigs_tick[i]
                )
            } else {
                format!(
                    "static inline __attribute__((always_inline))  \
		     void visit{}(Thread{}State &&ts{}){{\
		 std::visit([{}](auto &&t){{return t.tick({});}}, ts);}}",
                    i, i, _used_sigs_vec[i], _used_sigs_cap[i], _used_sigs_tick[i]
                )
            }
        }),
        "\n ",
    );
    _n = _n
        .append(RcDoc::hardline())
        .append(RcDoc::as_string(_hh))
        .append(RcDoc::hardline());

    if let None = _bench {
        // XXX: Attach production of position and current state string.
        _n = _n
            .append(RcDoc::hardline())
            .append("// Position from state")
            .append(RcDoc::hardline());
        for (_c, _i) in _states.iter().enumerate() {
            _n = _n
                .append(format!(" bool _state_pos{}(){{", _c))
                .append(RcDoc::hardline());
            for (_sy, _j) in _i {
                _n = _n
                    .append(format!(
                        "if (std::holds_alternative<Thread{}<{}>>(st{})){{",
                        _c,
                        _sy.get_string(),
                        _c
                    ))
                    .append(RcDoc::hardline());
                if let Some(_) = _gui {
                    _n = _n
                        .append(format!("_pos[{}][0] = {};", _c, _j.0))
                        .append(RcDoc::hardline());
                    _n = _n
                        .append(format!("_pos[{}][1] = {};", _c, _j.1))
                        .append(RcDoc::hardline());
                }
                _n = _n
                    .append(format!("_state[{}] = \"{}\";", _c, _sy.get_string()))
                    .append(RcDoc::hardline());
                _n = _n.append("return true;").append(RcDoc::hardline());
                _n = _n.append("}").append(RcDoc::hardline());
            }
            // XXX: Attach I, ND, and E states to string too!
            _n = _n
                .append(format!(
                    "if (std::holds_alternative<Thread{}<I>>(st{})) _state[{}]\
		     = \"I\";",
                    _c, _c, _c
                ))
                .append(RcDoc::hardline());
            _n = _n
                .append(format!(
                    "if (std::holds_alternative<Thread{}<E>>(st{})) _state[{}]\
		     = \"E\";",
                    _c, _c, _c
                ))
                .append(RcDoc::hardline());
            _n = _n
                .append(format!(
                    "if (std::holds_alternative<Thread{}<ND>>(st{})) _state[{}]\
		     = \"ND\";",
                    _c, _c, _c
                ))
                .append(RcDoc::hardline());
	    
	    // Add extra ND nodes too
	    let _ntidxl = zip(&_ndtidxs, &_ndtidlabs);
	    for (_h, _j) in _ntidxl {
		if *_h == _c {
		    _n = _n.append(format!(
			"if (std::holds_alternative<Thread{}<{}>>(st{})) _state[{}]\
			 = \"{}\";",
			_c, _j, _c, _c, _j
                    ))
		}
	    }
            _n = _n.append("return false;").append(RcDoc::hardline());
            _n = _n.append("}").append(RcDoc::hardline());
        }
    }

    // XXX: The real code from the FSM
    let _nn = _make_fsm_code(
        _ginode, // this is i
        _genode, // this is e
        _gnodes, // this is the adj-list
        *_nthreads,
        &_used_sigs_vec,
        &_sigs_map_per_thread,
        &_for_fsm,
        _sigs,
	&_ndtidxs,
	&_ndtidlabs
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
            _sigs,
	    &_ndtidxs,
	    &_ndtidlabs
        );
        let _ = _nn.render(8, &mut w1);
    });

    // XXX: Now make the main function and input/output functions, which
    // are extern.
    let _main = _make_main_code(_sigs, _for_main, *_nthreads, &_gui, &_bench);
    let mut w2: Vec<u8> = Vec::with_capacity(5000);
    let _ = _main.render(8, &mut w2);
    let _ = _n.render(8, &mut w);
    w.append(&mut w1);
    w.append(&mut w2);

    w
}

fn _make_print_ouputs<'a>(
    _osigs: Vec<(&'a str, Option<&'a Type>)>,
    _bench: &Option<usize>,
) -> RcDoc<'a> {
    if let None = _bench {
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
            .append("std::cout << \"----------------\\n\";")
            .append(RcDoc::hardline())
            .append("}");
        _m
    } else {
        RcDoc::nil()
    }
}

fn _make_pre_eq_curr(_sigs: &[Vec<Stmt>]) -> RcDoc {
    let mut _n = RcDoc::<()>::as_string("void pre_eq_curr(){");
    let _sigs = _sigs.iter().flatten().collect::<Vec<_>>();
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
                    .append(format!(
                        "{}_prev.value = {}_curr.value;",
                        _sy.get_string(),
                        _sy.get_string(),
                    ))
            }
            _ => panic!("Got a non signal building code for pre <- curr status update"),
        }
    }
    _n = _n.append("}");
    _n
}

fn _make_curr_reset(_sigs: &[Vec<Stmt>]) -> RcDoc {
    let mut _n = RcDoc::<()>::as_string("void reset_curr(){");
    let _sigs = _sigs.iter().flatten().collect::<Vec<_>>();
    for i in _sigs {
        match i {
            Stmt::Signal(_sy, _, _) => {
                _n = _n
                    .append(format!("{}_curr.status = false;", _sy.get_string()))
                    .append(RcDoc::hardline())
            }
            Stmt::DataSignal(_sy, _io, _, _, _, _) => {
                if let Some(IO::Output) = _io {
                    _n = _n
                        .append(format!(
                            "{}_curr.status = false; {}_curr.tag = false;",
                            _sy.get_string(),
                            _sy.get_string()
                        ))
                        .append(RcDoc::hardline())
                } else {
                    _n = _n
                        .append(format!("{}_curr.status = false; {}_curr.tag = false;"
					, _sy.get_string(), _sy.get_string()))
                        .append(RcDoc::hardline())
                }
            }
            _ => panic!("Got a non signal building code for pre <- curr status update"),
        }
    }
    _n = _n.append("}");
    _n
}

fn _make_main_code<'a>(
    _sigs: &'a [Vec<Stmt>],
    _vsigs: Vec<String>,
    _nthreads: usize,
    _gui: &'a Option<bool>,
    _bench: &'a Option<usize>,
) -> RcDoc<'a> {
    let mut _n = RcDoc::nil();
    let sigs_0 = join(_vsigs.into_iter().map(|x| format!("{}_curr", x)), ", ");
    // XXX: Get all output signals
    let __sigs = _sigs.iter().flatten().collect::<Vec<_>>();
    let _osigs = __sigs
        .iter()
        .filter(|x| match x {
            Stmt::Signal(_sy, _io, _pos) => {
                _io.is_none() || (_io.is_some() && _io.clone().unwrap().is_output())
            }
            Stmt::DataSignal(_sy, _io, _t, _v, _expr, _pos) => {
                _io.is_none() || (_io.is_some() && _io.clone().unwrap().is_output())
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
        RcDoc::as_string("visit0(std::move(st0));".to_string())
    } else {
        RcDoc::as_string(format!("visit0(std::move(st0), {});", sigs_0))
    };
    let __st = if let None = _bench {
        RcDoc::<()>::as_string(join(
        (0.._nthreads).into_iter().map(|x| {
            format!(
                "bool _res{} = _state_pos{}(); std::cout << \"Thread{} in state: \"<< _state[{}] << \"\\n\";",
                x, x, x, x
            )
        }),
        "\n",
    )).append(RcDoc::hardline()).append("print_outputs();")
    } else {
        RcDoc::nil()
    };
    // XXX: The loop counter for bench
    let _count = if let None = _bench {
        RcDoc::<()>::as_string("while(1){")
    } else {
        let _c = _bench.unwrap();
        RcDoc::as_string("unsigned long long counter = 0;")
            .append(RcDoc::hardline())
            .append("std::time_t _start = std::time(nullptr);")
            .append(RcDoc::hardline())
            .append(format!("while (counter++ < {_c}){{"))
    };
    let _tick = if let None = _bench {
        RcDoc::<()>::as_string("if (tick() == 'd') break;")
    } else {
        RcDoc::nil()
    };
    let _tend = if let Some(_) = _bench {
        RcDoc::<()>::hardline()
            .append("std::time_t _end = std::time(nullptr);")
            .append(RcDoc::hardline())
            .append(format!(
                "std::cout << std::difftime(_end, _start) << \"(sec)\\n\";"
            ))
            .append(RcDoc::hardline())
    } else {
        RcDoc::nil()
    };
    _n = _n
        .append(_make_print_ouputs(_osigs, _bench))
        .append(RcDoc::hardline())
        .append(_make_pre_eq_curr(_sigs))
        .append(RcDoc::hardline())
        .append(_make_curr_reset(_sigs))
        .append(RcDoc::hardline())
        .append("int main (void){")
        .append(RcDoc::hardline())
        .append("init0();")
        .append(RcDoc::hardline())
        .append(_count)
        // .append("int counter = 0;")
        // .append(RcDoc::hardline())
        // .append("while(1){")
        .append(RcDoc::hardline())
        .append("//read_inputs();")
        .append(RcDoc::hardline())
        .append(__m)
        .append(RcDoc::hardline())
        // XXX: Add the call to _state_pos
        .append(__st)
        // .append("print_outputs();")
        .append(RcDoc::hardline())
        .append("pre_eq_curr();")
        .append(RcDoc::hardline())
        .append("reset_curr();")
        .append(RcDoc::hardline())
        .append(_tick)
        // .append("if (tick() == 'd') break;")
        .append(RcDoc::hardline())
        .append("}")
        // XXX: here we add the printf for benchmark
        .append(_tend)
        .append("}");
    _n
}

fn _make_seq_code<'a>(
    _f: usize,
    _l: usize,
    rets: VecDeque<usize>,
    _nodes: &'a [GraphNode],
    _n: RcDoc<'a>,
    _i: usize,
    _ptid: usize,
    _used_sigs_per_thread: &'a [String],
    first: u8,
    _sigs_map_per_threads: &Vec<HashMap<&str, usize>>,
    _for_fsm_sigs_thread: &'a [Vec<String>],
    _all_sigs: &'a [Vec<Stmt>],
    _same_tid_indices: Vec<usize>,
    _ndtidxs: &Vec<usize>,
    _ndtidlabs: &Vec<String>,
) -> (RcDoc<'a>, VecDeque<usize>) {
    // XXX: First do a map of each child branch
    let _cbm = _nodes[_i]
        .children
        .iter()
        .enumerate()
        .filter_map(|(_j, x)| {
            if _same_tid_indices.iter().find(|&&k| k == _j).is_some() {
                Some(_gen_code(
                    _f,
                    _l,
                    rets.clone(),
                    _nodes,
                    _n.clone(),
                    *x,
                    _nodes[_i]._tid,
                    _used_sigs_per_thread,
                    first + 1,
                    _sigs_map_per_threads,
                    _for_fsm_sigs_thread,
                    _all_sigs,
		    _ndtidxs,
		    _ndtidlabs
                ))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let mut _bn: Vec<RcDoc> = Vec::with_capacity(_cbm.len());
    let mut _rn: Vec<VecDeque<usize>> = Vec::with_capacity(_cbm.len());
    for (i, j) in _cbm {
        _bn.push(i);
        _rn.push(j);
    }

    // XXX: Make the guards for each branch
    let mut _gm = _nodes[_i]
        .guards
        .iter()
        .enumerate()
        .filter_map(|(_j, x)| {
            if _same_tid_indices.iter().find(|&&k| k == _j).is_some() {
                Some(x.codegen(_nodes[_i]._tid,
			       &_sigs_map_per_threads[_nodes[_i]._tid]))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    // XXX: Make the determinism and reactivity assert statements for
    // CPP code.
    let (_det_gm, _reac_gm) = if _gm.len() > 1 {
        (
            Some(RcDoc::intersperse(_gm.clone(), RcDoc::as_string(" and "))),
            Some(RcDoc::intersperse(_gm.clone(), RcDoc::as_string(" or "))),
        )
    } else {
        (None, None)
    };

    // XXX: Call visit for each parent thread if they are not already
    // done, i.e., in the <E> state.
    let _is_join_node = matches!(_nodes[_i].tt, NodeT::SparJoin(_));

    let mut __neidx = usize::MAX;
    if _is_join_node {
        // XXX: Now get all parent thread _tids.
        let _ptids = _nodes[_i]
            .parents
            .iter()
            .filter(|&&_x| _nodes[_x]._tid != _nodes[_i]._tid)
            .map(|&x| _nodes[x]._tid)
            .collect::<Vec<_>>();
        // XXX: Make the guard condition
        let _has_alternative = join(
            _ptids
                .into_iter()
                .map(|x| format!("std::holds_alternative<Thread{}<E>>(st{})", x, x)),
            " and ",
        );
        let _has_alternative: RcDoc<()> =
            RcDoc::as_string("(").append(_has_alternative).append(")");
        let _not_has_alternative = RcDoc::<()>::as_string("(not ")
            .append(_has_alternative.clone())
            .append(")");

        // XXX: Check that the first node is always the self-loop
        assert!(!_nodes[_i].children.is_empty());
        assert!(_i == _nodes[_i].children[0]);

        // XXX: Check one is always the normal JoinChild
        let _normal_exit_idx = _nodes[_i]
            .children
            .iter()
            .enumerate()
            .filter_map(|(_k, &x)| {
                if let NodeT::JoinChild(y) = _nodes[x].tt {
                    if y == _i {
                        Some(_k)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        // XXX: There should be only 1 such normal exit child!
        let mut _neidx = usize::MAX;
        assert!(_normal_exit_idx.len() == 1);
        for i in _normal_exit_idx {
            _neidx = i;
        }

        // XXX: Copy the _neidx to __neidx to attach actions in _bn
        // later on for abort, etc.
        __neidx = _neidx;

        // XXX: Now attach the _has_alternative to each of the children
        // accordingly.
        _gm = _gm
            .into_iter()
            .enumerate()
            .map(|(_k, x)| {
                if _k == 0 {
                    RcDoc::<()>::as_string("(")
                        .append(x)
                        .append(" and ")
                        .append(_not_has_alternative.clone())
                        .append(")")
                } else if _k == _neidx {
                    RcDoc::<()>::as_string("(")
                        .append(x)
                        .append(" and ")
                        .append(_has_alternative.clone())
                        .append(")")
                } else {
                    x
                }
            })
            .collect::<Vec<_>>();
        // println!(" Join node {} {:?}", _nodes[_i]._tid, _gm);
    }

    // XXX: Make the actions for each branch
    let mut _am = _nodes[_i]
        .actions
        .iter()
        .enumerate()
        .filter_map(|(_j, x)| {
            if _same_tid_indices.iter().find(|&&k| k == _j).is_some() {
                Some(x.codegen(_nodes[_i]._tid, &_sigs_map_per_threads[_nodes[_i]._tid]))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    let mut __vp = RcDoc::<()>::nil();
    if _is_join_node {
        // XXX: Make the action statements to visit parent threads and
        // attach it to _am[0].
        // XXX: Now get all parent thread _tids.
        let _ptids = _nodes[_i]
            .parents
            .iter()
            .filter(|&&_x| _nodes[_x]._tid != _nodes[_i]._tid)
            .map(|&x| _nodes[x]._tid);
        let mut _vp = RcDoc::<()>::nil();
        // let mut _avp = RcDoc::as_string("//Parent threads done?").append(RcDoc::hardline());
        for i in _ptids {
            _vp = _vp.append(format!(
                "if (not (std::holds_alternative<Thread{}<E>>(st{}))){{",
                i, i
            ));
            let _m = _make_stmts_for_fork_join(_nodes,
					       _for_fsm_sigs_thread, i, _all_sigs,
					       _ndtidxs, _ndtidlabs);
            _vp = _vp.append(_m);
            // _avp = _avp
            //     .append(format!(
            //         "assert(std::holds_alternative<Thread{}<E>>(st{}));",
            //         i, i
            //     ))
            //     .append(RcDoc::hardline());
        }

        // XXX: This is needed for later attachment to _bn for abort,
        // etc statements.
        // __vp = _vp.clone().append(_avp);

        // XXX: Add _vp to _am[0] and also to all non normal exit nodes.
        if _am.is_empty() {
            _am.push(_vp);
        } else {
            _am = _am
                .into_iter()
                .enumerate()
            // XXX: Add to child 0 and any other child that is not _neidx
                .map(|(_j, x)| if _j == 0 { x.append(_vp.clone()) } else { x })
                .collect::<Vec<_>>();
        }
    }

    // XXX: Confirm we have a guard for each branch
    assert!(_gm.len() == _bn.len());
    // XXX: Confirm that we have an action on outgoing branch of join
    // node.
    assert!(_am.len() <= _bn.len());

    // XXX: First combine the actions with _bn
    let _bn = _bn
        .into_iter()
        .enumerate()
        .map(|(i, x)| {
            if i < _am.len() && !_am.is_empty() {
                _am[i].clone().append(x)
            } else if i != __neidx && _is_join_node {
                // XXX: This is the case when we have abort, etc and we
                // want to make sure that all children are in the exit
                // status before this thread exits.
                __vp.clone().append(x)
            } else {
                x
            }
        })
        .collect::<Vec<_>>();

    let _gml = _gm.len();
    let _gm = _gm.into_iter().map(|x| {
        // XXX: Remove the conditional if it is just "true"
        let vvm = if _gml == 1 {
            let vvm = format!("{:?}", x);
            vvm == String::from("\"true\"")
        } else {
            false
        };
        if !vvm {
            (
                RcDoc::as_string("if(")
                    .append(x)
                    .append(RcDoc::as_string(")")),
                true,
            )
        } else {
            (RcDoc::nil(), false)
        }
    });
    let mut __n = RcDoc::nil();
    // FIXME: This should remove all the trues
    // if let (Some(_x), Some(_y)) = (_det_gm, _reac_gm) {
    //     __n = __n
    //         .append(RcDoc::as_string("assert(("))
    //         .append(_x)
    //         .append(") == false && \"Non deterministic program\" );")
    //         .append(RcDoc::hardline());
    //     __n = __n
    //         .append(RcDoc::as_string("assert(("))
    //         .append(_y)
    //         .append(") == true && \"Non reactive program\");")
    //         .append(RcDoc::hardline());
    // }
    for (c, b) in zip(_gm, _bn) {
        // XXX: Remove the extra braces and line if there is no
        // conditional from above removal of if(true)
        let _bs: RcDoc<'_, _> = if c.1 {
            RcDoc::<()>::as_string("{").append(RcDoc::hardline())
        } else {
            RcDoc::nil()
        };
        let be: RcDoc<'_, _> = if c.1 {
            RcDoc::<()>::as_string("}").append(RcDoc::hardline())
        } else {
            RcDoc::nil()
        };
        let cb = c.0.append(_bs).append(b).append(be);
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

fn _make_stmts_for_fork_join<'a>(
    _nodes: &'a [GraphNode],
    _for_fsm_sigs_thread: &'a [Vec<String>],
    i: usize,
    _all_sigs: &'a [Vec<Stmt>],
    _ndtidxs: &Vec<usize>,
    _ndtidlabs: &Vec<String>,
) -> RcDoc<'a> {
    let mut _vp = RcDoc::<()>::nil();
    let _csigs = &_for_fsm_sigs_thread[i];
    for _s in _csigs.iter() {
        _vp = _vp
            .append(RcDoc::hardline())
            .append(format!("//Copy of signal {}", _s))
            .append(RcDoc::hardline());
        // _vp = _vp.append(format!("signal_{} {}_{} = {}_curr;", _s, _s, i, _s));
	_vp = _vp.append(format!("signal_{} {}_{};", _s, _s, i));
        _vp = _vp.append(RcDoc::hardline());
    }
    // XXX: Now make the input signals to visit
    let _vsigs = _csigs
        .iter()
        .enumerate()
        .map(|(_jk, x)| format!("{}_{}", x, i))
        .collect::<Vec<_>>();
    if _vsigs.is_empty() {
        _vp = _vp
            .append(format!("visit{}(std::move(st{}));", i, i))
            .append(RcDoc::hardline());
    } else {
        _vp = _vp
            .append(format!("visit{}(std::move(st{}), {});", i, i, _vsigs.join(", ")))
            .append(RcDoc::hardline());
    }
    // XXX: Update the status of the signal copies being sent!
    for _cs in _csigs {
        _vp = _vp
            .append(format!(
                "{}_curr.status = {}_curr.status || {}_{}.status;",
                _cs, _cs, _cs, i
            ))
            .append(RcDoc::hardline());
    }
    // XXX: Handle data value for signals here.
    let _dsigs = _all_sigs
        .into_iter()
        .flatten()
        .filter_map(|x| match x {
            Stmt::DataSignal(_sy, _, _, _, _, _pos) => {
                if _csigs.contains(_sy.get_string()) {
                    Some(_sy.get_string())
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    for _cs in _csigs {
        if _dsigs.contains(&_cs) {
	    // XXX: This ND fixes many not done nodes occuring in
	    // sequence with || with other NDs Make a vector of
	    // holds_alternative<...> for each ND in this thread.
	    let mut holdvec = Vec::with_capacity(10);
	    holdvec.push(format!("std::holds_alternative<Thread{i}<ND>>(st{i})"));
	    // holdvec.push(format!("false"));
	    let _ntidxl = zip(_ndtidxs.clone(), _ndtidlabs.clone());
	    for (_h, _j) in _ntidxl {
		if _h == i {
		    holdvec.push(
			format!("std::holds_alternative<Thread{i}<{}>>(st{i})",
				_j));
		}
	    }
	    let _holds = holdvec.join("||");
	    // not (std::holds_alternative<Thread{i}<ND>>(st{i}))) {{\
            _vp = _vp
                .append(format!(
                    "if ({_cs}_{i}.status and \
		     not ({_holds})) {{\
		     if({_cs}_curr.tag){{
			 {_cs}_curr.value = \
			 {_cs}_curr.op({_cs}_curr.value, {_cs}_{i}.value);}} 
			 else {{ {_cs}_curr.value = \
			 {_cs}_{i}.value; {_cs}_curr.tag = true; }} }}"
                ))
                .append(RcDoc::hardline());
        }
    }
    _vp = _vp
        .append(RcDoc::hardline())
        .append("}")
        .append(RcDoc::hardline());
    _vp
}

fn _make_fork_code<'a>(
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
    _ndtidxs: &Vec<usize>,
    _ndtidlabs: &Vec<String>

) -> (RcDoc<'a>, VecDeque<usize>) {
    // XXX: This is for the fork node
    let _jnode_idx = match _nodes[_i].tt {
        NodeT::SparFork(x) => x,
        _ => panic!("Got a non join node when building backend"),
    };

    // XXX: First make sure that the number of guards == number of children
    assert!(_nodes[_i].children.len() == _nodes[_i].guards.len());

    let mut _n = _n;
    let mut _gnn: Vec<RcDoc<()>> = Vec::with_capacity(_nodes[_i].guards.len());
    let mut _same_tid_indices: Vec<usize> =
	Vec::with_capacity(_nodes[_i].guards.len());
    let mut _other_tids: Vec<usize> = Vec::with_capacity(_nodes[_i].guards.len());
    for (j, &c) in _nodes[_i].children.iter().enumerate() {
        let i = _nodes[c]._tid; // The child thread id
        if _nodes[c]._tid != _nodes[_i]._tid {
            _other_tids.push(_nodes[c]._tid);
            // println!("child index: is: {j}, guard: {:?}", _nodes[_i].guards[j]);
            let _gn = _nodes[_i].guards[j]
                .codegen(_nodes[_i]._tid, &_sigs_map_per_threads[_nodes[_i]._tid]);
            _gnn.push(_gn.clone());
            let _ifgn = RcDoc::<()>::as_string("if(")
                .append(_gn)
                .append(")")
                .append("{");
            _n = _n.append(_ifgn);
            // 1. Then build the code for init.
            _n = _n.append(format!("init{}();", i));
            let _m = _make_stmts_for_fork_join(_nodes,
					       _for_fsm_sigs_thread, i, _all_sigs,
					       _ndtidxs, _ndtidlabs);
            _n = _n.append(_m);
        } else {
            // XXX: This is when a child is in the same _tid.
            _same_tid_indices.push(j);
        }
    }
    // XXX: This is calling the join node for ticking this
    // thread.
    let _mifgm = RcDoc::intersperse(_gnn, RcDoc::as_string(" and "));
    let _holds_alternative = "(".to_owned()
        + &join(
            _other_tids
                .into_iter()
                .map(|x| format!("std::holds_alternative<Thread{}<E>>(st{})", x, x)),
            "and ",
        )
        + ")";
    let _csigs = &_for_fsm_sigs_thread[_nodes[_i]._tid];

    // XXX: Now call the visit for Done node
    let _vsigs = _csigs
        .iter()
        .map(|x| {
            format!(
                "_{}",
                _sigs_map_per_threads[_nodes[_i]._tid]
                    .get(&x.as_str())
                    .unwrap()
            )
        })
        .collect::<Vec<_>>();
    let _nj = if _vsigs.is_empty() {
        format!("visit{}(std::move(st{}));", _nodes[_i]._tid, _nodes[_i]._tid)
    } else {
        format!(
            "visit{}(std::move(st{}), {});",
            _nodes[_i]._tid,
            _nodes[_i]._tid,
            _vsigs.join(", ")
        )
    };

    _n = _n
        .append("if(")
        .append(_mifgm)
        .append("){")
        .append(RcDoc::hardline())
        .append(format!(
            // XXX: Calling visit this thread when all internal threads
            // are done!
            "if({}){{ st{} = Thread{}<{}>{{}}; {_nj} }} \
	     else {{ st{} = Thread{}<{}>{{}};}}",
            _holds_alternative, _nodes[_i]._tid, _nodes[_i]._tid,
	    _nodes[_i].label, _nodes[_i]._tid, _nodes[_i]._tid,
	    _nodes[_i].label
        ))
        .append("}")
        .append(RcDoc::hardline());

    // XXX: Now make the sequential node with same _tid -- for example
    // immediate abort
    let (_sn, _srets) = _make_seq_code(
        _f,
        _l,
        _rets,
        _nodes,
        RcDoc::nil(),
        _i,
        _ptid,
        _used_sigs_per_thread,
        first,
        _sigs_map_per_threads,
        _for_fsm_sigs_thread,
        _all_sigs,
        _same_tid_indices,
	_ndtidxs,
	_ndtidlabs
    );

    _n = _n.append(_sn);

    // 2. Push the join node into _rets
    let mut _rets = _srets;
    _rets.push_back(_jnode_idx);

    (_n, _rets)
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
    _ndtidxs: &Vec<usize>,
    _ndtidlabs: &Vec<String>
) -> (RcDoc<'a>, VecDeque<usize>) {
    if _nodes[_i]._tid != _ptid {
        // println!("ptid != tid {_ptid} {:?}", _nodes[_i]._tid);
        // XXX: This means we are outside the previous thread
        let s = format!("st{} = Thread{}<E>{{}};", _ptid, _ptid);
        let mut _n = _n;
        _n = _n.append(RcDoc::as_string(s));
        // XXX: Do not push yourself onto _rets
        return (_n, _rets);
    }
    // XXX: First != 0 means that we have already reached this node
    // and we want to continue from here
    else if _nodes[_i].tag && first != 0 {
        // XXX: We have already found where to stop
        if _i != _l {
            // XXX: This must be a pause
            let _join = match _nodes[_i].tt {
                NodeT::PauseStart => false,
                NodeT::SparJoin(_) => true,
                _ => panic!("Got a non pause stop state: \
			    {:?}, l: {:?}", _nodes[_i], _l),
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
                        "st{} = Thread{}<{}>{{}};",
                        _nodes[_i]._tid, _nodes[_i]._tid, _nodes[_i].label
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
    }

    // XXX: Is this a fork node or just a normal node?
    let _fork = matches!(_nodes[_i].tt, NodeT::SparFork(_));

    if !_fork {
        let _same_tid_indices = (0.._nodes[_i].children.len()).collect_vec();
        _make_seq_code(
            _f,
            _l,
            _rets,
            _nodes,
            _n,
            _i,
            _ptid,
            _used_sigs_per_thread,
            first,
            _sigs_map_per_threads,
            _for_fsm_sigs_thread,
            _all_sigs,
            _same_tid_indices,
	    _ndtidxs,
	    _ndtidlabs
        )
    } else {
        // XXX: This is for the fork node
        _make_fork_code(
            _f,
            _l,
            _rets,
            _nodes,
            _n,
            _i,
            _ptid,
            _used_sigs_per_thread,
            first,
            _sigs_map_per_threads,
            _for_fsm_sigs_thread,
            _all_sigs,
	    _ndtidxs,
	    _ndtidlabs
        )
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
    _ndtidxs: &Vec<usize>,
    _ndtidlabs: &Vec<String>,
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
	_ndtidxs,
	_ndtidlabs
    );
    // XXX: Here we need to put it inside the method!
    // println!("thread id: {:?}, node type: {:?}", _nodes[inode]._tid,
    // 	     _nodes[inode].label);
    let __n = RcDoc::<()>::as_string(format!(
        "inline void Thread{}<{}>::tick({}) {{",
        _nodes[inode]._tid, _nodes[inode].label,
	_used_sigs_per_thread[_nodes[inode]._tid]
    ));
    // XXX: Here we close the method
    let __n =
	RcDoc::as_string("// Methods defined")
	.append(RcDoc::hardline())
	.append(__n)
        .append(RcDoc::hardline())
        .append(_n)
        .append(RcDoc::hardline())
        .append(RcDoc::as_string("}"))
        .append(RcDoc::hardline());
    (__n, _rets, inode)
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
    _ndtidxs: &Vec<usize>,
    _ndtidlabs: &Vec<String>,
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
            _sigs_map_per_threads,
            _for_fsm_sigs_thread,
            _all_sigs,
	    _ndtidxs,
	    _ndtidlabs
        );
        _res.push(_n);
        _done_nodes.push(_inode);
    }
    return _res.into_iter().fold(RcDoc::nil(), |acc, x| acc.append(x));
}
