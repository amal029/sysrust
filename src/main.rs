use analyse::{get_num_threads, get_states};
use error::print_bytes;
use rewrite::NodeT;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::Write;
use std::process::{exit, Command};
use sysrust::ast::CallNameType;
use sysrust::{ast, parse};

use crate::analyse::{
    _analyse_var_signal_uses, _type_infer_extern_calls, get_s_v_ref, get_signals, get_vars,
};
use crate::rewrite::{rewrite_to_graph_fsm, GraphNode};
mod analyse;
mod backend;
mod error;
mod rewrite;

fn main() {
    let args: Vec<String> = env::args().collect();
    let _ast = parse(&args[1]);

    // XXX: Analyse signa/var declaration and their uses
    let mut stack: Vec<HashMap<String, (ast::Type, analyse::SignalVarType, Option<ast::IO>)>> =
        Vec::with_capacity(50);

    // XXX: The errors in the program are collected here.
    let mut rets: Vec<(usize, usize, String)> = Vec::with_capacity(50);

    // XXX: Check the usage and declaration of signals and variables
    let tid = 0;
    stack.push(HashMap::with_capacity(50)); // pushed the first hashmap
    stack = _analyse_var_signal_uses(&args[1], &_ast, stack, &mut rets, tid);
    stack.pop(); // removed the final hashmap

    // XXX: Print all the errors
    let bb = rets.is_empty();
    for i in rets {
        print_bytes(&args[1], i.0, i.1).unwrap();
        println!("{} ", i.2);
    }
    if !bb {
        exit(1);
    }

    // XXX: Get all the threads in the program
    let mut num_threads = 1usize;
    get_num_threads(&mut num_threads, &_ast);
    // println!("Num of threads in the program: {}", num_threads);

    // XXX: Get all the states in each thread
    let mut _states: Vec<Vec<ast::Symbol>> = vec![vec![]; num_threads];
    let mut tid = 0;
    let mut tot = 1;
    get_states(&mut _states, &_ast, &mut tid, &mut tot);
    // println!("{:?}", _states);

    // XXX: Get all the signals in each thread
    let mut tid = 0;
    let mut tot = 1;
    let mut _signals: Vec<Vec<ast::Stmt>> = vec![vec![]; num_threads];
    get_signals(&mut _signals, &_ast, &mut tid, &mut tot);
    // println!("{:?}", _signals);

    // XXX: Get all the vars in each thread
    let mut tid = 0;
    let mut tot = 1;
    let mut _vars: Vec<Vec<ast::Stmt>> = vec![vec![]; num_threads];
    get_vars(&mut _vars, &_ast, &mut tid, &mut tot);
    // println!("{:?}", _vars);

    // XXX: Type inference for extern calls to C
    let mut _extern_calls: Vec<CallNameType> = Vec::with_capacity(50);
    let __signals = _signals.iter().flatten().collect::<Vec<_>>();
    let __vars = _vars.iter().flatten().collect::<Vec<_>>();
    _type_infer_extern_calls(&__signals, &__vars, &_ast, &mut _extern_calls, &args[1]);
    let _extern_calls = _extern_calls
        .into_iter()
        .collect::<HashSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();
    let mut uniq: HashSet<&str> = HashSet::new();
    for _i in _extern_calls.iter().map(|x| x._sy.as_str()) {
        if uniq.contains(_i) {
            println!(
                "Function overloading in C is not allowed. \
		 External C function \"{}\" with different signatures!",
                _i
            );
        }
        uniq.insert(_i);
    }
    // println!("Inferred extern functions: {:?}", _extern_calls);

    // XXX: Get all the signal and var reference in each thread
    let mut _sref: Vec<Vec<ast::SimpleDataExpr>> = vec![vec![]; num_threads];
    let mut _vref: Vec<Vec<ast::SimpleDataExpr>> = vec![vec![]; num_threads];
    let mut _syref: Vec<Vec<ast::Symbol>> = vec![vec![]; num_threads];
    let mut _vyref: Vec<Vec<ast::Symbol>> = vec![vec![]; num_threads];
    let mut tid = 0;
    let mut tot = 1;
    get_s_v_ref(
        &mut _sref,
        &mut _syref,
        &mut _vref,
        &mut _vyref,
        &_ast,
        &mut tid,
        &mut tot,
    );
    // TODO: Remove duplicate elements from the vec of vecs.
    // println!("{:?} {:?} {:?} {:?}", _syref, _sref, _vyref, _vref);

    // XXX: Make the FSM graph
    let mut _nodes: Vec<GraphNode> = Vec::with_capacity(50);
    let mut idx = 0usize;
    let mut tid = 0;
    let mut tot = 1;
    let mut _tidxs: Vec<(usize, usize)> = Vec::with_capacity(50);
    let (_i, _e) = rewrite_to_graph_fsm(
        &args[1],
        &_ast,
        &mut tid,
        &mut tot,
        &mut idx,
        &mut _nodes,
        &mut _tidxs,
    );
    // XXX: Make the label for _i and _e
    let _ = match _nodes[_i].tt {
        NodeT::PauseStart => panic!("Please add a nothing before the first pause in the program"),
        _ => (),
    };
    // XXX: Fix labels for _tidxs
    _tidxs.iter().for_each(|(i, e)| {
        match _nodes[*i].tt {
            NodeT::PauseStart => panic!("Please put a nothing at start of each thread"),
            _ => _nodes[*i].label = String::from("I"),
        }
        let _etid = _nodes[*e]._tid;
        // XXX: Check if any child node has _tid == _etid
        if _nodes[*e].children.iter().all(|x| _nodes[*x]._tid != _etid) {
            _nodes[*e].label = String::from("E");
            _nodes[*e].tag = true;
        }
    });
    _nodes[_i].label = String::from("I");
    // XXX: Only make the end node if it has no children -- a loop
    if _nodes[_e].children.is_empty() {
        _nodes[_e].label = String::from("E");
        _nodes[_e].tag = true;
    }
    // println!("{_i} {_e}");
    dbg!("{:?} {:?} {:?}", &_nodes, _i, _e);

    // XXX: Now start making the backend
    let ff = args[1].split('.').collect::<Vec<&str>>()[0];
    let _fname = format!("{}.{}", ff, "cpp");
    let mut _file = File::create(&_fname).expect("Cannot create the cpp file");

    // XXX: First make the prolouge -- includes, threads, states,
    // signals, and vars
    let _ftowrite = &backend::_codegen(
        &_signals,
        &_vars,
        &num_threads,
        &_states,
        &_syref,
        &_sref,
        &_vyref,
        &_vref,
        &args[1],
        // XXX: This is for external function in C
        &_extern_calls,
        // XXX: These are generating the actual code
        _i,
        _e,
        &_nodes,
        // XXX: These are the other threads in the program
        _tidxs,
    );
    // XXX: Make all othre thread code as well.
    _file
        .write_all(&_ftowrite)
        .expect("Cannot write to cpp file");

    // XXX: Format the generated Cpp file using clang-format
    let _clang_bin = Command::new("which")
        .arg("clang-format")
        .output()
        .expect("Could not get path to clang-format");
    let _clang_bin_status = _clang_bin.status;
    if _clang_bin_status.success() {
        let _clang_bin_path = _clang_bin
            .stdout
            .into_iter()
            .map(|x| x as char)
            .collect::<String>();
        let _clang_bin_path = _clang_bin_path.strip_suffix("\n").unwrap();
        let _fmt_res = Command::new(_clang_bin_path)
            .arg("-i")
            .arg(_fname)
            .output()
            .expect("failed to format the cpp file");
    } else {
        println!("Could not find clang-format, the generated cpp file will not be formatted");
    }
}
