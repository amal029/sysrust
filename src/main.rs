use analyse::{_check_signal_repeats,
	      _check_state_repeats, get_num_threads, get_states,
	      get_structs, set_parent_tid};
// use error::print_bytes;
use rewrite::NodeT;
// use sysrust::error::print_bytes_warn;
use std::collections::HashSet;
use std::fs::File;
use std::io::Write;
use std::process::{exit, Command};
use sysrust::ast::CallNameType;
use sysrust::{ast, error, parse};

use crate::analyse::{
    // _analyse_var_signal_uses,
    get_externs, get_s_v_ref, get_signals, get_vars
};
use crate::rewrite::{rewrite_to_graph_fsm, GraphNode};
mod analyse;
mod backend;
// mod error;
mod rewrite;

// XXX: Make the clap parser
use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "sysrustc compiler")]
#[command(version = "beta")]
#[command(about = "Compiles .sysrs files to C++ code for execution")]
#[command(long_about=None)]
struct Args {
    /// The .sysrs file to compile to C++-26 compatible code.
    #[arg(short, long)]
    file: Option<String>,
    /// Bind to GUI for interactive simulation.
    #[arg(short, long)]
    _gui: Option<bool>,
    /// For benchmarking the application runtime
    #[arg(short, long)]
    _bench: Option<usize>,
}

fn main() {
    // XXX: Get the arguments from the Arg parser
    let args = Args::parse();

    // XXX: Get the file name
    let file_to_compile = match args.file {
        Some(x) => x,
        None => {
            eprintln!("Try --help for options");
            exit(1);
        }
    };

    let _ast = parse(&file_to_compile);

    // XXX: Get all the threads in the program
    let mut num_threads = 1usize;
    get_num_threads(&mut num_threads, &_ast);
    // println!("Num of threads in the program: {}", num_threads);

    // XXX: Get all the states in each thread
    let mut _states: Vec<Vec<(ast::Symbol, (usize, usize))>>
	= vec![vec![]; num_threads];
    let mut tid = 0;
    let mut tot = 1;
    get_states(&mut _states, &_ast, &mut tid, &mut tot);
    // println!("{:?}", _states);

    // XXX: Get all the signals in each thread
    let mut tid = 0;
    let mut tot = 1;
    let mut _signals: Vec<Vec<ast::Stmt>> = vec![vec![]; num_threads];
    get_signals(&mut _signals, &_ast, &mut tid, &mut tot);

    // Here we collect all the structdefs
    let mut _structs = vec![];
    get_structs(&mut _structs, &_ast);

    // XXX: Check that the signals are not repeated between different
    // concurrent threads
    let _hp = _check_signal_repeats(&_signals, &file_to_compile);

    // XXX: Check that the states and signal do not overlap. Moreover,
    // the states also do not overlap with each other.
    _check_state_repeats(&_states, &file_to_compile, _hp);

    // XXX: Get all the vars in each thread
    let mut tid = 0;
    let mut tot = 1;
    let mut _vars: Vec<Vec<ast::Stmt>> = vec![vec![]; num_threads];
    get_vars(&mut _vars, &_ast, &mut tid, &mut tot);
    // println!("{:?}", _vars);

    //TODO: We should have a vector<tid> for tid for parent threads for
    // each thread.
    let mut tid = 0;
    let mut tot = 1;
    let mut _ptids: Vec<i64> = vec![-1;num_threads];
    set_parent_tid(&mut _ptids, &_ast, &mut tid, &mut tot);
    // println!("parent thread ids: {:?}", _ptids);


    // XXX: Type inference for extern calls to C
    let mut _extern_calls: Vec<CallNameType> = Vec::with_capacity(50);
    get_externs(&mut _extern_calls, &_ast);
    // Get the extern function calls here
    
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
    let mut _tidxs: Vec<(usize, usize)> = Vec::with_capacity(num_threads);
    let mut _ndtidxs: Vec<usize> = Vec::with_capacity(num_threads);
    let mut _ndtidlabs: Vec<String> = Vec::with_capacity(num_threads);
    let (_i, _e) = rewrite_to_graph_fsm(
        &file_to_compile,
        &_ast,
        &mut tid,
        &mut tot,
        &mut idx,
        &mut _nodes,
        &mut _tidxs,
        &mut _ndtidxs,
	&mut _ndtidlabs,
    );
    assert!(_ndtidxs.len() == _ndtidlabs.len(), "number of join threads ids,\
						 and their labels is not equal");
    // println!("{:?}, {:?}", _ndtidlabs, _ndtidxs);
    // XXX: Make the label for _i and _e
    if let NodeT::PauseStart = _nodes[_i].tt {
        panic!("Please add a nothing before the first pause in the program");
    };
    // XXX: Fix labels for _tidxs
    _tidxs.iter().for_each(|(i, e)| {
        match _nodes[*i].tt {
            NodeT::PauseStart =>
		panic!("Please add a nothing at start of each thread"),
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
    // dbg!("{:?} {:?} {:?}", &_nodes, _i, _e);

    // XXX: Now start making the backend
    let ff = file_to_compile.split('.').collect::<Vec<&str>>()[0];
    let _fname = format!("{}.{}", ff, "cpp");
    let _fname_header = format!("{}.{}", ff, "h");
    let mut _file = File::create(&_fname).expect("Cannot create the cpp file");
    let mut _file_header =
	File::create(&_fname_header).expect("Cannot create the h file");

    let mut _ext_header: Vec<u8> = Vec::with_capacity(50);
    // XXX: First make the prolouge -- includes, threads, states,
    // signals, and vars
    let _ftowrite = backend::_codegen(
        &_signals,
        &_vars,
        &num_threads,
        &_states,
        &_syref,
        &_sref,
        &_vyref,
        &_vref,
        &file_to_compile,
        // XXX: This is for external function in C
        &_extern_calls,
        // XXX: These are generating the actual code
        _i,
        _e,
        &_nodes,
        // XXX: These are the other threads in the program
        _tidxs,
        // XXX: These are the nodes that have a valid ND state
        _ndtidxs,
	_ndtidlabs,
        // XXX: This is the external header u8 vector
        &mut _ext_header,
        // XXX: The name of the compiled cpp and header file
        ff,
        // XXX: This is the _gui present?
        args._gui,
        // XXX: This is for benchmarking
        args._bench,
	// XXX: These are the structs
	&_structs,
	&_ptids,
    );
    // XXX: Make all other thread code as well.
    _file
        .write_all(&_ftowrite)
        .expect("Cannot write to cpp file");

    // XXX: Write the external header file too
    _file_header
        .write_all(&_ext_header)
        .expect("Cannot write to h file");

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
        let _clang_bin_path = _clang_bin_path.strip_suffix('\n').unwrap();
        let _fmt_res = Command::new(_clang_bin_path)
            .arg("-i")
            .arg(_fname)
            .output()
            .expect("failed to format the cpp file");
        let _fmt_res = Command::new(_clang_bin_path)
            .arg("-i")
            .arg(_fname_header)
            .output()
            .expect("failed to format the h file");
    } else {
        println!("Could not find clang-format, the generated cpp file will not be formatted");
    }
}
