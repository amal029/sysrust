use analyse::{get_num_threads, get_states};
use error::print_bytes;
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Write;
use std::process::exit;
use sysrust::{ast, parse};

use crate::analyse::{_analyse_var_signal_uses, get_s_v_ref, get_signals, get_vars};
use crate::rewrite::{rewrite_to_graph_fsm, GraphNode};
mod analyse;
mod backend;
mod error;
mod rewrite;

fn main() {
    let args: Vec<String> = env::args().collect();
    let _ast = parse(&args[1]);

    // XXX: Analyse signa/var declaration and their uses
    let mut stack: Vec<HashMap<String, (ast::Type, analyse::SignalVarType)>> =
        Vec::with_capacity(1000);

    // XXX: The errors in the program are collected here.
    let mut rets: Vec<(usize, usize, String)> = Vec::with_capacity(1000);

    // XXX: Check the usage and declaration of signals and variables
    let tid = 0;
    stack.push(HashMap::with_capacity(1000)); // pushed the first hashmap
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
    println!("{:?}", _states);

    // XXX: Get all the signals in each thread
    let mut tid = 0;
    let mut tot = 1;
    let mut _signals: Vec<Vec<ast::Stmt>> = vec![vec![]; num_threads];
    get_signals(&mut _signals, &_ast, &mut tid, &mut tot);
    println!("{:?}", _signals);

    // XXX: Get all the vars in each thread
    let mut tid = 0;
    let mut tot = 1;
    let mut _vars: Vec<Vec<ast::Stmt>> = vec![vec![]; num_threads];
    get_vars(&mut _vars, &_ast, &mut tid, &mut tot);
    println!("{:?}", _vars);

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
    println!("{:?} {:?} {:?} {:?}", _syref, _sref, _vyref, _vref);

    // XXX: Make the FSM graph
    let mut _nodes: Vec<GraphNode> = Vec::with_capacity(1000);
    let mut idx = 0usize;
    let tid = 0usize;
    let (_i, _e) = rewrite_to_graph_fsm(&args[1], &_ast, tid, &mut idx, &mut _nodes);
    // println!("{:?} {:?} {:?}", _nodes, _i, _e);

    // XXX: Now start making the backend
    let ff = args[1].split('.').collect::<Vec<&str>>()[0];
    let _fname = format!("{}.{}", ff, "cpp");
    let mut _file = File::create(_fname).expect("Cannot create the cpp file");

    // XXX: First make the prolouge -- includes, threads, states,
    // signals, and vars
    _file
        .write_all(&backend::_prolouge(
            &_signals,
            &_vars,
            &num_threads,
            &_states,
        ))
        .expect("Cannot write to cpp file");
    // println!("{:?}", backend::_prolouge());
}
