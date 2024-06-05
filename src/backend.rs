use pretty::RcDoc;
use sysrust::ast::{ExprOp, SimpleDataExpr, Stmt, Symbol, Type, Val};

fn _symbol_string(_sy: &Symbol) -> &String {
    match _sy {
        Symbol::Symbol(_sy, _) => _sy,
    }
}

fn _type_string(_ty: &Type) -> &str {
    match _ty {
        Type::Int => "int",
        Type::Float => "float",
        Type::None => panic!("Cannot write an empty type"),
    }
}

fn _val_string(_val: &Val) -> String {
    match _val {
        Val::VInt(x) => x.to_string(),
        Val::VFloat(x) => x.to_string(),
    }
}

fn _expr_op_std_op(_expr: &ExprOp) -> &str {
    match _expr {
        ExprOp::Plus => "std::plus",
        ExprOp::Minus => "std::minus",
        ExprOp::Mul => "std::multiplies",
        ExprOp::Div => "std::divides",
        ExprOp::Mod => "std::modulus",
        ExprOp::Pow => panic!("Power operator not yet supported in C++ backend"),
    }
}

fn _sig_decl(_s: &Stmt, _tid: usize) -> RcDoc<()> {
    match _s {
        Stmt::Signal(_sy, _io, _pos) => {
            let _m = format!("struct signal_{}_{}", _symbol_string(_sy), _tid);
            let _m = format!("{} {{bool status;}};", _m);
            let _a = RcDoc::<()>::as_string(_m).append(RcDoc::hardline());
            let sname = _symbol_string(_sy);
            let u = format!(
                "static signal_{}_{} {}_curr, {}_prev;",
                sname, _tid, sname, sname
            );
            _a.append(RcDoc::as_string(u)).append(RcDoc::hardline())
        }
        Stmt::DataSignal(_sy, _io, _ty, _iv, _op, _pos) => {
            let _m = format!("struct signal_{}_{}", _symbol_string(_sy), _tid);
            let _m = format!(
                "{} {{bool status; {} value = {}; {}<{}> op {{}};}};",
                _m,
                _type_string(_ty),
                _val_string(_iv),
                _expr_op_std_op(_op),
                _type_string(_ty)
            );
            let a = RcDoc::<()>::as_string(_m).append(RcDoc::hardline());
            let sname = _symbol_string(_sy);
            let u = format!(
                "static signal_{}_{} {}_curr, {}_prev;",
                sname, _tid, sname, sname
            );
            a.append(u).append(RcDoc::hardline())
        }
        _ => panic!("Got a non signal when generating backend"),
    }
}

fn _var_decl(_var: &Stmt, _tid: usize) -> RcDoc<()> {
    match _var {
        Stmt::Variable(_sy, _ty, _iv, _pos) => {
            let _m = format!(
                "{} {}_{} = {};",
                _type_string(_ty),
                _symbol_string(_sy),
                _tid,
                _val_string(_iv)
            );
            RcDoc::<()>::as_string(_m).append(RcDoc::hardline())
        }
        _ => panic!("Got a non variable when generating backend"),
    }
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
            let _m = _sig_decl(_ss, _i).append(RcDoc::hardline());
            _m.render(8, &mut w).expect("Cannot declare varaibles");
        }
    }

    // XXX: Declare all the variables in each thread
    let _m_header = RcDoc::<()>::as_string("// Var decls").append(RcDoc::hardline());
    _m_header.render(8, &mut w).expect("Cannot write signals");
    for (_i, _s) in _vars.iter().enumerate() {
        for _ss in _s {
            let _m = _var_decl(_ss, _i);
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
    let _threadvar: Vec<_> = (0..*_nthreads)
        .map(|x| format!("static Thread{}State st{};", x, x))
        .collect();
    let _threadvar = _threadvar.join("\n");
    _n = _n.append(RcDoc::hardline()).append(_threadvar);

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
    let _o =
<<<<<<< HEAD
        "template <class... Ts> struct overloaded: Ts... {{using Ts::operator()...;}};".to_string();
=======
        format!("template <class... Ts> struct overloaded: Ts... {{using Ts::operator()...;}};");
>>>>>>> 50278cd (Updated to start adding templates + code.)
    _n = _n
        .append(RcDoc::hardline())
        .append(_o)
        .append(RcDoc::hardline());
<<<<<<< HEAD

    let _ = _n.render(8, &mut w);
=======
>>>>>>> 50278cd (Updated to start adding templates + code.)

    // TODO: All thread prototypes
    // TODO: All the visits
    // TODO: The real code from the FSM

    let _ = _n.render(8, &mut w);
    // String::from_utf8(w).expect("Could not generate the prolouge")
    w
}
