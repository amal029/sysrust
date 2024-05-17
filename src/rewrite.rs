use sysrust::ast::*;

pub struct State {
    labelnum: u64,
}

impl State {
    pub fn new() -> Self {
        State { labelnum: 0 }
    }
    fn get_label_num(&mut self) -> u64 {
        let r = self.labelnum;
        self.labelnum += 1;
        return r;
    }
}

pub fn rewrite_stmts(stmts: Vec<Stmt>, state: &mut State) -> Vec<Stmt> {
    stmts
        .into_iter()
        .map(|x| imm_rewrite(x, state))
        .collect::<Vec<Stmt>>()
}

// XXX: Consume the ast and give back a new ast with weak and immediate
// aborts rewritten.
fn imm_rewrite(_ast: Stmt, state: &mut State) -> Stmt {
    fn rewrite_option(s: Option<Box<Stmt>>, state: &mut State) -> Option<Box<Stmt>> {
        match s {
            Some(x) => Some(Box::new(imm_rewrite(*x, state))),
            None => None,
        }
    }
    fn abort_rewrite(a: Option<ASQual>, s: Stmt, sy: Symbol, p: (usize, usize)) -> Stmt {
        match a {
            Some(x) => match x {
                ASQual::Immediate => Stmt::Present(
                    Expr::Esymbol(sy.clone(), p),
                    Box::new(Stmt::Noop(p)),
                    Some(Box::new(Stmt::Abort(sy, None, Box::new(s), p))),
                    p,
                ),
                ASQual::Weak => Stmt::Abort(sy, Some(ASQual::Weak), Box::new(s), p),
                ASQual::WeakImmediate => panic!("Weak and immediate abort !supported"),
            },
            None => Stmt::Abort(sy, a, Box::new(s), p),
        }
    }
    fn suspend_rewrite(
        a: Option<ASQual>,
        s: Stmt,
        sy: Symbol,
        p: (usize, usize),
        state: &mut State,
    ) -> Stmt {
        match a {
            Some(x) => match x {
                ASQual::Immediate => {
                    let mut ss = String::from("S");
                    ss.push_str(state.get_label_num().to_string().as_str());
                    let pa = Stmt::Pause(Symbol::Symbol(ss, p), p);
                    let lop = Stmt::Loop(Box::new(pa), p);
                    let ab = Stmt::Abort(sy.clone(), None, Box::new(lop), p);
                    let sus = Stmt::Suspend(sy, None, Box::new(s), p);
                    Stmt::Block(vec![ab, sus], p)
                }
                ASQual::Weak => Stmt::Abort(sy, Some(ASQual::Weak), Box::new(s), p),
                ASQual::WeakImmediate => panic!("Weak and immediate suspend !supported"),
            },
            None => Stmt::Abort(sy, a, Box::new(s), p),
        }
    }
    match _ast {
        Stmt::Block(x, p) => Stmt::Block(rewrite_stmts(x, state), p),
        Stmt::Pause(s, p) => Stmt::Pause(s, p),
        Stmt::Emit(s, v, p) => Stmt::Emit(s, v, p),
        Stmt::Present(e, s, o, p) => Stmt::Present(
            e,
            Box::new(imm_rewrite(*s, state)),
            rewrite_option(o, state),
            p,
        ),
        Stmt::Signal(s, o, p) => Stmt::Signal(s, o, p),
        Stmt::Abort(s, o, st, p) => {
            let sg = imm_rewrite(*st, state);
            abort_rewrite(o, sg, s, p)
        }
        Stmt::Suspend(s, o, st, p) => {
            let sg = imm_rewrite(*st, state);
            suspend_rewrite(o, sg, s, p, state)
        }
        Stmt::Loop(s, p) => Stmt::Loop(Box::new(imm_rewrite(*s, state)), p),
        Stmt::Assign(s, e, p) => Stmt::Assign(s, e, p),
        Stmt::Noop(p) => Stmt::Noop(p),
        Stmt::Spar(sts, p) => Stmt::Spar(rewrite_stmts(sts, state), p),
    }
}
