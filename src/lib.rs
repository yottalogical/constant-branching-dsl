use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum Typ {
    Num,
    Bool,
    Arrow(Rc<Typ>, Rc<Typ>),
    Unit,
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnOp {
    Neg,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BinOp {
    Lt,
    Gt,
    Eq,
    Plus,
    Minus,
    Times,
    Ap,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Exp {
    Num(i128),
    Bool(bool),
    UnOp(UnOp, Rc<Exp>),
    BinOp(BinOp, Rc<Exp>, Rc<Exp>),
    If(Rc<Exp>, Rc<Exp>, Rc<Exp>),
    Var(Rc<String>),
    Let(Option<Typ>, Rc<Exp>, Rc<String>, Rc<Exp>),
    Fun(Option<Typ>, Rc<String>, Rc<Exp>),
    Fix(Option<Typ>, Rc<String>, Rc<Exp>),
    Triv,
}

impl Exp {
    pub fn evaluate(&self) -> Option<Exp> {
        match self {
            Exp::UnOp(UnOp::Neg, e) => {
                if let Some(Exp::Num(n)) = e.evaluate() {
                    Some(Exp::Num(-n))
                } else {
                    None
                }
            }
            Exp::BinOp(op, e1, e2) => match (e1.evaluate(), e2.evaluate()) {
                (Some(Exp::Num(n1)), Some(Exp::Num(n2))) => match op {
                    BinOp::Lt => Some(Exp::Bool(n1 < n2)),
                    BinOp::Gt => Some(Exp::Bool(n1 > n2)),
                    BinOp::Eq => Some(Exp::Bool(n1 == n2)),
                    BinOp::Plus => Some(Exp::Num(n1 + n2)),
                    BinOp::Minus => Some(Exp::Num(n1 - n2)),
                    BinOp::Times => Some(Exp::Num(n1 * n2)),
                    BinOp::Ap => None,
                },
                (Some(Exp::Fun(_, x, body)), Some(e2)) => {
                    if let BinOp::Ap = op {
                        Some(body.substitute(&e2, &x))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            Exp::If(e1, e2, e3) => {
                if let Some(Exp::Bool(b)) = e1.evaluate() {
                    if b {
                        e2.evaluate()
                    } else {
                        e3.evaluate()
                    }
                } else {
                    None
                }
            }
            Exp::Var(_) => None,
            Exp::Let(_, e1, x, e2) => Some(e2.substitute(e1, x)),
            Exp::Fix(_, x, e1) => Some(e1.substitute(self, x)),
            Exp::Num(_) | Exp::Bool(_) | Exp::Fun(_, _, _) | Exp::Triv => Some(self.clone()),
        }
    }

    fn substitute(&self, sub: &Exp, x: &Rc<String>) -> Exp {
        match self {
            Exp::UnOp(op, e1) => Exp::UnOp(op.clone(), Rc::new(e1.substitute(sub, x))),
            Exp::BinOp(op, e1, e2) => Exp::BinOp(
                op.clone(),
                Rc::new(e1.substitute(sub, x)),
                Rc::new(e2.substitute(sub, x)),
            ),
            Exp::If(e1, e2, e3) => Exp::If(
                Rc::new(e1.substitute(sub, x)),
                Rc::new(e2.substitute(sub, x)),
                Rc::new(e3.substitute(sub, x)),
            ),
            Exp::Var(x1) => {
                if x1 == x {
                    sub.clone()
                } else {
                    self.clone()
                }
            }
            Exp::Let(t, e1, x1, e2) => {
                if x1 == x {
                    self.clone()
                } else {
                    Exp::Let(
                        t.clone(),
                        Rc::new(e1.substitute(sub, x)),
                        x1.clone(),
                        Rc::new(e2.substitute(sub, x)),
                    )
                }
            }
            Exp::Fun(t, x1, e1) => {
                if x1 == x {
                    self.clone()
                } else {
                    Exp::Fun(t.clone(), x1.clone(), Rc::new(e1.substitute(sub, x)))
                }
            }
            Exp::Fix(t, x1, e1) => {
                if x1 == x {
                    self.clone()
                } else {
                    Exp::Fix(t.clone(), x1.clone(), Rc::new(e1.substitute(sub, x)))
                }
            }
            Exp::Num(_) | Exp::Bool(_) | Exp::Triv => self.clone(),
        }
    }
}
