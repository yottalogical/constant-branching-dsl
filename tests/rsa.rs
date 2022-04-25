use constant_branching_dsl::*;
use std::rc::Rc;

fn mod_pow(m: i128, e: i128, n: i128) -> i128 {
    if e == 0 {
        1
    } else {
        let a = mod_pow(m, e / 2, n);
        let b = a * a;

        if e % 2 == 0 {
            b % n
        } else {
            (b * m) % n
        }
    }
}

fn test_rsa(m: i128, e: i128, n: i128) {
    let mod_pow_exp = Exp::Fun(
        None,
        "m",
        Rc::new(Exp::Fun(
            None,
            "n",
            Rc::new(Exp::Fix(
                None,
                "mod_pow",
                Rc::new(Exp::Fun(
                    None,
                    "e",
                    Rc::new(Exp::If(
                        Rc::new(Exp::BinOp(
                            BinOp::Eq,
                            Rc::new(Exp::Var("e")),
                            Rc::new(Exp::Num(0)),
                        )),
                        Rc::new(Exp::Num(1)),
                        Rc::new(Exp::Let(
                            None,
                            Rc::new(Exp::BinOp(
                                BinOp::Ap,
                                Rc::new(Exp::Var("mod_pow")),
                                Rc::new(Exp::BinOp(
                                    BinOp::Div,
                                    Rc::new(Exp::Var("e")),
                                    Rc::new(Exp::Num(2)),
                                )),
                            )),
                            "a",
                            Rc::new(Exp::Let(
                                None,
                                Rc::new(Exp::BinOp(
                                    BinOp::Times,
                                    Rc::new(Exp::Var("a")),
                                    Rc::new(Exp::Var("a")),
                                )),
                                "b",
                                Rc::new(Exp::If(
                                    Rc::new(Exp::BinOp(
                                        BinOp::Eq,
                                        Rc::new(Exp::BinOp(
                                            BinOp::Mod,
                                            Rc::new(Exp::Var("e")),
                                            Rc::new(Exp::Num(2)),
                                        )),
                                        Rc::new(Exp::Num(0)),
                                    )),
                                    Rc::new(Exp::BinOp(
                                        BinOp::Mod,
                                        Rc::new(Exp::Var("b")),
                                        Rc::new(Exp::Var("n")),
                                    )),
                                    Rc::new(Exp::BinOp(
                                        BinOp::Mod,
                                        Rc::new(Exp::BinOp(
                                            BinOp::Times,
                                            Rc::new(Exp::Var("b")),
                                            Rc::new(Exp::Var("m")),
                                        )),
                                        Rc::new(Exp::Var("n")),
                                    )),
                                )),
                            )),
                        )),
                    )),
                )),
                8,
                Rc::new(Exp::Num(0)),
            )),
        )),
    );

    let actual = Exp::BinOp(
        BinOp::Ap,
        Rc::new(Exp::BinOp(
            BinOp::Ap,
            Rc::new(Exp::BinOp(
                BinOp::Ap,
                Rc::new(mod_pow_exp),
                Rc::new(Exp::Num(m)),
            )),
            Rc::new(Exp::Num(n)),
        )),
        Rc::new(Exp::Num(e)),
    )
    .evaluate();

    let expected = Exp::Num(mod_pow(m, e, n));

    assert_eq!(actual.unwrap(), expected);
}

#[test]
fn test_0() {
    test_rsa(2, 0, 3);
}

#[test]
fn test_10() {
    test_rsa(2, 10, 29);
}

#[test]
fn test_50() {
    test_rsa(5, 50, 7919);
}

#[test]
fn test_127() {
    test_rsa(1234, 127, 7919);
}
