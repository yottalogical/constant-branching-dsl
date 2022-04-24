use constant_branching_dsl::*;
use std::rc::Rc;

#[test]
fn test_num() {
    assert_eq!(Exp::Num(123).evaluate().unwrap(), Exp::Num(123));
}

#[test]
fn test_true() {
    assert_eq!(Exp::Bool(true).evaluate().unwrap(), Exp::Bool(true));
}

#[test]
fn test_false() {
    assert_eq!(Exp::Bool(false).evaluate().unwrap(), Exp::Bool(false));
}

#[test]
fn test_neg() {
    assert_eq!(
        Exp::UnOp(UnOp::Neg, Rc::new(Exp::Num(123)))
            .evaluate()
            .unwrap(),
        Exp::Num(-123)
    );
}

#[test]
fn test_lt() {
    assert_eq!(
        Exp::BinOp(BinOp::Lt, Rc::new(Exp::Num(123)), Rc::new(Exp::Num(234)))
            .evaluate()
            .unwrap(),
        Exp::Bool(true)
    );

    assert_eq!(
        Exp::BinOp(BinOp::Lt, Rc::new(Exp::Num(123)), Rc::new(Exp::Num(123)))
            .evaluate()
            .unwrap(),
        Exp::Bool(false)
    );

    assert_eq!(
        Exp::BinOp(BinOp::Lt, Rc::new(Exp::Num(234)), Rc::new(Exp::Num(123)))
            .evaluate()
            .unwrap(),
        Exp::Bool(false)
    );
}

#[test]
fn test_gt() {
    assert_eq!(
        Exp::BinOp(BinOp::Gt, Rc::new(Exp::Num(123)), Rc::new(Exp::Num(234)))
            .evaluate()
            .unwrap(),
        Exp::Bool(false)
    );

    assert_eq!(
        Exp::BinOp(BinOp::Gt, Rc::new(Exp::Num(123)), Rc::new(Exp::Num(123)))
            .evaluate()
            .unwrap(),
        Exp::Bool(false)
    );

    assert_eq!(
        Exp::BinOp(BinOp::Gt, Rc::new(Exp::Num(234)), Rc::new(Exp::Num(123)))
            .evaluate()
            .unwrap(),
        Exp::Bool(true)
    );
}

#[test]
fn test_eq() {
    assert_eq!(
        Exp::BinOp(BinOp::Eq, Rc::new(Exp::Num(123)), Rc::new(Exp::Num(234)))
            .evaluate()
            .unwrap(),
        Exp::Bool(false)
    );

    assert_eq!(
        Exp::BinOp(BinOp::Eq, Rc::new(Exp::Num(123)), Rc::new(Exp::Num(123)))
            .evaluate()
            .unwrap(),
        Exp::Bool(true)
    );
}

#[test]
fn test_plus() {
    assert_eq!(
        Exp::BinOp(BinOp::Plus, Rc::new(Exp::Num(123)), Rc::new(Exp::Num(234)))
            .evaluate()
            .unwrap(),
        Exp::Num(123 + 234)
    );
}

#[test]
fn test_minus() {
    assert_eq!(
        Exp::BinOp(BinOp::Minus, Rc::new(Exp::Num(123)), Rc::new(Exp::Num(234)))
            .evaluate()
            .unwrap(),
        Exp::Num(123 - 234)
    );
}

#[test]
fn test_times() {
    assert_eq!(
        Exp::BinOp(BinOp::Times, Rc::new(Exp::Num(123)), Rc::new(Exp::Num(234)))
            .evaluate()
            .unwrap(),
        Exp::Num(123 * 234)
    );
}

#[test]
fn test_ap() {
    assert_eq!(
        Exp::BinOp(
            BinOp::Ap,
            Rc::new(Exp::Fun(
                None,
                "x",
                Rc::new(Exp::BinOp(
                    BinOp::Plus,
                    Rc::new(Exp::Var("x")),
                    Rc::new(Exp::Num(1)),
                ))
            )),
            Rc::new(Exp::Num(123))
        )
        .evaluate()
        .unwrap(),
        Exp::Num(124)
    );
}

#[test]
fn test_if_true() {
    assert_eq!(
        Exp::If(
            Rc::new(Exp::Bool(true)),
            Rc::new(Exp::Num(123)),
            Rc::new(Exp::Num(234))
        )
        .evaluate()
        .unwrap(),
        Exp::Num(123)
    );
}

#[test]
fn test_if_false() {
    assert_eq!(
        Exp::If(
            Rc::new(Exp::Bool(false)),
            Rc::new(Exp::Num(123)),
            Rc::new(Exp::Num(234))
        )
        .evaluate()
        .unwrap(),
        Exp::Num(234)
    );
}

#[test]
fn test_var() {
    Exp::Var("x").evaluate().unwrap_err();
}

#[test]
fn test_let() {
    assert_eq!(
        Exp::Let(
            None,
            Rc::new(Exp::Num(123)),
            "x",
            Rc::new(Exp::BinOp(
                BinOp::Plus,
                Rc::new(Exp::Var("x")),
                Rc::new(Exp::Var("x"))
            ))
        )
        .evaluate()
        .unwrap(),
        Exp::Num(246)
    );
}

#[test]
fn test_fun() {
    assert_eq!(
        Exp::Fun(
            None,
            "x",
            Rc::new(Exp::BinOp(
                BinOp::Plus,
                Rc::new(Exp::Var("x")),
                Rc::new(Exp::Var("x"))
            ))
        )
        .evaluate()
        .unwrap(),
        Exp::Fun(
            None,
            "x",
            Rc::new(Exp::BinOp(
                BinOp::Plus,
                Rc::new(Exp::Var("x")),
                Rc::new(Exp::Var("x"))
            ))
        )
    );
}

#[test]
fn test_fix() {
    assert_eq!(
        Exp::Fix(
            None,
            "f",
            Rc::new(Exp::Fun(None, "x", Rc::new(Exp::Var("f")))),
            1,
            Rc::new(Exp::Fun(None, "", Rc::new(Exp::Triv)))
        )
        .evaluate()
        .unwrap(),
        Exp::Fun(
            None,
            "x",
            Rc::new(Exp::Fix(
                None,
                "f",
                Rc::new(Exp::Fun(None, "x", Rc::new(Exp::Var("f")))),
                0,
                Rc::new(Exp::Fun(None, "", Rc::new(Exp::Triv)))
            ))
        )
    );

    assert_eq!(
        Exp::Fix(
            None,
            "f",
            Rc::new(Exp::BinOp(
                BinOp::Plus,
                Rc::new(Exp::Num(1)),
                Rc::new(Exp::Num(2))
            )),
            1,
            Rc::new(Exp::Num(0))
        )
        .evaluate()
        .unwrap(),
        Exp::Num(3)
    );
}

#[test]
fn test_triv() {
    assert_eq!(Exp::Triv.evaluate().unwrap(), Exp::Triv);
}
