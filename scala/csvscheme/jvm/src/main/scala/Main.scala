package csvscheme

import higherkindness.droste._
import higherkindness.droste.data._

import Expr._
import Eval._

@main def hello: Unit =
  println("Hello world!")
  println(s"fix : ${eval(Map.empty)(testExpr)}" )
  println(s"fix 2 : ${eval(Map.empty)(Fix(AppF(Fix(PrimF(Prim.I(2))), Fix(PrimF(Prim.I(2))))))}" )

val testExpr: Expr =
  Fix(AppF(
    Fix(LamF("x",
      Fix(AppF(
        Fix(AppF(
          Fix(PrimF(Prim.Plus)),
          Fix(VarF("x")))),
        Fix(PrimF(Prim.I(4))))))),
    Fix(PrimF(Prim.I(3)))))
