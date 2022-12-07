package csvscheme

import cats._
import cats.syntax._
import cats.parse._
import cats.parse.Rfc5234._

import Expr._
import Expr.ExprF
import higherkindness.droste.data._
import ExpParser._
import Types._
import Interpret._
import higherkindness.droste.data.Fix
import cats._
import cats.syntax.all._
class ParserTests extends munit.FunSuite {

    test("parse lambda id expr") {
        val exprStr = "(lambda x x)"
        val expected: Either[Parser.Error, Expr] = Right(Fix(LamF("x", Fix(VarF("x")))))
        val obtained = ExpParser.form.parse(exprStr).map(_._2)
        assertEquals(expected, obtained)
    }

     test("parse app id expr") {
        val exprStr = "(x y)"
        val expected: Either[Parser.Error, Expr] =
            Right(Fix(AppF(Fix((VarF("x"))), Fix(VarF("y")))))
        val obtained = ExpParser.form.parse(exprStr).map(_._2)
        assertEquals(expected, obtained)
    }

     test("parse expr app") {
        val exprStr = "((lambda x x)(lambda x x))"
        val expected: Either[Parser.Error, Expr] = Right(
            Fix(AppF(
                Fix(LamF("x", Fix(VarF("x")))),
                Fix(LamF("x", Fix(VarF("x")))))))
        val obtained = ExpParser.form.parse(exprStr).map(_._2)
        assertEquals(expected, obtained)
    }

    test("parse expr plus") {
        val exprStr = "(+ 1 2)"
        val expected: Either[Parser.Error, Expr] = Right(
           Expr.plus(intE(1), intE(2)))
        val obtained = ExpParser.form.parse(exprStr).map(_._2)
        assertEquals(expected, obtained)
     }

      test("parse expr select") {
        val exprStr = "(select a x)"
        val expected: Either[Parser.Error, Expr] = Right(
           Expr.select("a", varE("x")))
        val obtained = ExpParser.form.parse(exprStr).map(_._2)
        assertEquals(expected, obtained)
     }

    test("parse let") {
        val exprStr = "(let id (lambda x (>> l 3 x)) (id 1))"
        assert(ExpParser.form.parse(exprStr).isRight)
    }

    test("table select") {
        val exprStr = "(let table [{a:1}, {a:2}, {a:3}] (select a table))"
        assert(ExpParser.form.parse(exprStr).isRight)
        val ty = ExpParser.form.parse(exprStr).map(_._2).flatMap(runInference(Map.empty))
        assert(ty.isRight)
        assertEquals(ty, Right(Fix(TypeF.TListF(Fix(TypeF.TIntF())))))
        val value = ExpParser.form.parse(exprStr).map(_._2).flatMap(eval(Map.empty))
        assert(value.isRight)
        assertEquals(value, Right(Value.List(Stream(Value.I(1), Value.I(2), Value.I(3)))))
    }
}
