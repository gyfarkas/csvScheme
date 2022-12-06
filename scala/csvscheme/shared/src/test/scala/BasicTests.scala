package csvscheme

import Expr._
import Types._
import Interpret._
import higherkindness.droste.data.Fix
import cats._
import cats.syntax.all._

class BasicTests extends munit.FunSuite {
  val testAdditionExpr: Expr =
    Fix(
      AppF(
        Fix(
          LamF(
            "x",
            Fix(
              AppF(
                Fix(AppF(Fix(PrimF(Prim.Plus)), Fix(VarF("x")))),
                Fix(PrimF(Prim.I(4)))
              )
            )
          )
        ),
        Fix(PrimF(Prim.I(3)))
      )
    )

  val idLam: Expr = lambda("x", varE("x"))
  val idApp: Expr = app(varE("id"), varE("id"))
  val e0: Expr.Expr = Fix(LetF("id", idLam, Fix(VarF("id"))))
  val e1 = Fix(LetF("id", idLam, idApp))
  /*
  let id =
    fun x ->
      let y = x
      in y
    in
    id(id)

   */
  val e2 = let(
    "id",
    lambda("x", let("y", varE("x"), varE("y"))),
    app(varE("id"), varE("id"))
  )
  /* pseudo ml
  let id =
    fun x ->
      let y = x
      in y
  in id(id)(2)
   */
  val e3 = let(
    "id",
    lambda("x", let("y", varE("x"), varE("y"))),
    app(app(varE("id"), varE("id")), intE(2))
  )

  test("smart constructors are doing what I want") {
    val obtained = app(lambda("x", plus(varE("x"), intE(4))), intE(3))
    val expected = testAdditionExpr
    assertEquals(obtained, expected)
  }

  test("test addition type checks") {
    val obtained = runInference(Map.empty)(testAdditionExpr)
    val expected: Type = Fix(TypeF.TIntF())
    assertEquals(obtained, Right(expected))
  }

  test("expressions typeCheck") {
    val obtained = List(e0, e1, e2, e3).traverse(runInference(Map.empty))
    val expected = Right(
      List(
        Fix(TypeF.TFnF(Fix(TypeF.TVarF("a2")), Fix(TypeF.TVarF("a2")))),
        Fix(TypeF.TFnF(Fix(TypeF.TVarF("a4")), Fix(TypeF.TVarF("a4")))),
        Fix(TypeF.TFnF(Fix(TypeF.TVarF("a4")), Fix(TypeF.TVarF("a4")))),
        Fix(TypeF.TIntF())
      )
    )
    assert(obtained.isRight)
    assertEquals(obtained, expected)
  }

  test("expressions interpreted") {
    val obtained =
      List(e0, e1, e2, e3, testAdditionExpr).traverse(eval(Map.empty))
    assert(obtained.isRight)
    assertEquals(eval(Map.empty)(e3), Right(Value.I(2)))
    assertEquals(eval(Map.empty)(testAdditionExpr), Right(Value.I(7)))
  }

  test("simple record") {
    val rec = record(Map("a" -> intE(1), "b" -> intE(2)))
    val extended = extend("c", intE(3), rec)
    val projected = project("c", extended)
    val typeCheckResult = runInference(Map.empty)(projected)
    val evaluated = eval(Map.empty)(projected)

    assert(typeCheckResult.isRight)
    assertEquals(typeCheckResult, Right(Fix(TypeF.TIntF())))
    assertEquals(evaluated, Right(Value.I(3)))
  }

  test("deeper record") {
    val exp = app(
      lambda("r", plus(project("a", varE("r")), project("b", varE("r"))))
      , record(Map("a" -> intE(1), "b" -> intE(3))))
    val evaluated = eval(Map.empty)(exp)
    val typeChecked = runInference(Map.empty)(exp)
    assertEquals(evaluated, Right(Value.I(4)))
    assertEquals(typeChecked,Right(Fix(TypeF.TIntF())))
  }

  test("record type") {
    val rec = record(Map("a" -> intE(1), "c" -> intE(3)))
    val typeChecked = runInference(Map.empty)(rec)
    val expected =
      Fix(TypeF.TRowExtendF("c", Fix(TypeF.TIntF()),
       Fix(TypeF.TRowExtendF("a",Fix(TypeF.TIntF()),
        Fix(TypeF.TEmptyRowF())))))
    assertEquals(typeChecked, Right(expected))
  }

  test("project from record") {
    val proj = project("a", record(Map("a" -> intE(1), "c" -> intE(3))))
    val typeChecked = runInference(Map.empty)(proj)
    val expected = Fix(TypeF.TIntF())
    assertEquals(typeChecked, Right(expected))
  }

  test("record projection type error") {
    val exp = app(
       lambda("x", plus(project("a", varE("x")), project("b", varE("x"))))
      , record(Map("a" -> intE(1), "c" -> intE(3))))
    val typeChecked = runInference(Map.empty)(exp)
    assertEquals(typeChecked, Left(TypeError.TypesDoNotUnify))
  }

  test("emptyList") {
    val exp = emptyList
    val evaluated = eval(Map.empty)(exp)
    val typeChecked = runInference(Map.empty)(exp)
    assertEquals(evaluated, Right(Value.List(LazyList.empty)))
    assertEquals(typeChecked, Right(Fix(TypeF.TListF(Fix(TypeF.TVarF("a1"))))))
  }

   test("non empyList") {
    val exp = list(List(intE(1),intE(2),intE(3)))
    val evaluated = eval(Map.empty)(exp)
    val typeChecked = runInference(Map.empty)(exp)
    assertEquals(evaluated, Right(Value.List(LazyList(Value.I(1), Value.I(2), Value.I(3)))))
    assertEquals(typeChecked, Right(Fix(TypeF.TListF(Fix(TypeF.TIntF())))))
  }
}
