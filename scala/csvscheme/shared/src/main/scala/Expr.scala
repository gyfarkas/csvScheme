package csvscheme

import cats.Functor
import higherkindness.droste.data.Fix

object Expr:
  enum Prim:
    case I(i: Int)
    case Plus

  sealed trait ExprF[A]
  case class VarF[A](v: String) extends ExprF[A]
  case class LamF[A](v: String, body: A) extends ExprF[A]
  case class AppF[A](fn: A, arg: A) extends ExprF[A]
  case class PrimF[A](prim: Prim) extends ExprF[A]
  case class LetF[A](name: String, defExpr: A, inExpr: A) extends ExprF[A]

  given ef: Functor[ExprF] with
    def map[A, B](expr: ExprF[A])(f: A => B): ExprF[B] =
      expr match
        case VarF(v)                     => VarF(v)
        case LamF(v, body)               => LamF(v, f(body))
        case AppF(fn, arg)               => AppF(f(fn), f(arg))
        case PrimF(prim)                 => PrimF(prim)
        case LetF(name, defExpr, inExpr) => LetF(name, f(defExpr), f(inExpr))

  type Expr = Fix[ExprF]
  def let(name: String, d: Expr, i: Expr): Expr = Fix(LetF(name, d, i))
  def varE(name: String): Expr = Fix(VarF(name))
  def intE(i: Int): Expr = Fix(PrimF(Prim.I(i)))
  def lambda(name: String, body: Expr) = Fix(LamF(name, body))
  def app(f: Expr, x: Expr) = Fix(AppF(f, x))
  def plus(i: Expr, j: Expr): Expr = app(app(Fix(PrimF(Prim.Plus)), i), j)
