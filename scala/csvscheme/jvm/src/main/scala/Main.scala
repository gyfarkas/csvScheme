package csvscheme

import cats._
import cats.syntax._
import cats.free._


@main def hello: Unit =
  println("Hello world!")
  println(s"fix : ${Fix(Lam("x",I(3)))}" )


case class Fix[F[_]](unFix : F[Fix[F]])
case class CoFree[F[_], A](a: A, co: F[CoFree[F,A]])


sealed trait ExprF[A]
case class I[A](i: Int) extends ExprF[A]
case class Lam[A](v: String, body: ExprF[A]) extends ExprF[A]
case class App[A](fn: ExprF[A], arg: ExprF[A]) extends ExprF[A]

/* hand rolled Fix is not really stack safe or efficient,
 so let's trust Typelevel and use Cofree
 */
type Expr = Fix[ExprF]

type typedExpr[T] = Cofree[ExprF,T]

def toI(i: Int): Expr = Fix(I(i))

given Functor[ExprF] with
  def map[A, B](expr: ExprF[A])(f: A => B) =
    expr match
      case I(i) => I(i)
      case Lam(v, body) => Lam(v, map(body)(f))
      case App(fn, arg) => App(map(fn)(f), map(arg)(f))
