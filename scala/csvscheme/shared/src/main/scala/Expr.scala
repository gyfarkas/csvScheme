package csvscheme

import cats._
import cats.syntax.all._
import higherkindness.droste.data.Fix
import scala.io.Source

object Expr:
  enum Prim:
    case I(i: Int)
    case B(b: Boolean)
    case S(s: String)
    case Plus
    case Project(label: String)
    case Extend(label: String)
    case Remove(label: String)
    case EmptyList
    case ListCons

  sealed trait ExprF[A]
  case class VarF[A](v: String) extends ExprF[A]
  case class LamF[A](v: String, body: A) extends ExprF[A]
  case class AppF[A](fn: A, arg: A) extends ExprF[A]
  case class PrimF[A](prim: Prim) extends ExprF[A]
  case class LetF[A](name: String, defExpr: A, inExpr: A) extends ExprF[A]
  case class RecordF[A](fields: Map[String, A]) extends ExprF[A]

  given ef: Functor[ExprF] with
    def map[A, B](expr: ExprF[A])(f: A => B): ExprF[B] =
      expr match
        case VarF(v)                     => VarF(v)
        case LamF(v, body)               => LamF(v, f(body))
        case AppF(fn, arg)               => AppF(f(fn), f(arg))
        case PrimF(prim)                 => PrimF(prim)
        case LetF(name, defExpr, inExpr) => LetF(name, f(defExpr), f(inExpr))
        case RecordF(fields)             => RecordF(fields.map((l, v) => (l, f(v))))
  type Expr = Fix[ExprF]


  def let(name: String, d: Expr, i: Expr): Expr = Fix(LetF(name, d, i))
  def varE(name: String): Expr = Fix(VarF(name))
  def intE(i: Int): Expr = Fix(PrimF(Prim.I(i)))
  def boolE(b: Boolean):Expr = Fix(PrimF(Prim.B(b)))
  def stringE(s: String):Expr = Fix(PrimF(Prim.S(s)))
  def lambda(name: String, body: Expr) = Fix(LamF(name, body))
  def app(f: Expr, x: Expr) = Fix(AppF(f, x))
  def plus(i: Expr, j: Expr): Expr = app(app(Fix(PrimF(Prim.Plus)), i), j)
  val emptyRow : Expr = Fix(RecordF(Map.empty))
  def record(m :Map[String, Expr]) = Fix(RecordF(m))
  def extend(label: String, v: Expr, r: Expr) = app(app(Fix(PrimF(Prim.Extend(label))), v), r)
  def remove(label: String, r: Expr) = app(Fix(PrimF(Prim.Remove(label))), r)
  def project(label: String, r: Expr) = app(Fix(PrimF(Prim.Project(label))), r)
  def emptyList: Expr = Fix(PrimF(Prim.EmptyList))
  def list(es: Seq[Expr]): Expr = es.foldRight(emptyList)((e, xs) => app(app(Fix(PrimF(Prim.ListCons)), e), xs))

  def tryParseValue(a: String): Expr = {
     (a.toIntOption.map(intE) orElse a.toBooleanOption.map(boolE)).getOrElse(stringE(a))
  }

  def readString(csv: String, separator: String = ",") =
    val source = Source.fromString(csv)
    readSource(source)

  def readSource(csv: Source, separator: String = ",") =
    val lines = csv.getLines
    val firstLine = lines.next()
    val fields = firstLine.split(separator).toList
    list(for {
      line <- lines.toSeq
      values = line.split(separator).map(tryParseValue)
      labelled = fields.zip(values).toMap[String, Expr]
    } yield record(labelled))

  trait Expressable[A] {
    def toExpr(a: A) : Expr
  }

  given Expressable[Int] with
    def toExpr(i: Int) = intE(i)

  extension [A](a: A)(using m: Expressable[A])
    def toExpr = m.toExpr(a)
