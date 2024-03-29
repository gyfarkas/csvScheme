package csvscheme

import higherkindness.droste.{Algebra, scheme}

import Expr._
import cats._
import cats.syntax.all._
import scala.collection.immutable.Stream.Empty
object Interpret:
  /*
  case class Fix[F[_]](unFix : F[Fix[F]])
  def cata[F[_], A](alg: F[A] => A)(fa: Fix[F])(using f: Functor[F]): A =
    alg(f.map(fa.unFix)(cata(alg)))
  case class CoFree[F[_], A](a: A, co: F[CoFree[F,A]])
   */
  type Thunk = Eval[EvalM[Value]]
  object Thunk:
    def apply(v: => Value): Thunk = Eval.later(v).map(EvalM.pure)

  type EvalM[A] = Either[String, A]

  object EvalM {
    def pure[A](a: A): EvalM[A] = Right(a)
    def fail(error: String) = Left(error)
  }

  enum Value:
    case I(int: Int)
    case B(b: Boolean)
    case S(s: String)
    case Closure(f: Thunk => EvalM[Value])
    case Rec(rs: Map[String, Value]) // records are strict
    case List(values: Stream[Value]) // list are lazy

  type Env = Map[String, Thunk]

  def bind(env: Env)(name: String, thunk: Thunk): EvalM[Env] =
    EvalM.pure(env.updated(name, thunk))

  def eval(env: Env)(expr: Expr): EvalM[Value] =
    def alg: Algebra[ExprF, Env => EvalM[Value]] =
      Algebra((e: ExprF[Env => EvalM[Value]]) =>
        e match
          case VarF(v)     => env => env(v).value
          case PrimF(prim) => env => EvalM.pure(evalPrim(prim))
          case LamF(v, body) =>
            env => EvalM.pure(Value.Closure(x => bind(env)(v, x).flatMap(body)))
          case AppF(fn, arg) =>
            env => {
              val result: EvalM[EvalM[Value]] = for {
                f <- fn(env)
                a <- arg(env)
              } yield {
                f match
                  case Value.Closure(ft) => ft(Thunk(a))
                  case _                 => Left("not a function")
              }
              result.flatten
            }
          case LetF(name, defExpr, inExpr) =>
            (e: Env) =>
              val t = defExpr(e).map(x => Thunk(x))
              val newEnv = t.flatMap(t => bind(e)(name, t))
              newEnv.flatMap(e => inExpr(e))
          case RecordF(fields) => (e: Env) =>
            val newFields = fields.foldLeft(EvalM.pure(Map.empty[String, Value])){
              case (m, (l,v)) => v(e).flatMap(ve => m.map(_.updated(l, ve)))
            }
            newFields.map(Value.Rec(_))
      ) // end Algebra
    def ev = scheme.cata(alg)
    ev(expr)(env)

  def evalPrim(prim: Prim): Value = prim match
    case Prim.I(i) => Value.I(i)
    case Prim.B(i) => Value.B(i)
    case Prim.S(i) => Value.S(i)
    case Prim.Plus =>
      mkClosure2(x =>
        y => {
          (x, y) match
            case (Value.I(a), Value.I(b)) => EvalM.pure(Value.I(a + b))
            case _ => EvalM.fail("invalid arguments to plus")
        }
      )
    case Prim.Extend(label) => mkClosure2(v =>
      r =>
        (r, v) match
          case (Value.Rec(rs), x) =>
            EvalM.pure(Value.Rec(rs.updated(label, x)))
          case _ => EvalM.fail("invalid extension base")
    )

    case Prim.Remove(label) => mkClosure(r =>
      r match
        case (Value.Rec(rs)) => EvalM.pure(Value.Rec(rs - label))
        case _ => EvalM.fail("invalid removal")
      )

    case Prim.Project(label) => mkClosure(r =>
       r match
        case (Value.Rec(rs)) if rs.contains(label) => EvalM.pure(rs(label))
        case _ => EvalM.fail(s"cannot project, $label is invalid ")
    )

    case Prim.EmptyList => Value.List(Stream.empty[Value])
    case Prim.ListCons => mkClosure2(x =>
      l =>
        (x, l) match
          case (v, Value.List(xs)) => EvalM.pure(Value.List(xs.prepended(v)))
          case _ => EvalM.fail("consing on non list")
    )
    case Prim.ListMap => mkClosure2(f =>
      l =>
        (f, l) match
          case (Value.Closure(f), Value.List(xs)) =>
            xs.traverse(x => f(Thunk(x))).map(Value.List(_))
          case _ => EvalM.fail("invalid list map")
    )

     case Prim.Filter => mkClosure2(f =>
      l =>
        (f, l) match
          case (Value.Closure(f), Value.List(xs)) =>
            val filtered = xs
                          .map(x => (x, f(Thunk(x))))
                          .filter(x => x._2 == EvalM.pure(Value.B(true)))
                          .map((v, b) => v)
            EvalM.pure(Value.List(filtered))

          case _ => EvalM.fail("invalid list map")
    )

      case Prim.Fold => mkClosure3(
        z => f => l =>
        (z, f, l) match
          case (zv, Value.Closure(f), Value.List(xs)) =>
            xs.foldLeft(EvalM.pure(zv)) {
              (acc, v) => acc.flatMap(a => f(Thunk(a))).flatMap(g =>
                g match
                  case Value.Closure(gc) => gc(Thunk(v))
                  case _ => EvalM.fail("Expected two argument function")
              )
            }
          case _ => EvalM.fail("invalid list map")
    )

  def mkClosure2(f: Value => Value => EvalM[Value]): Value =
    mkClosure(x => EvalM.pure(mkClosure(y => f(x)(y))))
  def mkClosure(f: Value => EvalM[Value]): Value =
    Value.Closure(x => x.map(_.flatMap(f)).value)
  def mkClosure3(f: Value => Value => Value => EvalM[Value]): Value =
    mkClosure(x => EvalM.pure(mkClosure2(y => z => f(x)(y)(z))))