package csvscheme

import higherkindness.droste.{ Algebra, scheme }

import Expr._
object Eval:
/*
  case class Fix[F[_]](unFix : F[Fix[F]])
  def cata[F[_], A](alg: F[A] => A)(fa: Fix[F])(using f: Functor[F]): A =
    alg(f.map(fa.unFix)(cata(alg)))
  case class CoFree[F[_], A](a: A, co: F[CoFree[F,A]])
*/

  final case class Lazy[F[_],A](val a: () => F[A]){
    def force: F[A] = a()
  }

  type Thunk = Lazy[EvalM, Value]
  object Thunk:
      def apply(v: => Value): Thunk = Lazy(() => EvalM.pure(v))

  type EvalM[A] = Either[String, A]

  object EvalM {
    def pure[A](a: A): EvalM[A] = Right(a)
    def fail(error: String) = Left(error)
  }


  enum Value:
    case I(int: Int)
    case Closure(f: Thunk => EvalM[Value])

  type Env = Map[String, Thunk]



  def bind(env: Env)(name: String, thunk: Thunk): EvalM[Env] =
    EvalM.pure(env.updated(name, thunk))

  def eval(env: Env)(expr: Expr): EvalM[Value] =
    def alg : Algebra[ExprF, Env => EvalM[Value]] =
      Algebra((e: ExprF[Env => EvalM[Value]]) =>
      e match
        case VarF(v) => env => env(v).force

        case PrimF(prim) => env => EvalM.pure(evalPrim(prim))
        case LamF(v, body) =>
          env => EvalM.pure(Value.Closure(x => bind(env)(v, x).flatMap(body)))
        case AppF(fn, arg) => env => {
          val result: EvalM[EvalM[Value]] = for {
                  f <- fn(env)
                  a <- arg(env)
                } yield {
                 f match
                   case Value.Closure(ft) => ft(Thunk(a))
                   case _ => Left("not a function")
                 }
          result.flatten
        }

        case LetF(name, defExpr, inExpr) => (e: Env) =>
          val t = defExpr(env).map(x => Thunk.apply(x))
          val newEnv = t.flatMap(t => bind(env)(name, t))
          newEnv.flatMap(e => inExpr(e))
    )
    def ev = scheme.cata(alg)
    ev(expr)(env)

  def evalPrim(prim: Prim): Value = prim match
    case Prim.I(i) => Value.I(i)
    case Prim.Plus => mkClosure2(x => y => { (x,y) match
      case (Value.I(a), Value.I(b)) => EvalM.pure(Value.I(a + b))
      case _ => EvalM.fail("invalid arguments to plus")
    })

  def mkClosure2(f: Value => Value => EvalM[Value]): Value =
    mkClosure(x => EvalM.pure(mkClosure(y => f(x)(y))))
  def mkClosure(f: Value => EvalM[Value]) : Value =
    Value.Closure(x => x.force.flatMap(f))