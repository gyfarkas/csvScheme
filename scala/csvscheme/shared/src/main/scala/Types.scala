package csvscheme

import cats._
import cats.syntax.all._
import cats.implicits._
import higherkindness.droste.data.Fix
import cats.data.StateT
import csvscheme.Expr._
import higherkindness.droste.{Algebra, scheme}

object Types:
    enum TypeF[T]:
        case TIntF[T]() extends TypeF[T]
        case TBoolF[T]() extends TypeF[T]
        case TStringF[T]() extends TypeF[T]
        case TFnF[T](from:T, to:T) extends TypeF[T]
        case TVarF[T](name: String) extends TypeF[T]

    import TypeF._
    given Functor[TypeF] with
        def map[A,B](t: TypeF[A])(f: A=>B) = t match
            case TIntF() => TIntF()
            case TBoolF() => TBoolF()
            case TStringF() => TStringF()
            case TFnF(from,to) => TFnF(f(from), f(to))
            case TVarF(name) => TVarF(name)

    type Type = Fix[TypeF]

    case class TypeScheme(vars: List[String], t: Type)

    type Substitution = Map[String, Type]
    type TypeEnv = Map[String, TypeScheme]

    case class TypeState(val counter: Int)
    val emptyState = TypeState(0)

    enum TypeError:
        case TypesDoNotUnify
        case OccurCheckFailed
        case UnboundVariable

    type Error[A] = Either[TypeError, A]
    type TI[A] = StateT[Error, TypeState, A]
    type TypeCheckResult = TypeEnv => TI[(Substitution, Type)]

    def tiFail[A](err: TypeError): TI[A]  = StateT.liftF(Left(err))

    trait Types[T] {
        def apply(s: Substitution)(t: T): T
        def freeVars(t: T): Set[String]
    }

    extension [T](t:T) (using tt: Types[T])
        def apply(s: Substitution): T = tt.apply(s)(t)
        def freeVars: Set[String] = tt.freeVars(t)

    given Types[Type] with
        def freeVars(t: Type): Set[String] = {
            def alg = Algebra {
                (t: TypeF[Set[String]]) => t match
                    case TVarF(name) => Set(name)
                    case TFnF(from, to) => from.union(to)
                    case _ => Set.empty
            }
            val f = scheme.cata(alg)
            f(t)
        }
        def apply(s: Substitution)(t: Type): Type = {
            def alg = Algebra(
                (t: TypeF[Type]) => t match
                    case TIntF() => Fix(TIntF())
                    case TBoolF() => Fix(TBoolF())
                    case TStringF() => Fix(TStringF())
                    case TFnF(from, to) =>
                        val t1: Type = from(s)
                        val t2: Type = to(s)
                        Fix(TFnF(t1, t2))
                    case TVarF(name) => s.getOrElse(name, Fix(TVarF(name)))
            )
            val f = scheme.cata(alg)
            f(t)
        }

    given Types[TypeScheme] with
        def freeVars(ts: TypeScheme) = ts.t.freeVars -- ts.vars.toSet
        def apply(s: Substitution)(ts: TypeScheme): TypeScheme =
           ts.copy(t = ts.t.apply(s.removedAll(ts.vars)))

    given tl[F[_], A](using ta: Types[A], ff: Functor[F], fld: Foldable[F]): Types[F[A]] with
        def apply(s: Substitution)(fa: F[A]) = fa.map(_.apply(s))
        def freeVars(fa: F[A]) = fa.map(_.freeVars).foldLeft(Set.empty[String])(_ union _)
         // .foldRight(Eval.always(Set.empty[String]))((a, b) => b.map(_ union a)).value

    given Types[TypeEnv] with
        def freeVars(te: TypeEnv) = te.values.toList.freeVars
        def apply(s: Substitution)(te: TypeEnv) = te.mapValues(_.apply(s)).toMap

    val nullSubst: Substitution = Map.empty
    def composeSubst(s1: Substitution, s2: Substitution): Substitution =
        s2.map((s,v) => (s -> v(s1))) ++ s1

    given Monoid[Substitution] with
        def empty = nullSubst
        def combine(s1: Substitution, s2: Substitution) = composeSubst(s1,s2)

    def newTypeVar : TI[Type] = for {
        state <- StateT.get
        newState = (state.copy(counter = state.counter + 1))
        _ <- StateT.set(newState)
    } yield Fix(TVarF(s"a${newState.counter}"))

    def tiPrim(prim: Expr.Prim): TI[(Substitution, Type)] =  prim match
        case Prim.I(i) => (nullSubst, Fix(TIntF())).pure
        case Prim.Plus => (nullSubst, Fix(TFnF(Fix(TIntF()), Fix(TFnF(Fix(TIntF()), Fix(TIntF())))))).pure

    def varBind(v: String, t: Type): TI[Substitution] =
        if t.freeVars.contains(v)
        then StateT.liftF(Left(TypeError.OccurCheckFailed))
        else Map[String, Type](v -> t).pure

    def generalize(env: TypeEnv, t: Type): TypeScheme =
        TypeScheme(vars = (t.freeVars -- env.freeVars).toList, t)

    def instantiate(ts: TypeScheme): TI[Type] = for {
        nvars <- ts.vars.traverse[TI, Type](_ => newTypeVar)
        s = ts.vars.zip(nvars).toMap
    } yield (ts.t(s))

    def unify(t1: Type, t2: Type): TI[Substitution] = (t1, t2) match
        case (Fix(TIntF()), Fix(TIntF())) => nullSubst.pure
        case (Fix(TBoolF()), Fix(TBoolF())) => nullSubst.pure
        case (Fix(TStringF()), Fix(TStringF())) => nullSubst.pure
        case (Fix(TVarF(n)), Fix(TVarF(m))) if m == n => nullSubst.pure
        case (t, Fix(TVarF(m))) => varBind(m,t)
        case (Fix(TVarF(n)), t) => varBind(n,t)
        case (Fix(TFnF(a, b)), Fix(TFnF(c,d))) =>
            for {
                s1 <- unify(a,c)
                s2 <- unify(b,d)
            } yield s1 |+| s2
        case _ => tiFail[Substitution](TypeError.TypesDoNotUnify)

    def inferTypeAlg =
        Algebra((e: ExprF[TypeCheckResult]) => e match {
            case Expr.PrimF(prim) => (e: TypeEnv) => tiPrim(prim)
            case Expr.AppF(func, arg) => (e: TypeEnv) => for {
                v <- newTypeVar
                // that is unfortunate can't pattern match here
                // (x,y) <- thingReturningAWrappedPair
                // because withFilter is missing
                funcpair <- func(e)
                (fSubst, fType) = funcpair
                updatedEnv = e(fSubst)
                argpair <- arg(updatedEnv)
                (argSubst, argType) = argpair
                s3 <- unify(fType(argSubst), Fix(TFnF(argType, v)))
            } yield (s3 |+| argSubst |+| fSubst, v.apply(s3))
            case Expr.LamF(x, body) => (e: TypeEnv) => for {
                v <- newTypeVar
                newEnv = e.removed(x) ++ Map(x ->(TypeScheme(Nil, v)))
                b <- body(newEnv)
            } yield (b._1, Fix(TFnF(v, b._2)))
            case Expr.VarF(n) => (e: TypeEnv) => e.get(n) match {
                case Some(ts) =>instantiate(ts).map((nullSubst, _))
                case None => tiFail[(Substitution, Type)](TypeError.UnboundVariable)
            }
            case Expr.LetF(v, defExpr, inExpr) => (e: TypeEnv) => for {
                defPair <- defExpr(e)
                (defSubst, defT) = defPair
                ts = generalize(e(defSubst), defT)
                newEnv = e.updated(v, ts).apply(defSubst)
                inPair <- inExpr(newEnv)
                (inSubst, inT) = inPair
            } yield (defSubst |+| inSubst, inT)
    })

    def inferType = scheme.cata(inferTypeAlg)

    def runInference(env: TypeEnv)(expr: Expr): Error[Type] = inferType(expr)(env).map(_._2).run(emptyState).map(_._2)