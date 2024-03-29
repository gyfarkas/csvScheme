package csvscheme

import cats._
import cats.syntax.all._
import cats.implicits._
import higherkindness.droste.data.Fix
import cats.data.StateT
import csvscheme.Expr._
import csvscheme.PP.ppType
import higherkindness.droste.{Algebra, scheme}
import cats.data.EitherT
import cats.data.State

object Types:
  enum TypeF[T]:
    case TIntF[T]() extends TypeF[T]
    case TBoolF[T]() extends TypeF[T]
    case TStringF[T]() extends TypeF[T]
    case TFnF[T](from: T, to: T) extends TypeF[T]
    case TVarF[T](name: String) extends TypeF[T]
    case TRecordF[T](fields: T) extends TypeF[T]
    case TEmptyRowF[T]() extends TypeF[T]
    case TRowExtendF[T](label: String, v: T, rowTail: T) extends TypeF[T]
    case TListF[T](elemType: T) extends TypeF[T]

  import TypeF._
  given Functor[TypeF] with
    def map[A, B](t: TypeF[A])(f: A => B) = t match
      case TIntF()        => TIntF()
      case TBoolF()       => TBoolF()
      case TStringF()     => TStringF()
      case TFnF(from, to) => TFnF(f(from), f(to))
      case TVarF(name)    => TVarF(name)
      case TEmptyRowF()   => TEmptyRowF()
      case TRowExtendF(label, v, rowTail) => TRowExtendF(label, f(v), f(rowTail))
      case TRecordF(t) => TRecordF(f(t))
      case TListF(t) => TListF(f(t))

  type Type = Fix[TypeF]

  case class TypeScheme(vars: List[String], t: Type)

  type Substitution = Map[String, Type]
  type TypeEnv = Map[String, TypeScheme]

  case class TypeState(val counter: Int)
  val emptyState = TypeState(0)

  enum TypeError:
    case TypesDoNotUnify(string: String)
    case OccurCheckFailed
    case UnboundVariable
    case RecursiveRow

  type Error[A] = Either[TypeError, A]
  type TState[A] = State[TypeState, A]
  type TI[A] = StateT[Error, TypeState, A]
  type TypeCheckResult = TypeEnv => TI[(Substitution, Type)]

  def tiFail[A](err: TypeError): TI[A] = StateT.liftF(Left(err))

  trait Types[T] {
    def apply(s: Substitution)(t: T): T
    def freeVars(t: T): Set[String]
  }

  extension [T](t: T)(using tt: Types[T])
    def apply(s: Substitution): T = tt.apply(s)(t)
    def freeVars: Set[String] = tt.freeVars(t)

  given Types[Type] with
    def freeVars(t: Type): Set[String] = {
      def alg = Algebra { (t: TypeF[Set[String]]) =>
        t match
          case TVarF(name)    => Set(name)
          case TFnF(from, to) => from.union(to)
          case TRowExtendF(_, v, rowTail) => v.union(rowTail)
          case TListF(t)      => t
          case TRecordF(t)    => t
          case _              => Set.empty
      }
      val f = scheme.cata(alg)
      f(t) // needed to split the line to let scalac to apply the implicit typeclass
    }
    def apply(s: Substitution)(t: Type): Type = {
      def alg = Algebra((t: TypeF[Type]) =>
        t match
          case TIntF()    => Fix(TIntF())
          case TBoolF()   => Fix(TBoolF())
          case TStringF() => Fix(TStringF())
          case TFnF(from, to) =>
            val t1: Type = from(s)
            val t2: Type = to(s)
            Fix(TFnF(t1, t2))
          case TVarF(name) => s.getOrElse(name, Fix(TVarF(name)))
          case TEmptyRowF() => Fix(TEmptyRowF())
          case TRowExtendF(label, v, rowTail) =>
            val t1: Type = v(s)
            val t2: Type = rowTail(s)
            Fix(TRowExtendF(label, v, rowTail))
          case TListF(t) =>  Fix(TListF(t(s)))
          case TRecordF(t) => Fix(TRecordF(t(s)))
      )
      val f = scheme.cata(alg)
      f(t)
    }

  given Types[TypeScheme] with
    def freeVars(ts: TypeScheme) = ts.t.freeVars -- ts.vars.toSet
    def apply(s: Substitution)(ts: TypeScheme): TypeScheme =
      ts.copy(t = ts.t(s -- ts.vars))

  given tl[F[_], A](using
      ta: Types[A],
      ff: Functor[F],
      fld: Foldable[F]
  ): Types[F[A]] with
    def apply(s: Substitution)(fa: F[A]) = fa.map(_.apply(s))
    def freeVars(fa: F[A]) =
      fa.map(_.freeVars).foldLeft(Set.empty[String])(_ union _)
    // .foldRight(Eval.always(Set.empty[String]))((a, b) => b.map(_ union a)).value

  given Types[TypeEnv] with
    def freeVars(te: TypeEnv) = te.values.toList.freeVars
    def apply(s: Substitution)(te: TypeEnv) = te.mapValues(_.apply(s)).toMap

  val nullSubst: Substitution = Map.empty
  def composeSubst(s1: Substitution, s2: Substitution): Substitution =
    s1 ++ s2.map((s, v) => (s -> v(s1)))
  given Monoid[Substitution] with
    def empty = nullSubst
    def combine(s1: Substitution, s2: Substitution) = composeSubst(s1, s2)

  def newTypeVar(prefix: String): TI[Type] = for {
    state <- StateT.get
    newState = (state.copy(counter = state.counter + 1))
    _ <- StateT.set(newState)
  } yield Fix(TVarF(s"${prefix}${newState.counter}"))

  def tiPrim(prim: Expr.Prim): TI[(Substitution, Type)] = prim match
    case Prim.I(i) => (nullSubst, Fix(TIntF())).pure
    case Prim.B(b) => (nullSubst, Fix(TBoolF())).pure
    case Prim.S(s) => (nullSubst, Fix(TStringF())).pure
    case Prim.Plus =>
      (
        nullSubst,
        Fix(TFnF(Fix(TIntF()), Fix(TFnF(Fix(TIntF()), Fix(TIntF())))))
      ).pure

    case Prim.ListCons => for {
      a <- newTypeVar("a")
    } yield (nullSubst, Fix(TFnF(a, Fix(TFnF(Fix(TListF(a)), Fix(TListF(a)))))))

    case Prim.EmptyList => for {
      a <- newTypeVar("a")
    } yield (nullSubst, Fix(TListF(a)))

    case Prim.Extend(label) => for {
      r <- newTypeVar("r")
      a <- newTypeVar("a")
    } yield (nullSubst, Fix(TFnF(a, Fix(TFnF(Fix(TRecordF(r)), Fix(TRecordF(Fix(TRowExtendF(label, a, r)))))))))

    case Prim.Project(label) => for {
      a <- newTypeVar("a")
      r <- newTypeVar("r")
    } yield (nullSubst, Fix(TFnF(Fix(TRecordF(Fix(TRowExtendF(label, a, r)))), a)))

    case Prim.Remove(label) => for {
      a <- newTypeVar("a")
      r <- newTypeVar("r")
    } yield (nullSubst, Fix(TFnF(Fix(TRecordF(Fix(TRowExtendF(label, a, r)))), r)))

    case Prim.ListMap => for {
      a <- newTypeVar("b")
      b <- newTypeVar("a")
    } yield (
      nullSubst,
      Fix(TFnF(Fix(TFnF(a,b)), Fix(TFnF(Fix(TListF(a)), Fix(TListF(b))))))
    )

    case Prim.Filter => for {
      a <- newTypeVar("a")
    } yield (
      nullSubst,
      Fix(TFnF(Fix(TFnF(a, Fix(TBoolF()))), Fix(TFnF(Fix(TListF(a)), Fix(TListF(a))))))
    )

    case Prim.Fold => for {
      a <- newTypeVar("a")
      z <- newTypeVar("a")
      combineT = Fix(TFnF(z, Fix(TFnF(z,z))))
    } yield (
      nullSubst,
      Fix(TFnF(z,
        Fix(TFnF(combineT,
          Fix(TFnF(Fix(TListF(a)), z))))))
      )

  def varBind(v: String, t: Type): TI[Substitution] =
    if t.freeVars.contains(v)
    then StateT.liftF(Left(TypeError.OccurCheckFailed))
    else Map[String, Type](v -> t).pure

  def generalize(env: TypeEnv, t: Type): TypeScheme =
    TypeScheme(vars = (t.freeVars -- env.freeVars).toList, t)

  def instantiate(ts: TypeScheme): TI[Type] = for {
    nvars <- ts.vars.traverse(_ => newTypeVar("a"))
    s = ts.vars.zip(nvars).toMap
  } yield (ts.t(s))

  def varName(t: Type): Option[String] =
    def go(t: Type): (Map[String, Type], Option[String]) = t match
      case Fix(TVarF(name)) => (Map.empty, Some(name))
      case Fix(TRecordF(t)) => go(t)
      case Fix(TEmptyRowF()) => (Map.empty, None)
      case Fix(TRowExtendF(label, v, r)) =>
        val rs = go(r)
        (rs._1.updated(label, v), rs._2)
      case _ => (Map.empty, None)
    go(t)._2

  def rewriteRow(t: Type, newLabel: String): TI[Eval[(Substitution, Type, Type)]] = t match
    case Fix(TEmptyRowF()) => tiFail(TypeError.TypesDoNotUnify("empty row"))
    case Fix(TRowExtendF(label, v, rowTail)) =>
      if (label == newLabel)
      then Eval.always((nullSubst, v, rowTail)).pure
      else rowTail match
        case Fix(TVarF(name)) => for {
          rowVar <- newTypeVar("r")
          fieldVar <- newTypeVar("a")
          s = Map(name -> Fix(TRowExtendF(newLabel, fieldVar, rowVar)))
        } yield Eval.later((
           s,
           fieldVar,
           Fix(TRowExtendF(label, v, rowVar))))
        case _ =>
          for {
          result <- rewriteRow(rowTail, newLabel)
        } yield result.map(r =>
          (r._1, r._2, Fix(TRowExtendF(label, v, r._3(r._1)))))
    case other => tiFail(TypeError.TypesDoNotUnify(s"other: ${ppType(other)}"))

  def unify(t1: Type, t2: Type): TI[Substitution] = (t1, t2) match
    case (Fix(TIntF()), Fix(TIntF()))             => nullSubst.pure
    case (Fix(TBoolF()), Fix(TBoolF()))           => nullSubst.pure
    case (Fix(TStringF()), Fix(TStringF()))       => nullSubst.pure
    case (Fix(TVarF(n)), Fix(TVarF(m))) if m == n => nullSubst.pure
    case (Fix(TVarF(n)), t)                       => varBind(n, t)
    case (t, Fix(TVarF(m)))                       => varBind(m, t)
    case (Fix(TFnF(a, b)), Fix(TFnF(c, d))) =>
      for {
        s1 <- unify(a, c)
        s2 <- unify(b(s1), d(s1))
      } yield s1 |+| s2
    case (Fix(TListF(t1)), Fix(TListF(t2))) => unify(t1, t2)
    case (Fix(TRecordF(t1)), Fix(TRecordF(t2))) => unify(t1, t2)
    case (Fix(TEmptyRowF()), Fix(TEmptyRowF())) => nullSubst.pure
    case (r1@Fix(TRowExtendF(label1, fieldType1, rowTail1)), r2@Fix(TRowExtendF(_, _, _))) =>
      for {
        rewriteResult <- rewriteRow(r2, label1)
        (s1, fieldType2, rowTail2) = rewriteResult.value
        s <- varName(rowTail1) match
          case Some(name) if s1.contains(name)  => tiFail(TypeError.RecursiveRow)
          case _ => for {
            s2 <- unify(fieldType1(s1), fieldType2(s1))
            s3 = s2 |+| s1
            s4 <- unify(rowTail1(s3), rowTail2(s3))
          } yield s4 |+| s3
      } yield s
    case (t1,t2) => tiFail[Substitution](TypeError.TypesDoNotUnify(s"t1: ${ppType(t1)}, t2: ${ppType(t2)}"))

  def inferTypeAlg =
    Algebra((e: ExprF[TypeCheckResult]) =>
      e match {
        case Expr.PrimF(prim) => (e: TypeEnv) => tiPrim(prim)
        case Expr.AppF(func, arg) =>
          (e: TypeEnv) =>
            for {
              v <- newTypeVar("a")
              // that is unfortunate can't pattern match here
              // (x,y) <- thingReturningAWrappedPair
              // because withFilter is missing
              funcPair <- func(e)
              (fSubst, fType) = funcPair
              updatedEnv = e(fSubst)
              argPair <- arg(updatedEnv)
              (argSubst, argType) = argPair
              s3 <- unify(Fix(TFnF(argType, v)), fType(argSubst))
            } yield (s3 |+| argSubst |+| fSubst, v(s3))
        case Expr.LamF(x, body) =>
          (e: TypeEnv) =>
            for {
              v <- newTypeVar("a")
              newEnv = e ++ Map(x -> (TypeScheme(Nil, v)))
              b <- body(newEnv)
            } yield (b._1, Fix(TFnF(v(b._1), b._2)))
        case Expr.VarF(n) =>
          (e: TypeEnv) =>
            e.get(n) match {
              case Some(ts) => instantiate(ts).map((nullSubst, _))
              case None => tiFail[(Substitution, Type)](TypeError.UnboundVariable)
            }
        case Expr.LetF(v, defExpr, inExpr) =>
          (e: TypeEnv) =>
            for {
              defPair <- defExpr(e)
              (defSubst, defT) = defPair
              ts = generalize(e(defSubst), defT)
              newEnv = e.updated(v, ts).apply(defSubst)
              inPair <- inExpr(newEnv)
              (inSubst, inT) = inPair
            } yield (defSubst |+| inSubst, inT)
        case Expr.RecordF(fields) => (e: TypeEnv) =>
          fields.foldRight((nullSubst, Fix(TEmptyRowF())).pure[TI]) {
            case ((l, v), srti) => srti.flatMap((s,r) =>
                v(e).map((s1, t1) =>
                 (s |+| s1, Fix(TRowExtendF(l, t1, r)))))
          }.map((s,t) => (s, Fix(TRecordF(t))))
      }
    )

  def inferType = scheme.cata(inferTypeAlg)

  def runInference(env: TypeEnv)(expr: Expr): Error[Type] =
  inferType(expr)(env).map((s, t) => t(s)).run(emptyState).map(_._2)
