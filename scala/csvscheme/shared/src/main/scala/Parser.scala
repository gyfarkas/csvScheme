package csvscheme
import cats._
import cats.syntax._
import cats.parse._
import cats.parse.Rfc5234._

import Expr._
import Expr.ExprF
import higherkindness.droste.data._

object ExpParser:
    //helpers
    val whitespace: Parser[Unit] = Parser.charIn(" \t\r\n").void
    val whitespaces0: Parser0[Unit] = whitespace.rep0.void
    def identifier: Parser[String] = alpha.rep.string.surroundedBy(whitespaces0)
    def brackets[A](p: Parser[A]) = p.between(Parser.char('('), (Parser.char(')')))

    //prims
    def intP: Parser[Expr.Expr] = Numbers.bigInt.map(i => intE(i.toInt))
    def plus: Parser[Expr.Expr] = Parser.char('+').map(p => Fix(PrimF(Prim.Plus)))
    def stringP: Parser[Expr.Expr] =
        alpha.rep.string.surroundedBy(Parser.char('"')).surroundedBy(whitespaces0).map(s => stringE(s))
    def recordField = ((identifier) <* Parser.char(':').surroundedBy(sp)) ~ form
    def record = recordField
    def varP: Parser[Expr.Expr] = identifier.surroundedBy(whitespaces0).map(x => Fix(VarF(x)))

    // forms
    def form: Parser[Expr.Expr] = Parser.recursive[Expr.Expr](recur =>

        val listSep: Parser[Unit] =
            Parser.char(',').soft.surroundedBy(whitespaces0).void

        def rep[A](pa: Parser[A]): Parser0[List[A]] =
            pa.repSep0(listSep).surroundedBy(whitespaces0)

        val listP =
            rep(recur).with1
            .between(Parser.char('['), Parser.char(']'))
            .map { vs => Expr.list(vs) }

        val kv: Parser[(String, Expr.Expr)] =
            identifier ~ (Parser.char(':').surroundedBy(whitespaces0) *> recur)

        val recP =
            rep(kv).with1
            .between(Parser.char('{'), Parser.char('}'))
            .map { vs => Expr.record(vs.toMap) }

        def addition = for {
            x <- Parser.char('+')
            _ <- whitespaces0
            i <- recur
            _ <- whitespaces0
            j <- recur
        } yield Expr.plus(i, j)

        val builtIn2Map: Map[String, String => Expr.Expr => Expr.Expr] = Map(
            "lambda" -> ((x: String) => (f: Expr.Expr) => Expr.lambda(x,f)),
            "select" ->  ((x: String) => (f: Expr.Expr) => Expr.select(x,f)),
            "sum" ->   ((x: String) => (f: Expr.Expr) => Expr.sum(x,f)),
            "count" ->  ((x: String) => (f: Expr.Expr) =>Expr.count(x, f)),
            "." -> ((x: String) => (f: Expr.Expr) => Expr.project(x, f)),
            "<<" -> ((x: String) => (f: Expr.Expr) => Expr.remove(x,f))
        )

        def builtIns2: List[Parser[Expr.Expr]] =
            (for ((builtIn, f) <- builtIn2Map)
            yield (for {
            _ <- Parser.string(builtIn).void
            _ <- whitespaces0
            x <- identifier
            _ <- whitespaces0
            b <- recur
        } yield f(x)(b))).toList

        val builtIn3Map: Map[String, String => Expr.Expr => Expr.Expr => Expr.Expr] =
            Map(
                ">>" -> ((x: String) => (f: Expr.Expr) => (b: Expr.Expr) => Expr.extend(x,f,b)),
                "let" -> ((x: String) => (f: Expr.Expr) => (b: Expr.Expr) => Expr.let(x,f,b))
            )

        def builtIns3: List[Parser[Expr.Expr]] =
            (for ((builtIn, f) <- builtIn3Map)
            yield (for {
            _ <- Parser.string(builtIn).void
            _ <- whitespaces0
            x <- identifier
            _ <- whitespaces0
            a <- recur
             _ <- whitespaces0
            b <- recur
        } yield f(x)(a)(b))).toList

        def app: Parser[Expr.Expr] = for {
            f <- recur
            _ <- whitespaces0
            x <- recur
        } yield Expr.app(f, x)

        Parser.oneOf(List(
            brackets(Parser.oneOf(builtIns2 ++ builtIns3) orElse addition orElse app),
            varP,
            intP,
            stringP,
            recP,
            listP))
    )
