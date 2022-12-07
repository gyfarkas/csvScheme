package csvscheme

import higherkindness.droste.scheme
import higherkindness.droste.Algebra
import Types._
import Types.TypeF._
import csvscheme.Interpret.Value

object PP:
    def ppType(t: Types.Type): String = {
        def alg = Algebra{(t: TypeF[String]) => t match
          case TIntF()    => "Int"
          case TBoolF()   => "Bool"
          case TStringF() => "String"
          case TFnF(from, to) => s"($from -> $to)"
          case TVarF(name) => s"'$name"
          case TEmptyRowF() => "{}"
          case TRowExtendF(label, v, rowTail) => s"{$label:$v,$rowTail}"
          case TListF(t) => s"[$t]"
        }

        val f = scheme.cata(alg)
        f(t)
    }

    def ppValue(v: Value): String = v match
        case Value.I(i) => i.toString()
        case Value.S(s) => s
        case Value.B(b) => b.toString()
        case Value.Closure(_) => "<function>"
        case Value.Rec(m) => s"{${m.mapValues(ppValue).mkString}}"
        case Value.List(s) => s"$s"
