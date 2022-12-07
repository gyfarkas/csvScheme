package csvscheme

import higherkindness.droste._
import higherkindness.droste.data._

import Expr._
import Interpret._
import Types._
import ExpParser.form
import PP._

@main def hello: Unit =
  val banner = """
|
|                 __      _
|  ___ _____   __/ _\ ___| |__   ___ _ __ ___   ___
| / __/ __\ \ / /\ \ / __| '_ \ / _ \ '_ ` _ \ / _ \
|| (__\__ \\ V / _\ \ (__| | | |  __/ | | | | |  __/
| \___|___/ \_/  \__/\___|_| |_|\___|_| |_| |_|\___|
  """.stripMargin
  println(banner)
  go(Map.empty)(Map.empty)

  def go(environment: Interpret.Env)(typeEnv: Types.TypeEnv): Unit =
      print("csvScheme> ")
      val input = Console.in.readLine()
      if input == "quit"
      then
        println("Good bye!")
          System.exit(0)
      else
        val parsed = form.parse(input).map(_._2)
        val typed = parsed.flatMap(Types.runInference(typeEnv))
        val evaluated = parsed.flatMap(Interpret.eval(environment))
        parsed match
          case Left(err) =>
            println(err.toString())
            go(environment)(typeEnv)
          case Right(exp) =>
            Types.runInference(typeEnv)(exp) match
              case Left(err) =>
                println(err.toString())
                go(environment)(typeEnv)
              case Right(t) =>
                Interpret.eval(environment)(exp) match
                  case Right(v) =>
                    println(s"${ppValue(v)} : ${ppType(t)}")
                    go(environment)(typeEnv)
                  case Left(err) =>
                     println(err.toString())
                     go(environment)(typeEnv)
