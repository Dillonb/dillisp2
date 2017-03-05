package com.dillonbeliveau.dillvmisp

import scala.util.parsing.combinator.JavaTokenParsers


/**
  * Created by dillon on 3/1/17.
  */

object LispParser extends JavaTokenParsers {
  private def listToConsTree(list: List[Term]): Cons = list match {
    case x :: Nil => Cons(x, LispNil)
    case x :: xs => Cons(x, listToConsTree(xs))
  }

  def string: Parser[LispString] = "\"" ~> "[^\"]*".r <~ "\"" ^^ (s => LispString(s))
  def number: Parser[LispNumber] = """\d+\.?\d*""".r ^^ (s => LispNumber(s.toDouble))
  def value: Parser[Value] = string | number
  def token: Parser[LispToken] = """[A-Za-z\+\-\*\$\%\&\#\@]\S*""".r ^^ (s => LispToken(s))
  def list: Parser[Cons] = "(" ~> (expression+) <~ ")" ^^ (exprs => listToConsTree(exprs))
  def expression: Parser[Term] = value | list | token

  def quickParse(code: String): Term = {
    parse(expression, code) match {
      case Success(matched, _) => matched
    }
  }
}
