package com.dillonbeliveau.dillvmisp

import scala.util.parsing.combinator.JavaTokenParsers


/**
  * Created by dillon on 3/1/17.
  */

/*
object Parser {
  def parse(text: String): Option[Sexp] = text match {
    case t if t.isEmpty => None
    case t if t.headOption.isDefined && t.head == "(" => parseList(text)
    case _ => parseValue(text)
  }

  def parseList(text: String) = ???
  def parseValue(text: String) = ???
}
*/


object LispParser extends JavaTokenParsers {

  private def listToConsTree(list: List[Expression]): Cons = list match {
    case head :: Nil => head match {
      case head: Value => Cons(head, LispNil)
      case head: Expression => Cons(head, )
    }
    case x :: y =>
  }

  def string: Parser[LispString] = "\"" ~> "[^\"]*".r <~ "\"" ^^ (s => LispString(s))
  def number: Parser[LispNumber] = """\d+.?\d*""".r ^^ (s => LispNumber(s.toDouble))
  def value: Parser[Value] = (string | number)
  def list: Parser[Expression] = "(" ~> (expression+) <~ ")" ^^ (exprs => listToConsTree(exprs))
  def expression: Parser[Expression] = (value | list)
}