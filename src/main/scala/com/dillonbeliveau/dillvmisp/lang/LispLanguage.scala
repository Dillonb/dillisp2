package com.dillonbeliveau.dillvmisp.lang

/**
  * Created by dillon on 3/1/17.
  */

trait Term
// An expression can still be evaluated.
trait Expression extends Term
// A value is done evaluating.
trait Value extends Term

case class LispNumber(number: Double) extends Value
case class LispString(text: String) extends Value
case class LispToken(name: String) extends Expression
case object LispNil extends Value
case class Cons(left: Term, right: Term) extends Expression
