package com.dillonbeliveau.dillvmisp

/**
  * Created by dillon on 3/1/17.
  */

sealed trait Expression
sealed trait Value extends Expression
case class LispNumber(number: Double) extends Value
case class LispString(text: String) extends Value
case class LispToken(name: String) extends Value
case object LispNil extends Value
case class Cons(val left: Expression, val right: Expression) extends Expression
