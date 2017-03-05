package com.dillonbeliveau.dillvmisp

/**
  * Created by dillon on 3/1/17.
  */
trait Expression {
  def evaluate: Value
}

case object LispNil extends Value {
  override def +(other: Value): Value = other
  override def evaluate: Value = this
}

/*
class Function extends Expression {
  override def evaluate: Value = ???
}

class Application extends Expression {
  override def evaluate: Value = ???
}
*/

case class Cons(val left: Expression, val right: Expression) extends Expression {
  override def evaluate: Value = ???
}
