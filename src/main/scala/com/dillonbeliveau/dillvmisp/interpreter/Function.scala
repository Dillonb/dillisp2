package com.dillonbeliveau.dillvmisp.interpreter

import com.dillonbeliveau.dillvmisp._
import com.dillonbeliveau.dillvmisp.lang.{Term, Value}

/**
  * Created by Dillon on 3/5/17.
  */
trait Function extends Value {
  def apply(arg: Value): Value
  def result: Value
}

object LambdaFunction {
  def apply(args: Seq[String], scope: Scope, body: Term, appliedArguments: Map[String, Value] = Map.empty): LambdaFunction = new LambdaFunction(args, scope, body, appliedArguments)
}
class LambdaFunction(args: Seq[String], scope: Scope, body: Term, appliedArguments: Map[String, Value] = Map.empty) extends Value with Function {
  def apply(arg: Value): Value = args match {
    case x :: Nil => LispInterpreter.interpret(body, scope.newChildWith(appliedArguments + (x -> arg)))
    case x :: xs => LambdaFunction(xs, scope, body, appliedArguments + (x -> arg))
  }

  override def toString: String = s"(lambda (args) ${body.toString}"

  // If we're being forced to return a result (this will happen if we're called like `(fn)`, return the function with currying as-is.
  override def result: Value = this
}
