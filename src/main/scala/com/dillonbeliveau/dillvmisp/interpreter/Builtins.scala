package com.dillonbeliveau.dillvmisp.interpreter

import com.dillonbeliveau.dillvmisp.lang._

/**
  * Created by Dillon on 3/5/17.
  */
object Builtins {
  def get: Map[String, Value] =
    Map(
      "+" -> new PlusFunction(LispNumber(0)),

      "lambda" -> new NewLambda()
    )
}

class PlusFunction(value: LispNumber) extends Function {
  override def apply(arg: Value): Value = arg match {
    case LispNumber(addend) => {
      new PlusFunction(LispNumber(value.number + addend))
    }
  }

  override def toString: String = s"+${value.number}"

  override def result: Value = value
}

class NewLambda extends Value {
  private def unrollArgList(args: Term): List[String] = args match {
    case Cons(LispToken(name), xs) => name :: unrollArgList(xs)
    case LispNil => Nil
  }

  def apply(args: Term, body: Term, scope: Scope): LambdaFunction = {
    LambdaFunction(unrollArgList(args), scope, body)
  }
}
