package com.dillonbeliveau.dillvmisp.interpreter

import com.dillonbeliveau.dillvmisp._

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

trait ScopeObject {
  def newChild: Scope = Scope(Map.empty, this)
  def newChildWith(bindings: Map[String, Value]) = Scope(bindings, this)
  def get(name: String): Value
}
case class Scope(bindings: Map[String, Value], parent: ScopeObject) extends ScopeObject {
  def addBinding(name: String, value: Value): Scope = Scope(bindings + (name -> value), parent)

  override def get(name: String): Value = bindings.getOrElse(name, parent.get(name))
}
case object TopLevelScope extends ScopeObject {
  override def get(name: String): Value = throw new Exception(s"Value $name undefined in scope.")
  override def newChild: Scope = this.newChildWith(Builtins.get)
}

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

object LispInterpreter {
  def interpret(code: String): Value = interpret(LispParser.quickParse(code))
  def interpret(expression: Term, scope: Scope): Value = expression match {
    // We can just return a value if that's what we got
    case value: Value => value

    // Getting a token? Grab it.
    case LispToken(name) => scope.get(name)

    // If it's a declaration of a new lambda function, take special action.
    case Cons(f: NewLambda, Cons(args, Cons(body, LispNil))) => f.apply(args, body, scope)

    // If it's a list containing only a function, force getting the function's value.
    // If this is a lambda function, this means it hasn't been completely applied yet. This will return a curried version.
    // If it's a builtin that takes an arbitrary number of arguments - this will force final evaluation.
    case Cons(f: Function, LispNil) => f.result

    // If the first thing in the sexp is a function and there's an expression in the spot where the first argument should be, evaluate it.
    case Cons(f: Function, Cons(x: Expression, xs)) => interpret(Cons(f, Cons(interpret(x, scope.newChild), xs)), scope)

    // If the first thing in the sexp is a function and there's a value to apply to it, apply it.
    case Cons(f: Function, Cons(x: Value, xs)) => {
      (f.apply(x), xs) match {
        // Keep applying arguments as long as we have them if we get a function back
        case (f: Function, _) => interpret(Cons(f, xs), scope)
        // If we get a value back, we're done.
        case (v: Value, LispNil) => v
        case _ => throw new Exception("Extra arguments on call to lambda function!")
      }
    }

    // If we have just an expression, interpret just that expression.
    case Cons(x: Expression, LispNil) => interpret(x, scope)

    // If the first thing in the sexp is an expression, evaluate it.
    case Cons(x: Expression, xs) => interpret(Cons(interpret(x, scope.newChild), xs), scope)
  }

  def interpret(expression: Term): Value = {
    interpret(expression, TopLevelScope.newChild)
  }
}
