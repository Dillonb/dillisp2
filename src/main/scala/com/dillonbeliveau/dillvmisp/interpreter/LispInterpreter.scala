package com.dillonbeliveau.dillvmisp.interpreter

import com.dillonbeliveau.dillvmisp.lang._
import com.dillonbeliveau.dillvmisp.parser.LispParser

import scala.collection.mutable
import scala.io.Source

/**
  * Created by Dillon on 3/5/17.
  */


trait ScopeObject {
  def newChild: Scope = Scope(mutable.Map.empty, this)
  def newChildWith(bindings: mutable.Map[String, Value]) = Scope(bindings, this)
  def newChildWith(bindings: Map[String, Value]) = Scope(mutable.Map.empty ++ bindings, this)
  def get(name: String): Value
}
case class Scope(bindings: mutable.Map[String, Value], parent: ScopeObject) extends ScopeObject {
  def addBinding(name: String, value: Value): Scope = {
    bindings.put(name, value)
    this
  }

  override def get(name: String): Value = bindings.getOrElse(name, parent.get(name))
}
case object TopLevelScope extends ScopeObject {
  override def get(name: String): Value = throw new Exception(s"Value $name undefined in scope.")
  override def newChild: Scope = {
    val scope = this.newChildWith(Builtins.get)
    val code = LispParser.quickParse(Source.fromResource("stdlib.l").getLines.fold("")((a,b) => a+b))
    LispInterpreter.interpret(code, scope)

    scope
  }
}


object LispInterpreter {
  def interpret(code: String): Value = interpret(LispParser.quickParse(code))
  def interpret(expression: Term, scope: Scope): Value = expression match {
    // We can just return a value if that's what we got
    case value: Value => value

    // Getting a token? Grab it.
    case LispToken(name) => scope.get(name)

    // Bind values to names
    case Cons(LispToken("define"), xs) => xs match {
      case Cons(LispToken(name), Cons(t: Term, LispNil)) => {
        val v = interpret(t, scope)
        scope.addBinding(name, v)
        v
      }
    }

    // If it's a declaration of a new lambda function, take special action.
    case Cons(f: NewLambda, Cons(args, Cons(body, LispNil))) => f.apply(args, body, scope)

    // If it's a list containing only a function, force getting the function's value.
    // If this is a lambda function, this means it hasn't been completely applied yet. This will return a curried version.
    // If it's a builtin that takes an arbitrary number of arguments - this will force final evaluation.
    case Cons(f: Function, LispNil) => f.result

    // If the first thing in the sexp is a function and there's an expression in the spot where the first argument should be, evaluate it.
    case Cons(f: Function, Cons(x: Expression, xs)) => interpret(Cons(f, Cons(interpret(x, scope), xs)), scope)

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
    case Cons(x: Expression, xs) => interpret(Cons(interpret(x, scope), xs), scope)
  }

  def interpret(expression: Term): Value = {
    interpret(expression, TopLevelScope.newChild)
  }
}
