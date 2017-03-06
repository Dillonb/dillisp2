package com.dillonbeliveau.dillvmisp.interpreter

import com.dillonbeliveau.dillvmisp.lang.LispNumber
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Dillon on 3/5/17.
  */
class LispInterpreterSpec extends FlatSpec with Matchers {
  "interpret" should "add some numbers" in {
    LispInterpreter.interpret("(+ 1 2)") shouldBe LispNumber(3)
  }

  "interpret" should "add some numbers in nested expressions" in {
    LispInterpreter.interpret("(+ (+ 1 2 3) 4)") shouldBe LispNumber(10)
  }

  "interpret" should "apply a lambda function" in {
    LispInterpreter.interpret("((lambda (x) (+ x 1)) 2)") shouldBe LispNumber(3)
  }

  "do" should "evaluate all arguments and return the result of the last one" in {
    LispInterpreter.interpret("(do (+ 1 1) (+ 2 2) (+ 3 3))") shouldBe LispNumber(6)
  }

  "define" should "bind a value to a name" in {
    LispInterpreter.interpret("(do (define a 1) (+ a 1))") shouldBe LispNumber(2)
  }

  "interpret" should "apply a lambda function bound to a name" in {
    LispInterpreter.interpret("(do (define plusOne (lambda (x) (+ x 1))) (plusOne 2))") shouldBe LispNumber(3)
  }
}
