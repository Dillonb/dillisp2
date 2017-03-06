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
}
