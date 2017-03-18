package com.dillonbeliveau.dillvmisp.interpreter.builtins

import com.dillonbeliveau.dillvmisp.interpreter.Function
import com.dillonbeliveau.dillvmisp.lang._

/**
  * Created by Dillon on 3/18/17.
  */
class ModuloFunction extends Function {
  override def apply(arg: Value): Value = arg match {
    case number: LispNumber => new AppliedModuloFunction(number)
  }

  override def result: Value = ???
}

class AppliedModuloFunction(number: LispNumber) extends ModuloFunction {
  override def apply(arg: Value): Value = arg match {
    case LispNumber(divisor) => new AppliedModuloFunction(LispNumber(number.number % divisor))
  }

  override def result: Value = number
}
