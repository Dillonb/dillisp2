package com.dillonbeliveau.dillvmisp.interpreter.builtins

import com.dillonbeliveau.dillvmisp.interpreter.Function
import com.dillonbeliveau.dillvmisp.lang._

/**
  * Created by Dillon on 3/18/17.
  */
class DivideFunction extends Function {
  override def apply(arg: Value): Value = arg match {
    case number: LispNumber => new AppliedDivideFunction(number)
  }

  override def result: Value = LispNil
}

class AppliedDivideFunction(number: LispNumber) extends DivideFunction {
  override def apply(arg: Value): Value = arg match {
    case LispNumber(divisor) => new AppliedDivideFunction(LispNumber(number.number / divisor))
  }

  override def result: Value = number
}
