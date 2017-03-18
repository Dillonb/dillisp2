package com.dillonbeliveau.dillvmisp.interpreter.builtins

import com.dillonbeliveau.dillvmisp.lang._
import com.dillonbeliveau.dillvmisp.interpreter.Function

/**
  * Created by Dillon on 3/18/17.
  */

class MinusFunction extends Function {
  override def apply(arg: Value): Value = arg match {
    case subtrahend: LispNumber => new AppliedMinusFunction(subtrahend)
  }

  override def result: Value = LispNil
}

class AppliedMinusFunction(value: LispNumber) extends MinusFunction {
  override def apply(arg: Value): Value = arg match {
    case LispNumber(subtrahend) => new AppliedMinusFunction(LispNumber(value.number - subtrahend))
  }

  override def result: Value = value
}
