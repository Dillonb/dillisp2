package com.dillonbeliveau.dillvmisp.interpreter.builtins

import com.dillonbeliveau.dillvmisp.interpreter.Function
import com.dillonbeliveau.dillvmisp.lang._

/**
  * Created by Dillon on 3/18/17.
  */
class TimesFunction(value: LispNumber) extends Function {
  override def apply(arg: Value): Value = arg match {
    case LispNumber(multiplicand) => new TimesFunction(LispNumber(multiplicand * value.number))
  }

  override def result: Value = value
}

