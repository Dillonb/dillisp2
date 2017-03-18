package com.dillonbeliveau.dillvmisp.interpreter.builtins

import com.dillonbeliveau.dillvmisp.lang._
import com.dillonbeliveau.dillvmisp.interpreter.Function

/**
  * Created by Dillon on 3/18/17.
  */

class PlusFunction(value: LispNumber) extends Function {
  override def apply(arg: Value): Value = arg match {
    case LispNumber(addend) => {
      new PlusFunction(LispNumber(value.number + addend))
    }
  }

  override def toString: String = s"+${value.number}"

  override def result: Value = value
}
