package com.dillonbeliveau.dillvmisp

/**
  * Created by dillon on 3/1/17.
  */
trait Value extends Expression {
  def +(other: Value): Value
}
