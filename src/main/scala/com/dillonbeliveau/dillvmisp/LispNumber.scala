package com.dillonbeliveau.dillvmisp

/**
  * Created by dillon on 3/1/17.
  */


case class LispNumber(number: Double) extends Value {
  override def evaluate: Value = this

  override def +(other: Value): Value = other match {
    case other: LispNumber => LispNumber(number + other.number)
  }
}
