package com.dillonbeliveau.dillvmisp

/**
  * Created by dillon on 3/1/17.
  */

case class LispString(val text: String) extends Value {
  override def evaluate: Value = this

  override def +(other: Value): Value = other match {
    case other: LispNumber => LispString(text + other.number)
    case other: LispString => LispString(text + other.text)
  }
}
