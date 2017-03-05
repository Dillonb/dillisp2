package com.dillonbeliveau.dillvmisp

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dillon on 3/3/17.
  */
class LispParserSpec extends FlatSpec with Matchers {
  "parse" should "parse a floating point number" in {
    val result = LispParser.parse(LispParser.number, "12.46")
    val number = result match {
      case LispParser.Success(matched, _) => matched
    }
    number.number shouldBe 12.46
  }

  "parse" should "parse an integer" in {
    val result = LispParser.parse(LispParser.number, "12")
    val number = result match {
      case LispParser.Success(matched, _) => matched
    }
    number.number shouldBe 12
  }

  "parse" should "parse an integer with a trailing decimal point" in {
    val result = LispParser.parse(LispParser.number, "12.")
    val number = result match {
      case LispParser.Success(matched, _) => matched
    }
    number.number shouldBe 12
  }

  "parse" should "parse a string" in {
    val result = LispParser.parse(LispParser.string, "\"hello\"")
    val string = result match {
      case LispParser.Success(matched, _) => matched
    }
    string.text shouldBe "hello"
  }
}
