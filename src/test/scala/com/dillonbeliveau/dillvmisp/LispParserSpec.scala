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

  "parse" should "parse a pair of strings" in {
    val result = LispParser.parse(LispParser.expression, """("hello" "world")""")
    val consTree = result match {
      case LispParser.Success(matched, _) => matched
    }
    consTree match {
      case Cons(LispString(hello), Cons(LispString(world), LispNil)) => {
        hello shouldBe "hello"
        world shouldBe "world"
      }
    }
  }


  "parse" should "parse a string tree" in {
    val result = LispParser.parse(LispParser.expression, """("hello" ("world" "i" ("said" "hello")))""")
    val consTree = result match {
      case LispParser.Success(matched, _) => matched
    }
    consTree match {
      case Cons(
            LispString(hello),
            Cons(
              Cons(
                LispString(world),
                Cons(
                  LispString(i),
                  Cons(
                    Cons(
                      LispString(said),
                      Cons(
                        LispString(hello2),
                        LispNil)
                    ),
                    LispNil
                  )
                )
              ),
              LispNil)) => {
        hello shouldBe "hello"
        world shouldBe "world"
        i shouldBe "i"
        said shouldBe "said"
        hello2 shouldBe "hello"
      }
    }
  }

  "parse" should "parse a tree-shaped tree" in {
    val result = LispParser.parse(LispParser.expression, """(("a" "b") ("c" "d"))""")
    val consTree = result match {
      case LispParser.Success(matched, _) => matched
    }
    consTree match {
      case Cons(Cons(LispString(a), Cons(LispString(b), LispNil)), Cons(Cons(LispString(c), Cons(LispString(d), LispNil)), LispNil)) => {
        a shouldBe "a"
        b shouldBe "b"
        c shouldBe "c"
        d shouldBe "d"
      }
    }

  }
}
