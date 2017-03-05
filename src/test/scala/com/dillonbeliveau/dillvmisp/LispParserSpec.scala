package com.dillonbeliveau.dillvmisp

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dillon on 3/3/17.
  */
class LispParserSpec extends FlatSpec with Matchers {
  "parse" should "parse a floating point number" in {
    LispParser.quickParse("12.46") shouldBe LispNumber(12.46)
  }

  "parse" should "parse an integer" in {
    LispParser.quickParse("12") shouldBe LispNumber(12)
  }

  "parse" should "parse an integer with a trailing decimal point" in {
    LispParser.quickParse("12.") shouldBe LispNumber(12)
  }

  "parse" should "parse a string" in {
    LispParser.quickParse("\"hello\"") shouldBe LispString("hello")
  }

  "parse" should "parse a pair of strings" in {
    LispParser.quickParse("""("hello" "world")""") shouldBe Cons(LispString("hello"), Cons(LispString("world"), LispNil))
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

  "parse" should "parse a token and a list of tokens" in {
    LispParser.parse(LispParser.expression, "token") match {
      case LispParser.Success(LispToken(name), _) => name shouldBe "token"
    }
  }

  "parse" should "parse a list of numbers" in {
    LispParser.quickParse("(1 2 3)") shouldBe  Cons(LispNumber(1), Cons(LispNumber(2), Cons(LispNumber(3), LispNil)))
  }
}
