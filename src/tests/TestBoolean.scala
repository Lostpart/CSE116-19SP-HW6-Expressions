package tests

import expressions.Expressions
import org.scalatest._

class TestBoolean extends FunSuite {

  test("Test 2.1") {
    assert(Expressions.evaluateBoolean(" (true -> false) <> (false || false)"))
  }

  test("Test 2.2") {
    assert(!Expressions.evaluateBoolean(" true || false && false -> false xor true && false") )
  }

  test("Test 2.3") {
    assert(Expressions.evaluateBoolean(" false || false && false -> false xor true && false") )
  }
  test("Test 2.4") {
    assert(Expressions.evaluateBoolean(" ((false -> false) && (true -> true) )-> (true)") )
  }
}
