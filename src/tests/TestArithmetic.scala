package tests
import expressions.Expressions
import org.scalatest._

class TestArithmetic extends FunSuite {
  val EPSILON: Double = 0.000001

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  test("Test 1") {
    assert(Expressions.evaluateArithmetic(" (12-4) - (8+9/3) ")=="-3".toDouble)
  }

  test("Test 2 with space") {
    assert(Expressions.evaluateArithmetic("  2.5  +  2.5  ")=="5.0".toDouble)
    assert(Expressions.evaluateArithmetic("2.5+2.5")=="5.0".toDouble)
  }

  test("Test 3") {
    assert(Expressions.evaluateArithmetic("3+5*2")=="13".toDouble)
  }

  test("Test 4") {
    assert(equalDoubles(Expressions.evaluateArithmetic("7/3+3"),"5.33333333".toDouble))
  }

  test("Test 5") {
    assert(equalDoubles(Expressions.evaluateArithmetic("3*(5-1)"),"12".toDouble))
  }

  test("Test 6") {
    assert(Expressions.evaluateArithmetic("10 - (8/12.0*6)/2-1")=="7.0".toDouble)
  }
  test("Test 7") {
    assert(Expressions.evaluateArithmetic("(3+4)*3")=="21".toDouble)
  }
  test("Test 8") {
    assert(Expressions.evaluateArithmetic("1^2+3*4/5")=="3.4".toDouble)
  }
  test("Test 9") {
    assert(Expressions.evaluateArithmetic("((12-4)-(8+(9/3)))")=="-3".toDouble)
  }
  test("Test 10") {
    assert(Expressions.evaluateArithmetic("278+1606-(3627-2832)")=="1089".toDouble)
    assert(Expressions.evaluateArithmetic("1738-(4893-4432)+1555")=="2832".toDouble)
    assert(Expressions.evaluateArithmetic("1920+(1080/7)-16*9/7+(1*7-12+(8*7)/10)+5")=="2059.3142857142857".toDouble)
  }

}

