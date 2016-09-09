package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  /******************
   ** Polynomial **
   ******************/

  test("discriminant of 3x^2 + 5x + 2") {
    val result = Polynomial.computeDelta(Var(3), Var(5), Var(2))
    assert(result() == 1)
  }

  test("roots of 3x^2 + 5x + 2") {
    val a = Signal(3.0)
    val b = Signal(5.0)
    val c = Signal(2.0)

    val delta = Polynomial.computeDelta(a, b, c)
    val result = Polynomial.computeSolutions(a, b, c, delta)

    assert(result() === Set(-0.6666666666666666, -1.0))
  }

  test("discriminant of 3x^2 - 6x + 3") {
    val result = Polynomial.computeDelta(Var(3.0), Var(-6.0), Var(3.0))
    assert(result() == 0)
  }

  test("roots of 3x^2 - 6x + 3") {
    val a = Signal(3.0)
    val b = Signal(-6.0)
    val c = Signal(3.0)

    val delta = Polynomial.computeDelta(a, b, c)
    val result = Polynomial.computeSolutions(a, b, c, delta)

    assert(result() === Set(1.0))
  }

  test("discriminant of x^2 + 2x + 5") {
    val result = Polynomial.computeDelta(Var(1.0), Var(2.0), Var(5.0))
    assert(result() == -16)
  }

  test("roots of x^2 + 2x + 5") {
    val a = Signal(1.0)
    val b = Signal(2.0)
    val c = Signal(5.0)

    val delta = Polynomial.computeDelta(a, b, c)
    val result = Polynomial.computeSolutions(a, b, c, delta)

    assert(result() === Set())
  }
}
