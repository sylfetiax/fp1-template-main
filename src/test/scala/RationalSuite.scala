package com.tkroman.kpi.y2022.l1

import munit.FunSuite

class RationalSuite extends FunSuite {
  //negative denominator
  test("negative denominator") {
    val expected = Rational(-1, 2)
    val actual = Rational(1, -2)
    assertEquals(actual, expected)
  }
  test("negative denominator and nominator") {
    val expected = Rational(1, 2)
    val actual = Rational(-1, -2)
    assertEquals(actual, expected)
  }
  //normalize
  test("normalize") {
    val expected = Rational(4, 7)
    val actual = Rational(28, 49)
    assertEquals(actual, expected)
  }
  test("normalize negative") {
    val expected = Rational(-4, 7)
    val actual = Rational(-28, 49)
    assertEquals(actual, expected)
  }
  test("normalize zero") {
    val expected = Rational(0, 1)
    val actual = Rational(0, 17649)
    assertEquals(actual, expected)
  }
  //gcd
  test("gcd with zero") {
    val expected = 5
    val actual = gcd(0, 5)
    assertEquals(actual, expected)
  }
  test("gcd with negative") {
    val expected = -5
    val actual = gcd(-5, 0)
    assertEquals(actual, expected)
  }
  test("gcd") {
    val expected = 1
    val actual = gcd(636, 391)
    assertEquals(actual, expected)
  }
  //add
  test("add negative and positive") {
    val expected = Rational(1, 6)
    val actual = Rational(-1, 3).add(Rational(1, 2))
    assertEquals(actual, expected)
  }
  test("add negative and negative") {
    val expected = Rational(-5, 6)
    val actual = Rational(-1, 3).add(Rational(-1, 2))
    assertEquals(actual, expected)
  }
  test("add zero") {
    val expected = Rational(4, 3)
    val actual = Rational(4, 3).add(Rational(0, 4))
    assertEquals(actual, expected)
  }
  //mul
  test("mul zero") {
    val expected = Rational(0, 1)
    val actual = Rational(0, 21).mul(Rational(43, 2))
    assertEquals(actual, expected)
  }
  test("mul negative") {
    val expected = Rational(-1, 6)
    val actual = Rational(-1, 2).mul(Rational(1, 3))
    assertEquals(actual, expected)
  }
  //to string
  test("to string") {
    val expected = "-1/2"
    val r = Rational(1, -2)
    val actual = r.ToString
    assertEquals(actual, expected)
  }
}
