package com.tkroman.kpi.y2022.l1

import com.tkroman.kpi.y2022.l1

import scala.annotation.targetName

case class Rational(private val nom: Int, private val denom: Int) {
  def ToString: String = nom + "/" + denom

  def add(that: Rational) =
    Rational(
      this.nom * that.denom + that.nom * this.denom,
      this.denom * that.denom
    )

  def mul(that: Rational): Rational =
    Rational(
      this.nom * that.nom,
      this.denom * that.denom
    )

  def normalize: Rational =
    Rational(
      nom / gcd(nom, denom),
      denom / gcd(nom, denom)
    )
}

def gcd(a: Int, b: Int): Int = {
  if (b == 0) a else gcd(b, a % b)
}

object Rational {
  def apply(nom: Int, denom: Int): Rational = {
    assert(denom != 0)
    val g = gcd(nom, denom)
    if ((denom / g) < 0) {
      new Rational(-1 * nom / g, -1 * denom / g)
    } else
      new Rational(nom / g, denom / g)
  }
}

@main def run() =
  println(Rational(10, -2))

