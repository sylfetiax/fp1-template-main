
package com.tkroman.kpi.y2022.l1

import scala.collection.mutable.ListBuffer

import scala.annotation.targetName

case class Rational private(val nom: Int, val denom: Int)


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

def rationalToString(r: Rational): String = {
  r.nom + "/" + r.denom
}

def gcd(a: Int, b: Int): Int = {
  if (b == 0) a else gcd(b, a % b)
}

def add(l: Rational, r: Rational): Rational =
  Rational(l.nom * r.denom + r.nom * l.denom, l.denom * r.denom)


def mul(l: Rational, r: Rational): Rational =
  Rational(l.nom * r.nom, l.denom * r.denom)


def normalize(r: Rational): Rational = {
  Rational(r.nom / gcd(r.nom, r.denom), r.denom / gcd(r.nom, r.denom))
}

def floor(x: Double): Double = java.lang.Math.floor(x)

def continuedFraction(r: Rational): ListBuffer[Int] = {
  var listFraction = new ListBuffer[Int]()
  listFraction.append(r.nom / r.denom)
  recursiveFraction(r.nom.toDouble / r.denom.toDouble, 1, listFraction)
}

def recursiveFraction(prev: Double, curr: Double, list: ListBuffer[Int]): ListBuffer[Int] = {
  val next: Double = prev % curr
  if (BigDecimal(next).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble != 0) {
    list.append(floor(curr / next).toInt)
    recursiveFraction(curr, next, list)
  } else list
}


@main def run() =
  println(continuedFraction(Rational(314, 100)))


