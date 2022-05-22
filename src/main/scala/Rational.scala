package com.tkroman.kpi.y2022.l1

import scala.annotation.targetName

case class Rational(var nom: Int, var denom: Int){
  assert(denom!=0)
  private val g = gcd (nom, denom)
  denom = denom / g
  nom = nom / g
  if (denom<0) {
    denom = -1 * denom
    nom = -1 * nom
  }
}

def ToString(r: Rational): String = {
  r.nom + "/" + r.denom
}

def gcd(a: Int, b: Int): Int = {
  if(b == 0) a else gcd(b, a%b)
  }

def add(l: Rational, r: Rational): Rational =
    Rational(l.nom * r.denom + r.nom * l.denom, l.denom * r.denom)


def mul(l: Rational, r: Rational): Rational =
  Rational(l.nom * r.nom, l.denom * r.denom)


def normalize(r: Rational): Rational = {
  Rational(r.nom / gcd(r.nom, r.denom), r.denom / gcd(r.nom, r.denom))
}

@main def run() =
  println(Rational(1, -2))




