package com.github.dmlap.pql

import org.specs2.mutable._
import scala.util.parsing.combinator.syntactical._

class PqlParserSpecs extends Specification {
  import PqlParser._
  import Pql._
  "A PQL parser" should {
    "parse a trivial query" in {
      term(marginalProbabilityW)(new lexical.Scanner("P(Person)")) mustEqual MarginalProbability(EventExpression(PathEvent(List("Person"))))
    }
  }
}
