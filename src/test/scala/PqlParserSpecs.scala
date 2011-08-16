package com.github.dmlap.pql

import org.specs2.mutable._
import scala.util.parsing.combinator.syntactical._

class PqlParserSpecs extends Specification {
  import Pql._
  "A PQL parser" should {
    "parse a trivial query" in {
      PqlParser.parseText("P(Person)") mustEqual MarginalProbability(EventExpression(PathEvent(List("Person"))))
    }
  }
}
