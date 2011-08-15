package com.github.dmlap.pql

import org.specs2.mutable._

class PqlParserSpecs extends Specification {
  import PqlParser._
  "A PQL parser" should {
    "parse a trivial query" in {
      PathEvent(List("Person"))
      EventExpression(PathEvent(List("Person")))
      "P(Person)" mustEqual MarginalProbability(EventExpression(PathEvent(List("Person"))))
    }
  }
}
