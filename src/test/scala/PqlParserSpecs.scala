package com.github.dmlap.pql

import org.specs2.mutable._
import scala.util.parsing.combinator.syntactical._

class PqlParserSpecs extends Specification {
  colors(new org.specs2.reporter.InvertedColors())
  import Pql._
  "A PQL parser" should {
    "parse a trivial event query: \"P(Person)\"" in {
      PqlParser.parseText("P(Person)") must beRight(MarginalProbability(EventExpression(PathEvent(List("Person")))))
    }
    "parse an event query: \"P(Person.name)\"" in {
      PqlParser.parseText("P(Person.name)") must beRight(MarginalProbability(EventExpression(PathEvent(List("Person", "name")))))
    }
    "parse a query with a binary expression: \"P(Person and Person)\"" in {
      val personEvent = EventExpression(PathEvent(List("Person")))
      PqlParser.parseText("P(Person and Person)") must beRight(MarginalProbability(BinaryExpression(personEvent, And, personEvent)))
    }
    "parse a trivial conditional query: \"P(Person|Person)\"" in {
      val personEvent = EventExpression(PathEvent(List("Person")))
      PqlParser.parseText("P(Person|Person)\"") must beRight(ConditionalProbability(personEvent, personEvent))
    }
    "parse a conditional query with binary expressions: \"P(Person or Person|Person and Person)\"" in {
      val personEvent = EventExpression(PathEvent(List("Person")))
      PqlParser.parseText("P(Person or Person|Person and Person)\"") must beRight(ConditionalProbability(BinaryExpression(personEvent, Or, personEvent), BinaryExpression(personEvent, And, personEvent)))
    }
    "parse a query with a string-equality event condition: \"P(Person.name = \"gob\")\"" in {
      PqlParser.parseText("P(Person.name = \"gob\")") must beRight(MarginalProbability(EventExpression(ConditionalEvent(List("Person", "name"), StringCondition("\"gob\"")))))
    }
    "parse numeric-relation event conditions" >> {
      def numericRelationSpec(token: String, relation: Relation) = {
        PqlParser.parseText("P(Person.age " + token + "10)") must beRight(MarginalProbability(EventExpression(ConditionalEvent(List("Person", "age"), NumericCondition(relation, 10L)))))
      }
      "less-than: \"P(Person.age < 10)\"" in {
        numericRelationSpec("<", Lt)
      }
      "less-than-or-equal-to: \"P(Person.age <= 10)\"" in {
        numericRelationSpec("<=", Lte)
      }
      "equal-to: \"P(Person.age = 10)\"" in {
        numericRelationSpec("=", Eq)
      }
      "greater-than-or-equal-to: \"P(Person.age >= 10)\"" in {
        numericRelationSpec(">=", Gte)
      }
      "greater-than: \"P(Person.age > 10)\"" in {
        numericRelationSpec(">", Gt)
      }
    }
    "parse a multi-expression query: \"P(Person and Person or Person)\"" in {
      val personEvent = EventExpression(PathEvent(List("Person")))
      PqlParser.parseText("P(Person and Person or Person)") must beRight {
        MarginalProbability(
          BinaryExpression(
            BinaryExpression(
              personEvent,
              And,
              personEvent),
            Or,
            personEvent))
      }
    }
    "parse a query with sub-expressions: \"P(Person and (Person or Person))\"" in {
      val personEvent = EventExpression(PathEvent(List("Person")))
      PqlParser.parseText("P(Person and (Person or Person))") must beRight {
        MarginalProbability(
          BinaryExpression(
            personEvent,
            And,
            BinaryExpression(
              personEvent,
              Or,
              personEvent)))
      }
    }
  }
}
