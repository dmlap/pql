package com.github.dmlap.pql

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.PackratParsers

/**
 * A probability query language.
 *
 * Assuming the probability should be across the entire data set as opposed
 * to just the types referenced in the query.
 * Assuming that objects with identical composition are identical.
 * For the most part, I'm ignoring efficiency of the data store
 * implementation. Language features are selected primarily for the purpose of
 * providing a simple and familiar syntax, not to match up with features of a
 * particular storage system.
 *
 * see http://en.wikipedia.org/wiki/Probability#Mathematical_treatment
 * see http://en.wikipedia.org/wiki/Probability_axioms
 * see http://www.json.org
 * see http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-262.pdf
 * 
 * Model:
 * Undefined: a unique value
 * String: as defined by json.org
 * Number: an integer, some n <- {..., -1, -2, 0, 1, 2, ...}
 * Identifier: as defined in Ecma-262
 * obj(f): Identifier => (String | Number | Object | Undefined)
 * type(f): Identifier => Set[Object]
 * dist: forall f. Union(type(f))
 *
 * Grammar:
 * Query := "P" Expr
 * Expr := "(" WS* Term WS* ")" | "(" WS* Term WS* "|" WS* Term ")"
 * Term := Expr WS+ BinOp WS+ Expr | "not" WS+ Expr | Event
 * BinOp := "and" | "or"
 *
 * Identifier := as defined in Ecma-262
 * Event := Path | Path WS+ Cond
 * Path := Identifier | Identifier "." Identifier
 * Cond := NCond | SCond
 * NCond := "<" WS+ Number | "<=" WS+ Number | "=" WS+ Number | ">=" WS+ Number | ">" WS+ Number
 * SCond := "=" WS+ """ String """
 * WS := a whitespace character
 */
object PqlParser extends StandardTokenParsers with PackratParsers {
  import Pql._
  lexical.delimiters ++= List("(", ")")
  lexical.reserved += ("P", "not", "and", "or", "|")

  def parseText(text: String): ParseResult[Probability] = probability(new PackratReader(new lexical.Scanner(text)))
  
  def path: PackratParser[Path] = (
      ident ~ "." ~ ident ^^ { case i ~ _ ~ j => println("paths"); List(i, j) }
    | ident ^^ { case i => println("path"); List(i) }
  )
  def ncond: PackratParser[NumericCondition[Relation, Long]] = (
    "<" ~ numericLit ^^ { case "<" ~ n => NumericCondition[Relation, Long](Lt, n.toLong) }
    | "<=" ~ numericLit ^^ { case "<=" ~ n => NumericCondition[Relation, Long](Lte, n.toLong) }
    | "=" ~ numericLit ^^ { case "=" ~ n => NumericCondition[Relation, Long](Eq, n.toLong) }
    | ">=" ~ numericLit ^^ { case ">=" ~ n => NumericCondition[Relation, Long](Gte, n.toLong) }
    | ">" ~ numericLit ^^ { case ">" ~ n => NumericCondition[Relation, Long](Gt, n.toLong) }
  )
  def scond: PackratParser[StringCondition] = "=" ~> stringLit ^^ {
    case string => StringCondition(string)
  }
  def cond: PackratParser[Condition] = ncond | scond
  def event: PackratParser[Event] = (
    path ^^ { case path => println("path event"); PathEvent(path) }
    | path ~ cond ^^ { case path ~ cond => println("cond event"); ConditionalEvent(path, cond) }
  )
  def binOp: Parser[Operation] = (
    "and" ^^^ And
    | "or" ^^^ Or
    )
  def expr: PackratParser[Expression] = (
    "not" ~ expr ^^ { case _ ~ expr => println("not e"); NegationExpression(expr) }
    | event ^^ { case event => println("ev"); EventExpression(event) }
    | expr ~ binOp ~ expr ^^ { case left ~ op ~ right => println("l op r"); BinaryExpression(left, op, right) }
    )
  def probability: PackratParser[Probability] = (
    "P" ~ expr ^^ { case _ ~ expr => println("marginal p"); MarginalProbability(expr) }
    | "P" ~ expr ~ "|" ~ expr ^^ { case _ ~ expr ~ _ ~ cond => println("conditional p"); ConditionalProbability(expr, cond) }
  )
}

object Pql {
  sealed trait Probability
  final case class MarginalProbability[E <: Expression](expression: E) extends Probability
  final case class ConditionalProbability[E <: Expression, C <: Expression](expression: E, condition: C) extends Probability

  sealed trait Expression
  final case class BinaryExpression[L <: Expression, O, R <: Expression](left: L, op: O, right: R) extends Expression
  final case class NegationExpression[E <: Expression](expression: E) extends Expression
  final case class EventExpression[E <: Event](event: E) extends Expression

  sealed trait Operation
  object And extends Operation
  object Or extends Operation

  sealed trait Event
  final case class PathEvent[P <: Path](path: P) extends Event
  final case class ConditionalEvent[P <: Path, C <: Condition](path: P, condition: C) extends Event

  type Path = List[String]
  sealed trait Condition
  final case class NumericCondition[R <: Relation, N: Numeric](relation: R, number: N) extends Condition
  final case class StringCondition(string: String) extends Condition

  sealed trait Relation
  object Lt extends Relation
  object Lte extends Relation
  object Eq extends Relation
  object Gte extends Relation
  object Gt extends Relation

  type Identifier = String
}
