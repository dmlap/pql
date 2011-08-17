package com.github.dmlap.pql

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}
import scala.util.parsing.input.CharSequenceReader

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
 */
object PqlParser extends JavaTokenParsers with PackratParsers {
  import Pql._

  def parseText(text: String): Either[String, Probability] =
    probability(new PackratReader(new CharSequenceReader(text))) match {
      case Success(probability, _) => Right(probability)
      case Failure(msg, _) => Left(msg)
    }
  
  lazy val path: PackratParser[Path] = 
    (ident ~ ("." ~> ident*)) ^^ {
      case i ~ path => i :: path
    }

  def ncond: Parser[NumericCondition[Relation, Long]] = (
    "<" ~ wholeNumber ^^ { case "<" ~ n => NumericCondition[Relation, Long](Lt, n.toLong) }
    | "<=" ~ wholeNumber ^^ { case "<=" ~ n => NumericCondition[Relation, Long](Lte, n.toLong) }
    | "=" ~ wholeNumber ^^ { case "=" ~ n => NumericCondition[Relation, Long](Eq, n.toLong) }
    | ">=" ~ wholeNumber ^^ { case ">=" ~ n => NumericCondition[Relation, Long](Gte, n.toLong) }
    | ">" ~ wholeNumber ^^ { case ">" ~ n => NumericCondition[Relation, Long](Gt, n.toLong) }
  )
  def scond: Parser[StringCondition] = "=" ~> stringLiteral ^^ {
    case string => StringCondition(string)
  }
  def cond: Parser[Condition] = ncond | scond
  def event: Parser[Event] = (
    path ~ cond ^^ { case path ~ cond => ConditionalEvent(path, cond) }
    | path ^^ { case path => PathEvent(path) }
  )
  def binOp: Parser[Operation] = (
    "and" ^^^ And
    | "or" ^^^ Or
  )
  lazy val expr: PackratParser[Expression] = (
    "not" ~ expr ^^ { case _ ~ expr => NegationExpression(expr) }
    | event ^^ { case event => EventExpression(event) }
  )
  lazy val opExpr: PackratParser[Expression] = (
    (opExpr ~ binOp ~ "(" ~ opExpr ~ ")" ^^ {
      case left ~ op ~ _ ~ right ~ _ => BinaryExpression(left, op, right)
    }
     | opExpr ~ binOp ~ expr ^^ {
      case left ~ op ~ right => BinaryExpression(left, op, right)
    }
     | expr)
  )
  lazy val probability: PackratParser[Probability] = (
    "P(" ~ opExpr ~ "|" ~ opExpr ~ ")" ^^ {
      case _ ~ expr ~ _ ~ cond ~ _ => ConditionalProbability(expr, cond)
    }
    | "P(" ~ opExpr ~ ")" ^^ {
      case _ ~ expr ~ _ => MarginalProbability(expr)
    }
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
}
