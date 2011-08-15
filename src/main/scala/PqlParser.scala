package com.github.dmlap.pql

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
object PqlParser {
  trait Liftable[B[_]] {
    type T
    def apply[A]: B[A =:= T]
  }

  sealed trait Probability[P]
  sealed case class MarginalProbability[F, E[_]](expression: E[F])(implicit witness: Liftable[E, Expression])
  sealed case class ConditionalProbability[E, C](expression: E, condition: C)(implicit witnessE: Expression[E], witnessC: Expression[C])
  implicit object ConditionalProbability extends Probability[ConditionalProbability[_, _]]

  sealed trait Expression[E]
  sealed case class BinaryExpression[L, O, R](left: L, op: O, right: R)(implicit witnessLeft: Expression[L], witnessRight: Expression[R])
  implicit object BinaryExpressionW extends Expression[BinaryExpression[_, _, _]]
  sealed case class NegationExpression[E](expression: E)(implicit witness: Expression[E])
  implicit object NegationExpressionW extends Expression[NegationExpression[_]]
  sealed case class EventExpression[F, E[_]](event: E[F])(implicit witness: Liftable[E, Event])
  implicit object EventExpressionW extends Liftable[EventExpression, Expression]
  //implicit def eventExpressionW[E](ee: EventExpression[E]): Expression[EventExpression[E]] =
    //new Expression[EventExpression[E]] {}

  sealed trait Event[E]
  sealed case class PathEvent[P: Path](path: P)
  implicit object PathEventW extends Liftable[Event]
  //implicit def pathEventW[P](pe: PathEvent[P]): Event[PathEvent[P]] = new Event[PathEvent[P]] {}
  sealed case class ConditionalEvent[P, C](path: P, condition: C)(implicit witnessP: Path[P], witnessC: Condition[C])
  implicit object ConditionalEventW extends Event[ConditionalEvent[_, _]]

  sealed trait Path[P]
  implicit object ListPathW extends Path[List[String]]
  sealed trait Condition[C]
  case class NumericCondition[R, N](relation: R, number: N)(implicit witnessN: Integral[N])
  implicit object NumericConditionW extends Condition[NumericCondition[_, _]]
  case class StringCondition(string: String)
  implicit object StringConditionW extends Condition[StringCondition]

  // Path[List[String]]
  // PathEvent[P] => Event[PathEvent[P]]
  // forall P . Path[P] => PathEvent[P]
  // forall P . PathEvent[P] => Event[PathEvent[P]]
  // --> Path[List[String]] => PathEvent[List[String]]
  // --> PathEvent[List[String]] => Event[PathEvent[List[String]]]

  sealed trait Integral[N]
  implicit object LongW extends Integral[Long]
}
