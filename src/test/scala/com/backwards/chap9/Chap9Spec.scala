package com.backwards.chap9

import scala.util.matching.Regex
import org.scalacheck._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalacheck.Prop.forAll

class Chap9Spec extends AnyWordSpec with Matchers

trait JSON

object JSON {
  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON
}

/**
 * Parser[+_] -> A type parameter that is itself a type constructor
 *
 * - string(s): Recognizes and returns a single String
 * - regex(s): Recognizes a regular expression s
 * - slice(p): Returns the portion of input inspected by p if successful
 * - succeed(a): Always succeeds with the value a
 * - flatMap(p)(f): Runs a parser, then uses its result to select a second parser to run in sequence
 * - or(p1, p2): Chooses between two parsers, first attempting p1, and then p2 if p1 fails
 */
trait Parsers[ParseError, Parser[+_]] { self => // This introduces the name self to refer to this Parsers instance; it’s used later in ParserOps.
  def run[A](p: Parser[A])(input: String): ParseError Either A

  def char(c: Char): Parser[Char] = // Here the Parser type constructor is applied to Char
    string(c.toString) map (_.charAt(0))

  // Because of "implicit", Scala will automatically promote a String to a Parser
  implicit def string(s: String): Parser[String]

  /**
   * What if we want to recognize either the string "abra" or the string "cadabra"? We could add a very specialized combinator for it:
   * def orString(s1: String, s2: String): Parser[String]
   * But choosing between two parsers seems like something that would be more generally useful, regardless of their result type,
   * so let’s go ahead and make this polymorphic:
   */
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def operators[A](p: Parser[A]): ParserOps[A] =
    ParserOps[A](p)

  // Here we get infix operators for any type that can be converted to a Parser[String].
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  // How would we recognize 3 repetitions of our "abra" | "cadabra" parser? Once again, let’s add a combinator for it:
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  // Parser that recognizes zero or more repetitions of the character 'a' and returns the number of characters it has seen.
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  // Note that the above results in Parser[List[A]] but we really wanted a count i.e. a Parser[Int]
  // Without changing "many" to be too specific and result in required Parser[Int], we can instead combine "many" with "map" resulting in a Parser[Int] e.g.
  val theCount: Parser[Int] = map(many(char('a')))(_.size)

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(f andThen succeed)

  // This parser always succeeds with the value a, regardless of the input string (since string("") will always succeed, even if the input is empty)
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => map(p2)(b => (a, b)))

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p1) { a =>
      map(p2) { b =>
        f(a, b)
      }
    }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  // Use "self" to explicitly disambiguate reference to the "or" method on the trait
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many[B >: A]: Parser[List[B]] = ???

    def map[B](f: A => B): Parser[B] = ???
  }

  object Laws {
    // char (function) law
    val c = 'a'

    run(char(c))(c.toString) == Right(c)

    // string (function) law
    val s = "abracadabra"

    run(string(s))(s) == Right(s)

    run(or(string("abra"), string("cadabra")))("abra")    == Right("abra")
    run(or(string("abra"), string("cadabra")))("cadabra") == Right("cadabra")

    // For nicer syntax than the above use of "or":
    // Given val P: Parsers, we can then import P._ to let us write expressions like "abra" | "cadabra" to create parsers e.g.
    val p: Parser[String] = "abra" | "cadabra"

    // listOfN (function) law
    run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
    run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
    run(listOfN(3, "ab" | "cad"))("ababab")  == Right("ababab")

    // many in conjunction with map
    val numA: Parser[Int] = char('a').many.map(_.size)

    run(numA)("aaa") == Right(3)
    run(numA)("b")   == Right(0)

    // map law
    map(p)(a => a) == p

    // succeed law
    run(succeed("a"))(s) == Right("a")

    // slice law
    run(slice(("a" | "b").many))("aaba") == Right("aaba")
  }

  object ScalacheckLaws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}