package ru.otus.module2

import java.util.Date
import scala.language.postfixOps
import cats.{Contravariant, Functor, Monad, Monoid, Semigroup, Show}
import cats.implicits._

object catsTypeClasses {


//  trait Show[T] {
//    def show(v: T): String
//  }
//
//  object Show {
//    def apply[T](implicit ev: Show[T]): Show[T] = ev
//
//    def from[T](f: T => String): Show[T] = v => f(v)
//
//    def fromToString[T]: Show[T] = v => v.toString
//
//    implicit val showInt: Show[Int] = fromToString[Int]
//
//    implicit val showDate = from[Date](d => s"From epoch start: ${d.getTime}")
//  }
//
//  object showSyntax {
//    implicit class ShowOps[T](v: T) {
//      def show(implicit ev: Show[T]): String = ev.show(v)
//    }
//  }
//
//  import showSyntax._

  println(10.show)

  implicit val showDate = Show.show[Date](d => s"${d.getTime} since epoch")
  val d1 = new Date(1000l)
  println(d1.show)


  // Monoid

//  trait Semigroup[T] {
//    def combine(a: T, b: T): T
//  }
//
//  object Semigroup {
//    def apply[T](implicit ev: Semigroup[T]): Semigroup[T] = ev
//    implicit val intSemigroup: Semigroup[Int] = (a, b) => a + b
//  }

  // Задача мержа 2-х Map
  val m1 = Map("a" -> 1, "b" -> 2)
  val m2 = Map("b" -> 3, "c" -> 4)
  // val m3 = Map("a" -> 1, "b" -> 5, "c" -> 4)

  def mergeOpt[V: Semigroup](v1: V, optV2: Option[V]): V = optV2 match {
    case Some(v2) => Semigroup[V].combine(v1, v2)
    case None => v1
  }


  def merge[K, V: Semigroup](m1: Map[K, V], m2: Map[K, V]): Map[K, V] =
    m1.foldLeft(m2){ case (acc, (k, v)) =>
      acc.updated(k, mergeOpt(v, acc.get(k)))
    }

  val m3 = merge(m1, m2)
  println(m3)

  // Monoid

//  trait Monoid[T] {
//    def combine[T](a: T, b: T): T
//    def empty: T
//  }

  def combineAll[A: Monoid](l: List[A]): A =
    l.foldLeft(Monoid[A].empty)(Monoid[A].combine(_, _))

  println(combineAll(List(1, 3, 5, 8)))

  def doMath[F[_]](start: F[Int])(implicit ev: Functor[F]) =
    start.map(v => v + 1 * 2)

  println(doMath(Option(2)))
  println(doMath(List(1, 2, 3)))

  val f1: Int => String = _.show
  val f2: String => Unit = println
  val f3: Int => Unit = f1 andThen f2
  val f4: Int => Unit = f1 map f2

  f3(10)
  f4(10)

  // Contravariant
  class Id(val raw: String)
  class User(val id: Id)

  implicit val showId = Show.show[Id](_.raw)
  implicit val showUser: Show[User] = Contravariant[Show].contramap[Id, User](showId)(v => v.id)  // B => A: F[B]

  println(new User(new Id("1")).show)

  // Invariant

  implicit val dateSemi: Semigroup[Date] = Semigroup[Long].imap(l => new Date(l))(d => d.getTime)
  val now = new Date(1000L)
  val timeLeft = new Date(1000L)
  val result = now |+| timeLeft
  println(result.show)


  // Monad
//  trait Monad[F[_]] {
//    def pure[A](v: A): F[A]
//    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
//  }

   val monad1: Option[Int] = Monad[Option].pure(10)
   val monad2: Option[Int] =  Monad[Option].flatMap(monad1)(v => Monad[Option].pure(v * 2))
}