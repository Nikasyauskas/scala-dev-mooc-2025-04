package ru.otus.module2

import cats.data.{Chain, Ior, Kleisli, NonEmptyChain, NonEmptyList, NonEmptyVector, OptionT, State, Validated, ValidatedNec, Writer, WriterT}
import cats.implicits._

import scala.concurrent.Future
import scala.util.Try



object functional {

  def sum(a: Int, b: Int): (String, Int) = {
    val result = a + b
    val log = s"Sum result: $result"
    (log, result)
  }

  def double(v: Int): (String, Int) = {
    val result = v * 2
    val log = s"Doubling result: $result"
    (log, result)
  }

  def sumAndDouble(a: Int, b: Int): (String, Int) = {
    val (l1, r1) = sum(a, b)
    val (l2, r2) = double(r1)
    (l1 ++ l2, r2)
  }

  def sum2(a: Int, b: Int): Writer[Chain[String], Int] = {
    val result = a + b
    Writer.value[Chain[String], Int](result)
      .tell(Chain.one(s"Sum result: $result"))
  }

  def double2(v: Int): Writer[Chain[String], Int] = {
    val result = v * 2
    Writer.value[Chain[String], Int](result)
      .tell(Chain.one(s"Doubling result: $result"))
  }

  def sumAndDouble2(a: Int, b: Int): Writer[Chain[String], Int] =
    sum2(a, b).flatMap(r => double2(r))

  val wResult: (Chain[String], Int) = sumAndDouble2(3, 5).run

  println(wResult)




  var counter = 0
  final case class RegNumber private (value: String)

  object RegNumber {
    private val prefix = new StringBuilder("REG-O-")
    def apply(i: Int): RegNumber = new RegNumber(prefix.append(i).toString())
  }


  def regNumber1() = {
    counter += 1
    RegNumber(counter)
  }

  def regNumber2() = {
    counter += 1
    RegNumber(counter)
  }

  def regNumber3() = {
    counter += 1
    RegNumber(counter)
  }

  def regNumbers() = {
    List(regNumber1(), regNumber2(), regNumber3())
  }

  println(regNumbers())
  // f: (S, T) => T
  def regNumber1_1(): State[Int, RegNumber] = {
    State[Int, RegNumber](i => (i + 1, RegNumber(i + 1)))
  }

  def regNumber2_1() = {
    State[Int, RegNumber](i => (i + 1, RegNumber(i + 1)))
  }

  def regNumber3_1() = {
    State[Int, RegNumber](i => (i + 1, RegNumber(i + 1)))
  }

  def regNumbers2(): State[Int, List[RegNumber]] = for{
    r1 <- regNumber1_1()
    r2 <- regNumber2_1()
    r3 <- regNumber3_1()
  } yield List(r1, r2, r3)

  println(regNumbers2().run(0).value)



  // Kleisli
  // a => b
  // b => c
  // a => c

  val f1: String => Int = str => str.toInt
  val f2: Int => Int = i => 10 / i

  val f3: String => Int = f1 andThen f2

  val f4: String => Option[Int] = _.toIntOption
  val f5: Int => Option[Int] = i => Try(10 / i).toOption
  val f6: Kleisli[Option, String, Int] = Kleisli(f4) andThen Kleisli(f5)

  val cr1: Option[Int] = f6.run("2")
  val cr2: Option[Int] = f6.run("foo")
  println(cr1)
  println(cr2)



}



object dataStructures{

  // Chain

  val ch1 = Chain.one(1)
  val ch2 = Chain.empty[Int]
  val ch3 = Chain(2, 3)
  val ch4 = Chain.fromSeq(List(1, 2, 3))

  // операторы

  ch2 :+ 5
  5 +: ch2

  ch4.headOption

  // NonEmptyChain

  val nec: NonEmptyChain[Int] = NonEmptyChain.one(1)
  val ne2: NonEmptyChain[Int] = NonEmptyChain(1, 2)
  val nec3: Option[NonEmptyChain[Int]] =
    NonEmptyChain.fromSeq(List(1, 2))

  nec.head


}

object validation{

  type EmailValidationError = String
  type NameValidationError = String
  type AgeValidationError = String
  type Name = String
  type Email = String
  type Age = Int

  case class UserDTO(email: String, name: String, age: Int)
  case class User(email: String, name: String, age: Int)

  def emailValidatorE: Either[EmailValidationError, Email] = Left("Invalid email")
  def userNameValidatorE: Either[NameValidationError, Name] = Right("Bob")
  def ageValidatorE: Either[AgeValidationError, Age] = Left("Invalid age")


  def validateUserDTO(userDTO: UserDTO): Either[String, User] = for{
    email <- emailValidatorE
    name <- userNameValidatorE
    age <- ageValidatorE
  } yield User(email, name, age)

  println(validateUserDTO(UserDTO("fvf", "fvfvf", 10)))

  // Validated

  def emailValidatorV: Validated[EmailValidationError, Email] = Validated.invalid("Invalid email")
  def userNameValidatorV: Validated[NameValidationError, Name] = Validated.valid("Bob")
  def ageValidatorV: Validated[AgeValidationError, Age] = Validated.invalid("Invalid age")

//  def validateUserDTOV(userDTO: UserDTO): Validated[String, User] = for{
//    email <- emailValidatorV
//    name <- userNameValidatorV
//    age <- ageValidatorV
//  } yield User(email, name, age)

  def validateUserDTOV(userDTO: UserDTO): ValidatedNec[String, User] =
    (emailValidatorV.toValidatedNec, userNameValidatorV.toValidatedNec,
      ageValidatorV.toValidatedNec).mapN{ (email, name, age) =>
      User(email, name, age)
    }

  println(validateUserDTOV(UserDTO("fvf", "fvfvf", 10)))


  // IoR

  val ior = Ior.Left("Error")
  val ior2 = Ior.Right("Bob")
  val ior3 = Ior.Both("Warning", "")

  def emailValidatorI: Ior[EmailValidationError, Email] = Ior.Left("Invalid email")
  def userNameValidatorI: Ior[NameValidationError, Name] = Ior.Right("Bob")
  def ageValidatorI: Ior[AgeValidationError, Age] = 30.rightIor[String]

}



object transformers {

  val f1: Future[String] = Future.successful("2")
  def f2(str: String): Future[Option[Int]] = Future.successful(Try(str.toInt).toOption)
  def f3(i: Int): Future[Option[Int]] = Future.successful(Try(10 / i).toOption)

  import scala.concurrent.ExecutionContext.Implicits.global

  val r: OptionT[Future, Int] = for{
    i1 <- OptionT.liftF(f1)
    i2 <- OptionT(f2(i1))
    i3 <- OptionT(f3(i2))
  } yield i2 + i3

  val rr: Future[Option[Int]] = r.value


}