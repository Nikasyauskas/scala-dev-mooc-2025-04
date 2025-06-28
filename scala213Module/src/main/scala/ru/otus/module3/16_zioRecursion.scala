package ru.otus.module3

import zio.ZIO
import zio.Task

import scala.io.StdIn

object zioRecursion {



  /** 
   * Создать ZIO эффект, который будет читать число в виде строки из консоли
   */

  lazy val readLine: Task[String] = ZIO.attempt(StdIn.readLine())



  /** *
   * Воспользовавшись readLine создайте ZIO, который прочитает строку из консоли
   * и вернет число (Int)
   */

  lazy val readInt: Task[Int] = readLine.map(str => str.toInt)
  lazy val read2Int: Task[Int] = readLine.flatMap(str => ZIO.attempt(str.toInt))


  /**
   * Написать программу, которая считывает из консоли Int введенный пользователем,
   * а в случае ошибки, сообщает о некорректном вводе, и просит ввести заново
   *
   */

   lazy val readIntOrRetry: ZIO[Any, Throwable, Int] = read2Int.orElse(
     ZIO.attempt(println("Не корректный ввод, попробуйте еще")) zipRight readIntOrRetry
   )




  /**
   * Считаем факториал
   */



  def factorial(n: BigInt): BigInt = {
    if(n <= 1) n
    else n * factorial(n - 1)
  }

  def fib(n: Long): Long = {
      if(n == 0 || n == 1) n
      else fib(n - 1) + fib(n - 2)
  }

  /**
   * Написать ZIO версию ф-ции факториала
   *
   */

  def factorialZ(n: BigInt): Task[BigInt] = {
    if(n <= 1) ZIO.succeed(n)
    else ZIO.succeed(n).zipWith(factorialZ(n - 1))(_ * _)
  }


}
