package ru.otus

import ru.otus.module1.{concurrency, executors, future}
import ru.otus.module1.concurrency.{getRatesLocation1, getRatesLocation2, printRunningTime}
import ru.otus.module2.{catsTypeClasses, functional, implicits, type_classes, validation}
import ru.otus.module3.functional_effects.functionalProgram.{declarativeEncoding, executableEncoding}

import scala.util.{Failure, Success}


object Main {


  def main(args: Array[String]): Unit = {
    declarativeEncoding.run(declarativeEncoding.greet2)
  }
} 