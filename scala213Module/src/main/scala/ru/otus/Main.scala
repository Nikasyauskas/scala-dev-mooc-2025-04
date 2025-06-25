package ru.otus

import ru.otus.module1.{concurrency, executors, future}
import ru.otus.module1.concurrency.{getRatesLocation1, getRatesLocation2, printRunningTime}
import ru.otus.module2.{catsTypeClasses, functional, implicits, type_classes, validation}
import ru.otus.module2.catsHomework.{Branch, Leaf, treeFunctor}
import ru.otus.module3.functional_effects.functionalProgram.{declarativeEncoding, executableEncoding}

import scala.util.{Failure, Success}


object Main {


  def main(args: Array[String]): Unit = {

    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val modifaedTree1 = ru.otus.module2.catsHomework.treeFunctor.map(tree)(_ + 1)
    println(modifaedTree1)
  }
} 