package ru.otus

import ru.otus.module2.catsHomework.{Branch, Leaf, treeFunctor, tryME}

import scala.util.{Failure, Success}


object Main {


  def main(args: Array[String]): Unit = {

    // TreeFunctor

    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val modifaedTree1 = treeFunctor.map(tree)(_ + 1)
    println(modifaedTree1)

    // MonadError

    println(tryME.flatMap(Success(2))((x: Int) => Success(x + 3)))
    println(tryME.flatMap(Failure(new Exception("error")))((x: Int) => Success(x + 1)))
    println(tryME.raiseError(new Exception("Custom error")))

    val errorTry = Failure(new Exception("Original error"))
    val handledWithRecovery = tryME.handleErrorWith(errorTry) { e =>
      println(s"Handling error: ${e.getMessage}")
      Success(42)
    }
    println(handledWithRecovery)

    val handledError = tryME.handleError(errorTry) { e =>
      println(s"Recovering from: ${e.getMessage}")
      100
    }
    println(handledError)

    val successTry = Success(5)
    println(tryME.ensure(successTry)(new Exception("Number too small"))(_ > 3))
    println(tryME.ensure(successTry)(new Exception("Number too small"))(_ > 10))

    
  }
} 