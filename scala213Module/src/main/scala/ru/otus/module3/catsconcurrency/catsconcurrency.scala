import cats.effect._
import cats.implicits._
import scala.util.Try

sealed trait Command
object Command {
  case object Echo extends Command
  case object Exit extends Command
  case class AddNumber(num: Int) extends Command
  case object ReadNumber extends Command
  case class LaunchDog(name: String) extends Command
  case object ReleaseTheDogs extends Command

  def parse(s: String): Either[String, Command] =
    s.toLowerCase match {
      case "echo" => Right(Echo)
      case "exit" => Right(Exit)
      case "dogs" => Right(ReleaseTheDogs)
      case "read-number" => Right(ReadNumber)
      case cmd =>
        cmd.split(" ").toList match {
          case List("l", dogName) =>
            Right(LaunchDog(dogName))
          case List("n", IntString(num)) =>
            Right(AddNumber(num))
          case _ => Left("not a valid commmand")
        }
    }

  private object IntString {
    def unapply(s: String): Option[Int] =
      Try(s.toInt).toOption
  }
}

//1
/*
object MainCatsConcurrent extends IOApp.Simple {
  def program: IO[Unit] = for {
    cmd <- IO.readLine
    _ <- Command.parse(cmd) match {
      case Left(error) => IO.raiseError(new Exception(s"invalide command: $error"))
      case Right(command) => process(command)
    }
  } yield ()

  def process(command: Command): IO[Unit] =
    command match {
      case Command.Echo => {
        IO.readLine.flatMap(text => IO.println(text))
      }
      case Command.Exit => {
        IO.println("Bye Bye")
      }
      case Command.AddNumber(num) => ???
      case Command.ReadNumber => ???
      case Command.LaunchDog(name) => ???
      case Command.ReleaseTheDogs => ???
    }


  def run: IO[Unit] = program
}*/

//2 цикл комманд
/*
object MainCatsConcurrent extends IOApp.Simple {
  def program: IO[Unit] = for {
    cmd <- IO.readLine
    _ <- Command.parse(cmd) match {
      case Left(error) => IO.raiseError(new Exception(s"invalide command: $error"))
      case Right(command) => process(command).flatMap{
        case true => program
        case false => IO.unit
      }
    }
  } yield ()

  def process(command: Command): IO[Boolean] =
    command match {
      case Command.Echo => {
        IO.readLine.flatMap(text => IO.println(text)).as(true) // map(_=>true)
      }
      case Command.Exit => {
        IO.println("Bye Bye").as(false)
      }
      case Command.AddNumber(num) => ???
      case Command.ReadNumber => ???
      case Command.LaunchDog(name) => ???
      case Command.ReleaseTheDogs => ???
    }


  def run: IO[Unit] = program
}*/

//3 убираем рекурсию
/*
object MainCatsConcurrent extends IOApp.Simple {

  def program(counter: Ref[IO, Int]): IO[Unit] = iteration(counter).iterateWhile(a=>a).void

  def iteration(counter: Ref[IO, Int]): IO[Boolean] = for {
    cmd <- IO.println("> ") *> IO.readLine
    shouldProcess <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"invalide command: $err"))
      case Right(command) => process(counter)(command)
    }
  } yield shouldProcess

  def process(counter: Ref[IO, Int])(command: Command): IO[Boolean] =
    command match {
      case Command.Echo => {
        IO.readLine.flatMap(text => IO.println(text)).as(true) // map(_=>true)
      }
      case Command.Exit => {
        IO.println("Bye Bye").as(false)
      }
      case Command.AddNumber(num) => counter.updateAndGet(_+num).flatMap(IO.println).as(true)
      case Command.ReadNumber => counter.get.flatMap(IO.println).as(true)
      case Command.LaunchDog(name) => ???
      case Command.ReleaseTheDogs => ???
    }


  def run: IO[Unit] = for {
    counter <- Ref.of[IO, Int](0)
    _ <- program(counter)
  } yield ()
}
*/
//4 env patter
/*
object MainCatsConcurrent extends IOApp.Simple {
  final case class Environment(counter: Ref[IO, Int])

  def program(env: Environment): IO[Unit] = iteration(env).iterateWhile(a=>a).void

  def iteration(env: Environment): IO[Boolean] = for {
    cmd <- IO.println("> ") *> IO.readLine
    shouldProcess <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"invalide command: $err"))
      case Right(command) => process(env)(command)
    }
  } yield shouldProcess

  def process(env: Environment)(command: Command): IO[Boolean] =
    command match {
      case Command.Echo => {
        IO.readLine.flatMap(text => IO.println(text)).as(true) // map(_=>true)
      }
      case Command.Exit => {
        IO.println("Bye Bye").as(false)
      }
      case Command.AddNumber(num) => env.counter.updateAndGet(_ + num).flatMap(IO.println).as(true)
      case Command.ReadNumber => env.counter.get.flatMap(IO.println).as(true)
      case Command.LaunchDog(name) => ???
      case Command.ReleaseTheDogs => ???
    }

  def buildEnv: Resource[IO, Environment] = {
    val counter = Resource.make(IO.println("Alloc. counter") *> Ref.of[IO, Int](0)) (_ =>
    IO.println("Deallo, counter"))
    counter.map(Environment)
  }

  def run: IO[Unit] = buildEnv.use(env => program(env))
}*/

//5 bring evr. together

object MainCatsConcurrent extends IOApp.Simple {
  final case class Environment(counter: Ref[IO, Int], startWithGun: Deferred[IO, Unit])

  def program(env: Environment): IO[Unit] = iteration(env).iterateWhile(a=>a).void

  def iteration(env: Environment): IO[Boolean] = for {
    cmd <- IO.println("> ") *> IO.readLine
    shouldProcess <- Command.parse(cmd) match {
      case Left(err) => IO.raiseError(new Exception(s"invalide command: $err"))
      case Right(command) => process(env)(command)
    }
  } yield shouldProcess

  def process(env: Environment)(command: Command): IO[Boolean] =
    command match {
      case Command.Echo => {
        IO.readLine.flatMap(text => IO.println(text)).as(true) // map(_=>true)
      }
      case Command.Exit => {
        IO.println("Bye Bye").as(false)
      }
      case Command.AddNumber(num) => env.counter.updateAndGet(_ + num).flatMap(IO.println).as(true)
      case Command.ReadNumber => env.counter.get.flatMap(IO.println).as(true)
      case Command.LaunchDog(name) =>
        val fiberIO = (IO.println(s"Dog $name is ready") *> env.startWithGun.get *>
          IO.println(s"Dog $name is starting") *> env.counter.updateAndGet(_ + 1)
        .flatMap(value=> IO.println(s"Dog $name observe value $value")))
        fiberIO.start.as(true)
      case Command.ReleaseTheDogs => env.startWithGun.complete()
    }

  def buildEnv: Resource[IO, Environment] = (
    Resource.make(IO.println("Alloc. counter")*> Ref.of[IO, Int](0))(_ =>
    IO.println("Dealloc. counter")),
    Resource.make(IO.println("Alloc gun") *> Deferred[IO, Unit])(_=>
    IO.println("Dealloc gun"))).parMapN{ case (counter, gun) => Environment(counter, gun) }


  def run: IO[Unit] = buildEnv.use(env => program(env))
}