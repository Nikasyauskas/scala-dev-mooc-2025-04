package ru.otus.module4.htt4sdsl


import cats.data.{EitherT, OptionT, ReaderT}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

object catsefect {
  //1
  def getUserName: IO[Option[String]] = IO.pure(Some("sdf"))
  def getId(name: String): IO[Option[Int]] = IO.pure(Some(1))
  def getPermission(id: Int): IO[Option[String]] = IO.pure(Some("Permissions"))

  def main(args: Array[String]): Unit = {
    //1
    val res: OptionT[IO, (String, Int, String)] = for {
      username <- OptionT(getUserName)
      id <- OptionT(getId(username))
      permissions <-OptionT(getPermission(id))
    } yield (username, id, permissions)

    //2
    def getId1(name: String): IO[Int] = IO.pure(1)
    val res1 = for  {
      username <- OptionT(getUserName)
      id <- OptionT.liftF(getId1(username))
      permissions <- OptionT(getPermission(id))
    } yield (username, id, permissions)

    // 3 either
    sealed  trait  UserServerError
    case class PermissionDenied(msg: String) extends  UserServerError
    case class UserNotFound(userid: Int) extends UserServerError
    def getUserName2(userid: Int): EitherT[IO, UserServerError, String] = EitherT.pure("test")
    def getUserAddress(userId: Int): EitherT[IO, UserServerError, String] = EitherT.fromEither(Right("bla bla bla"))

    def getProfile(id:Int) = for {
      name <- getUserName2(id)
      address <- getUserAddress(id)
    } yield (name, address)

    println(getProfile(1).value.unsafeRunSync())

    // 4 ReaderT
    // Env => IO[A]
   /* trait ConnectionPool
    case class Environemnt(cp: ConnectionPool)
    def getUserAlias(id: Int): ReaderT[IO, Environemnt, String] = ReaderT(cp => IO.pure("111"))
    def getComment(id: Int): ReaderT[IO, Environemnt, String] = ReaderT.liftF(IO.println("updated"))
    def updateComment(id: Int, text: String): ReaderT[IO, Environemnt, Unit] = ReaderT.liftF(IO.println("updated"))

    val result = for {
      a <- getUserAlias(1)
      b <- getComment(1)
      _ <- updateComment(1, "bla bla bla")
    } yield (a,b)

    println(result(Environemnt(new ConnectionPool {{}})).unsafeRunSync())
*/
    // Kleisli[F,A,B] A=>F[B]
    // type readerT[F[_],R,A] = Kleisli[F,R,A] R=>F[A]

  }
}