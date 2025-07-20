package ru.otus.module3

import ru.otus.module3.emailService.EmailAddress
import ru.otus.module3.userService.User
import ru.otus.module3.userService.UserID
import zio.{Task, ULayer, ZIO, ZLayer}


package object userDAO {

  /**
   * Реализовать сервис с двумя методами
   *  1. list - список всех пользователей
   *  2. findBy - поиск по User ID
   */

  trait UserDAO {
    def list(): Task[List[User]]
    def findBy(id: UserID): Task[Option[User]]
  }

  object UserDAO {
    val live: ULayer[UserDAO] = ZLayer.succeed(
      new UserDAO {
        val users: List[User] = List(
          User(UserID(1), EmailAddress("email@mail.com")),
          User(UserID(2), EmailAddress("email2@mail.com")),
          User(UserID(32), EmailAddress("email3@mail.com"))
        )
        override def list(): Task[List[User]] = ZIO.attempt(users)

        override def findBy(id: UserID): Task[Option[User]] =
          ZIO.attempt(users.find(_.id == id))
      }
    )
  }


}
