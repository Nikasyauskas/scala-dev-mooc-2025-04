package ru.otus.module4.homework.dao.repository

import zio.{ULayer, ZIO, ZLayer}
import io.getquill.context.ZioJdbc._
import ru.otus.module4.homework.dao.entity._
import ru.otus.module4.phoneBook.db

import java.sql.SQLException
import javax.sql.DataSource

trait UserRepository{
    def findUser(userId: UserId): QIO[Option[User]]
    def createUser(user: User): QIO[User]
    def createUsers(users: List[User]): QIO[List[User]]
    def updateUser(user: User): QIO[Unit]
    def deleteUser(user: User): QIO[Unit]
    def findByLastName(lastName: String): QIO[List[User]]
    def list(): QIO[List[User]]
    def userRoles(userId: UserId): QIO[List[Role]]
    def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]
    def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
}


class UserRepositoryImpl extends UserRepository {
    val dc = db.Ctx
    import dc._

    override def findUser(userId: UserId): QIO[Option[User]] = 
        run(query[User].filter(_.id == lift(userId.id))).map(_.headOption)

    override def createUser(user: User): QIO[User] = 
        run(query[User].insertValue(lift(user))).map(_ => user)

    override def createUsers(users: List[User]): QIO[List[User]] = 
        run(liftQuery(users).foreach(u => query[User].insertValue(u))).map(_ => users)

    override def updateUser(user: User): QIO[Unit] = 
        run(query[User].filter(_.id == lift(user.id)).updateValue(lift(user))).unit

    override def deleteUser(user: User): QIO[Unit] = 
        run(query[User].filter(_.id == lift(user.id)).delete).unit

    override def findByLastName(lastName: String): QIO[List[User]] = 
        run(query[User].filter(_.lastName == lift(lastName)))

    override def list(): QIO[List[User]] = 
        run(query[User])

    override def userRoles(userId: UserId): QIO[List[Role]] = 
        run(
            query[UserToRole]
                .filter(_.userId == lift(userId.id))
                .join(query[Role])
                .on(_.roleId == _.code)
                .map(_._2)
        )

    override def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit] = 
        run(
            query[UserToRole].insertValue(
                lift(UserToRole(roleCode.code, userId.id))
            )
        ).unit

    override def listUsersWithRole(roleCode: RoleCode): QIO[List[User]] = 
        run(
            query[UserToRole]
                .filter(_.roleId == lift(roleCode.code))
                .join(query[User])
                .on(_.userId == _.id)
                .map(_._2)
        )

    override def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]] = 
        run(query[Role].filter(_.code == lift(roleCode.code))).map(_.headOption)
}

object UserRepository{

    val layer: ULayer[UserRepository] = ZLayer.succeed(new UserRepositoryImpl)
}