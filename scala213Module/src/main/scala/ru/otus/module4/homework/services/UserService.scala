package ru.otus.module4.homework.services

import io.getquill.context.ZioJdbc.QIO
import ru.otus.module4.homework.dao.entity.{Role, RoleCode, User}
import ru.otus.module4.homework.dao.repository.UserRepository
import ru.otus.module4.phoneBook.db
import zio.{ZIO, ZLayer}
import java.sql.SQLException

trait UserService{
    def listUsers(): QIO[List[User]]
    def listUsersDTO(): QIO[List[UserDTO]]
    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]]
}
class Impl(userRepo: UserRepository) extends UserService {
    val dc = db.Ctx

    def listUsers(): QIO[List[User]] =
        userRepo.list()


    def listUsersDTO(): QIO[List[UserDTO]] = 
        for {
            users <- userRepo.list()
            userDTOs <- ZIO.foreach(users) { user =>
                for {
                    roles <- userRepo.userRoles(user.typedId)
                } yield UserDTO(user, roles.toSet)
            }
        } yield userDTOs

    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO] = 
        for {
            createdUser <- userRepo.createUser(user)
            _ <- userRepo.insertRoleToUser(roleCode, createdUser.typedId)
            role <- userRepo.findRoleByCode(roleCode).some.mapError(_ => new SQLException("Role not found"))
        } yield UserDTO(createdUser, Set(role))

    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]] = 
        for {
            users <- userRepo.listUsersWithRole(roleCode)
            userDTOs <- ZIO.foreach(users) { user =>
                for {
                    roles <- userRepo.userRoles(user.typedId)
                } yield UserDTO(user, roles.toSet)
            }
        } yield userDTOs
}

object UserService{
    val layer: ZLayer[UserRepository, Nothing, UserService] = 
        ZLayer.fromFunction((userRepo: UserRepository) => new Impl(userRepo))
}

case class UserDTO(user: User, roles: Set[Role])