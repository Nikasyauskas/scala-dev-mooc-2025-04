package ru.otus.module4.phoneBook

import ru.otus.module4.phoneBook.api.PhoneBookAPI
import ru.otus.module4.phoneBook.dao.repositories.{AddressRepository, PhoneRecordRepository}
import ru.otus.module4.phoneBook.db.LiquibaseService
import ru.otus.module4.phoneBook.services.PhoneBookService
import zio._
import zio.http.Server


object Main extends ZIOAppDefault {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = Server.serve(PhoneBookAPI.api)
    .provide(Server.default, PhoneBookService.live, PhoneRecordRepository.live, AddressRepository.live,
      LiquibaseService.liquibase, LiquibaseService.live, db.zioDS,  ZLayer.succeed(Random.RandomLive))
}
