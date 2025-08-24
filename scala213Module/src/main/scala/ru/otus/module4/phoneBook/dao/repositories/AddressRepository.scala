package ru.otus.module4.phoneBook.dao.repositories

import io.getquill.context.ZioJdbc._
import ru.otus.module4.phoneBook.dao.entities.Address
import zio.{ULayer, ZLayer}
import ru.otus.module4.phoneBook.db

trait AddressRepository{
  def findBy(id: String): QIO[Option[Address]]
  def insert(phoneRecord: Address): QIO[Unit]
  def update(phoneRecord: Address): QIO[Unit]
  def delete(id: String): QIO[Unit]
}

object AddressRepository {
  
  import db.Ctx._
  class ServiceImpl extends AddressRepository{

      def findBy(id: String): QIO[Option[Address]] = ???
      def insert(address: Address): QIO[Unit] = ???
      def update(address: Address): QIO[Unit] = ???
      
      def delete(id: String): QIO[Unit] = ???
      
  }

  val live: ULayer[AddressRepository] = ZLayer.succeed(new ServiceImpl)
}
