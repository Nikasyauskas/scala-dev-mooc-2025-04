package ru.otus.module4.phoneBook.services

import ru.otus.module4.phoneBook.dao.entities.{Address, PhoneRecord}
import ru.otus.module4.phoneBook.dao.repositories.{AddressRepository, PhoneRecordRepository}
import ru.otus.module4.phoneBook.db
import ru.otus.module4.phoneBook.dto.PhoneRecordDTO
import zio.{RIO, Random, ZIO, ZLayer}

import javax.sql.DataSource

trait PhoneBookService {
  def find(phone: String): ZIO[DataSource, Option[Throwable], (String, PhoneRecordDTO)]

  def insert(phoneRecord: PhoneRecordDTO): RIO[DataSource with Random, String]

  def update(id: String, addressId: String, phoneRecord: PhoneRecordDTO): RIO[DataSource, Unit]

  def delete(id: String): RIO[DataSource, Unit]
}

class Impl(phoneRecordRepository: PhoneRecordRepository, addressRepository: AddressRepository) extends PhoneBookService {
  val ctx = db.Ctx

  def find(phone: String): ZIO[DataSource, Option[Throwable], (String, PhoneRecordDTO)] = for {
    result <- phoneRecordRepository.find(phone).some
  } yield (result.id, PhoneRecordDTO.from(result))

  def insert(phoneRecord: PhoneRecordDTO): RIO[DataSource with Random, String] = for {
    uuid <- zio.Random.nextUUID.map(_.toString())
    uuid2 <- zio.Random.nextUUID.map(_.toString())
    address = Address(uuid, phoneRecord.zipCode, phoneRecord.address)
    _ <- ctx.transaction(
      for {
        _ <- addressRepository.insert(address)
        _ <- phoneRecordRepository.insert(PhoneRecord(uuid2, phoneRecord.phone, phoneRecord.fio, address.id))
      } yield ()
    )
  } yield uuid

  def update(id: String, addressId: String, phoneRecord: PhoneRecordDTO): RIO[DataSource, Unit] = for {
    _ <- phoneRecordRepository.update(PhoneRecord(id, phoneRecord.phone, phoneRecord.fio, addressId))
  } yield ()

  def delete(id: String): RIO[DataSource, Unit] = for {
    _ <- phoneRecordRepository.delete(id)
  } yield ()

}

object PhoneBookService {

  def find(phone: String): ZIO[PhoneBookService with DataSource, Option[Throwable], (String, PhoneRecordDTO)] =
    ZIO.serviceWithZIO[PhoneBookService](_.find(phone))

  def insert(phoneRecord: PhoneRecordDTO): RIO[PhoneBookService with DataSource with Random, String] =
    ZIO.serviceWithZIO[PhoneBookService](_.insert(phoneRecord))

  def update(id: String, addressId: String, phoneRecord: PhoneRecordDTO): RIO[PhoneBookService with DataSource, Unit] =
    ZIO.serviceWithZIO[PhoneBookService](_.update(id, addressId, phoneRecord))

  def delete(id: String): RIO[PhoneBookService with DataSource, Unit] =
    ZIO.serviceWithZIO[PhoneBookService](_.delete(id))


  val live: ZLayer[PhoneRecordRepository with AddressRepository, Nothing, PhoneBookService] =
    ZLayer(
      for {
        phoneRecordRepo <- ZIO.service[PhoneRecordRepository]
        addressRepo <- ZIO.service[AddressRepository]
      } yield new Impl(phoneRecordRepo, addressRepo)
    )


}
