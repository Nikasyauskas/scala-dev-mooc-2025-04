package ru.otus.module4.phoneBook.dao.repositories

import io.getquill.context.ZioJdbc._
import ru.otus.module4.phoneBook.dao.entities.{Address, PhoneRecord}
import ru.otus.module4.phoneBook.db
import zio.ULayer
import zio.ZLayer

trait PhoneRecordRepository{
  def find(phone: String): QIO[Option[PhoneRecord]]
  def list(): QIO[List[PhoneRecord]]
  def insert(phoneRecord: PhoneRecord): QIO[Unit]
  def update(phoneRecord: PhoneRecord): QIO[Unit]
  def delete(id: String): QIO[Unit]
}

class Impl extends PhoneRecordRepository{
    
    val ctx = db.Ctx
    import ctx._

    val phoneRecordSchema = quote{
      querySchema[PhoneRecord]("""PhoneRecord""")
    }

    val addressSchema = quote{
      querySchema[Address]("""Address""")
    }


    // SELECT x1."id" AS id, x1."phone" AS phone, x1."fio" AS fio, x1."addressId" AS addressId FROM PhoneRecord x1 WHERE x1."phone" = ?
    def find(phone: String): QIO[Option[PhoneRecord]] =
      ctx.run(phoneRecordSchema.filter(_.phone == lift(phone)).sortBy(_.phone).take(1).drop(5)).map(_.headOption)

    // SELECT x."id" AS id, x."phone" AS phone, x."fio" AS fio, x."addressId" AS addressId FROM PhoneRecord
    def list(): QIO[List[PhoneRecord]] = ctx.run(phoneRecordSchema)

    // INSERT INTO PhoneRecord ("id","phone","fio","addressId") VALUES (?, ?, ?, ?)
    def insert(phoneRecord: PhoneRecord): QIO[Unit] = ctx.run(phoneRecordSchema.insertValue(lift(phoneRecord))).unit

    // UPDATE PhoneRecord AS x3 SET "id" = ?, "phone" = ?, "fio" = ?, "addressId" = ? WHERE x3."id" = ?
    def update(phoneRecord: PhoneRecord): QIO[Unit] = ctx.run(phoneRecordSchema
    .filter(_.id == lift(phoneRecord.id)).updateValue(lift(phoneRecord))).unit

    //  UPDATE PhoneRecord AS x4 SET "id" = phr.id, "phone" = phr.phone, "fio" = phr.fio, "addressId" = phr.addressId FROM (VALUES (?, ?, ?, ?)) 
    // AS phr(id, phone, fio, addressId) WHERE x4."id" = ?
    def update(phoneRecords: List[PhoneRecord]): QIO[Unit] = ctx.run(liftQuery(phoneRecords).foreach{phr => 
      phoneRecordSchema.filter(_.id == lift(phr.id)).updateValue(phr)}).unit

    // DELETE FROM PhoneRecord AS x4 WHERE x4."id" = ?
    def delete(id: String): QIO[Unit] = ctx.run(phoneRecordSchema.filter(_.id == lift(id)).delete).unit

    // implicit join
    // SELECT phoneRecord."id" AS id, phoneRecord."phone" AS phone, phoneRecord."fio" AS fio, phoneRecord."addressId" AS addressId, 
    // address."id" AS id, address."zipCode" AS zipCode, address."streetAddress" AS streetAddress 
    // FROM PhoneRecord phoneRecord, Address address WHERE address."id" = phoneRecord."addressId"
    ctx.run(
      for{
        phoneRecord <- phoneRecordSchema
        address <- addressSchema if(address.id == phoneRecord.addressId)
      } yield (phoneRecord, address)
    )

    // applicative join
    // SELECT x7."id" AS id, x7."phone" AS phone, x7."fio" AS fio, x7."addressId" AS addressId, x8."id" AS id, x8."zipCode" AS zipCode, 
    // x8."streetAddress" AS streetAddress FROM PhoneRecord x7 INNER JOIN Address x8 ON x7."addressId" = x8."id"
    ctx.run(
      phoneRecordSchema.join(addressSchema).on(_.addressId == _.id)
    )

    // flat join
    ctx.run(
      for{
        phoneRecord <- phoneRecordSchema
        address <- addressSchema.join(_.id == phoneRecord.addressId)
      } yield phoneRecord
    )
}


 
object PhoneRecordRepository {



  val live: ULayer[PhoneRecordRepository] = ZLayer.succeed(new Impl)

}
