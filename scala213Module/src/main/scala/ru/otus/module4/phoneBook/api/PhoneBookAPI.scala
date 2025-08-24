package ru.otus.module4.phoneBook.api

import zio._
import io.circe.syntax._
import ru.otus.module4.phoneBook.dto.PhoneRecordDTO
import ru.otus.module4.phoneBook.services.PhoneBookService
import zio.http._

object PhoneBookAPI {

  val api = Routes(
    Method.GET / "api" / "v1" / string("phone") -> handler{ (phone: String, _: Request) =>
      PhoneBookService.find(phone).fold(
        _ => Response.status(Status.NotFound),
        result => Response.json(result.asJson.toString())
      )
    },
    Method.POST / "api" / "v1" -> handler{ request : Request =>
      val dtoZIO: ZIO[Any, Throwable, PhoneRecordDTO] = request.body.asString(Charsets.Utf8)
        .flatMap(str => ZIO.fromEither(PhoneRecordDTO.decoder.decodeJson(str.asJson)))

      (for{
        dto <- dtoZIO
        result <- PhoneBookService.insert(dto)
      } yield result).fold(
        err => Response.badRequest(err.getMessage),
        result => Response.json(result)
      )
    },
    Method.PUT / "api" / "v1" / string("id") / string("addressId") -> handler{ (id: String, addressId: String, request : Request) =>
      val dtoZIO: ZIO[Any, Throwable, PhoneRecordDTO] = request.body.asString(Charsets.Utf8)
        .flatMap(str => ZIO.fromEither(PhoneRecordDTO.decoder.decodeJson(str.asJson)))

      (for{
        dto <- dtoZIO
        result <- PhoneBookService.update(id, addressId, dto)
      } yield result).fold(
        err => Response.badRequest(err.getMessage),
        _ => Response.ok
      )
    },
    Method.DELETE / "api" / "v1" / string("id") -> handler{(id: String, _: Request) =>
      PhoneBookService.delete(id).fold(
        err => Response.badRequest(err.getMessage),
        _ => Response.ok
      )
    }

  )
}