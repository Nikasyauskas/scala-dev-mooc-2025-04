package ru.otus.module4.phoneBook

import zio.config.magnolia.deriveConfig
import zio.{Config, _}


package object configuration {

  case class MyConfig(api: Api, liquibase: LiquibaseConfig)
  
  case class LiquibaseConfig(changeLog: String)
  case class Api(host: String, port: Int)
  case class DbConfig(driver: String, url: String, user: String, password: String)

  private val myConfigAutomatic: Config[MyConfig] = deriveConfig[MyConfig]
  
  object Configuration{
    val config: IO[Config.Error, MyConfig] = ConfigProvider.defaultProvider.load(myConfigAutomatic)
  }
}
