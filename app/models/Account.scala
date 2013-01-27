package models

import play.api.db._
import play.api.libs.json._
import play.api.libs.json.util._
import play.api.Logger
import play.api.Play.current
import org.mindrot.jbcrypt.BCrypt

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.lifted.{MappedTypeMapper,BaseTypeMapper,TypeMapperDelegate}
import scala.slick.driver.BasicProfile
import scala.slick.session.{PositionedParameters,PositionedResult}
import language.implicitConversions

import models._

sealed trait Permission
case object Administrator extends Permission
case object NormalUser extends Permission

case class Account(id: Option[Long] = None, 
    			   email: String, 
    			   password: String, 
    			   name: String, 
    			   permission: Permission)

object Account {
  
  lazy val database = Database.forDataSource(DB.getDataSource())

  implicit val permissionTypeMapper = MappedTypeMapper.base[Permission, String](
    p => p match {
      case Administrator => "Administrator"
      case NormalUser => "NormalUser"
    },
    s => s match {
      case "Administrator" => Administrator
      case "NormalUser" => NormalUser
    }
  )

  val AccountTable = new Table[Account]("account") {
    def id = column[Long]("account_id", O.PrimaryKey, O.AutoInc)
    def email = column[String]("email")
    def password = column[String]("password")
    def name = column[String]("name")
    def permission = column[Permission]("permission")
    def * = id.? ~ email ~ password ~ name ~ permission <> (Account.apply _, Account.unapply _)
  }

  def authenticate(name: String, password: String): Option[Account] = {
    Logger.info("[Account] Authenticating %s/%s".format(name, password))

    findByName(name).filter { account => BCrypt.checkpw(password, account.password) }
  }

  def findByEmail(email: String): Option[Account] = database.withSession { implicit db: Session =>
    Query(AccountTable).filter(a => a.email === email).firstOption
  }

  def findById(id: Long): Option[Account] = database.withSession { implicit db: Session =>
    Query(AccountTable).filter(a => a.id === id).firstOption
  }

  def findByName(name: String): Option[Account] = database.withSession { implicit db: Session =>
    Query(AccountTable).filter(a => a.name === name).firstOption
  }

  def findAll: Seq[Account] = database.withSession { implicit db: Session =>
    Query(AccountTable).list
  }

  def isAdmin(accountId: Long): Boolean = database.withSession { implicit db: Session =>
    Query(AccountTable).filter(a => a.id === accountId && a.permission === (Administrator:Permission)).firstOption.isDefined
  }

  def create(account: Account) = database.withSession { implicit db: Session =>
    AccountTable.insert(account)
  }

}

