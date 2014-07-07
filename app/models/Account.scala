package models

import org.mindrot.jbcrypt.BCrypt
import scala.slick.driver.H2Driver.simple._
import scala.slick.jdbc.JdbcBackend.Database.dynamicSession
/**
 * Created by Jamie on 30/06/2014.
 */

case class Account(email: String, password: String, name: String)

object Account {

  class Users(tag: Tag) extends Table[(String, String, String)](tag, "USERS") {
    def email = column[String]("EMAIL", O.PrimaryKey)
    def password = column[String]("PASSWORD")
    def name = column[String]("NAME")
    def * = (email, password, name)
  }
  val users = TableQuery[Users]

  def authenticate(email: String, password: String): Option[Account] = {
    findByEmail(email).filter { account => BCrypt.checkpw(password, account.password) }
  }

  def create(name: String, email: String, password: String, confirm: String): Option[Account] = {
    if (password != confirm) None
    else {
      Database.forURL("jdbc:h2:ribbits", driver = "org.h2.Driver") withDynSession {
        try {
          users.ddl.create
        } catch {
          case e: org.h2.jdbc.JdbcSQLException => println("Skipping table creation. It already exists.")
        }
        users.insert(email, BCrypt.hashpw(password, BCrypt.gensalt()), name)
      }
      findByEmail(email)
    }
  }

  def findByEmail(email: String): Option[Account] = findBy("email", email)

  def findById(id: Int): Option[Account] = findBy("id", id.toString)

  def findAll(): Seq[Account] = findById(1).toSeq

  def findBy(field: String, value: String): Option[Account] = {
    val user = Database.forURL("jdbc:h2:ribbits", driver = "org.h2.Driver") withDynSession  {
      val foundUsers = field match {
        case "email" => for {
          u <- users if u.email.toLowerCase === value.toLowerCase
        } yield (u)
        case "name" => for {
          u <- users if u.name.toLowerCase === value.toLowerCase
        } yield(u)
      }
      foundUsers.firstOption
    }
    user map {
      case (email, password, name) => new Account(email, password, name)
    }
  }
}

class NullAccount(email: String = "not@set", password: String = "", name: String = "Unknown")
  extends Account(email: String, password: String, name: String) {
}
