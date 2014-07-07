package models

/**
 * Created by Jamie on 07/07/2014.
 */

import java.text.SimpleDateFormat
import java.util.Calendar

import models.Account.Users
import org.mindrot.jbcrypt.BCrypt
import scala.slick.driver.H2Driver.simple._
import scala.slick.jdbc.JdbcBackend.Database.dynamicSession

case class RibbitRepository()
object  RibbitRepository {

  class Ribbits(tag: Tag) extends Table[(String, String, String)](tag, "RIBBITS") {
    def content = column[String]("CONTENT", O.PrimaryKey)
    def sender = column[String]("SENDER")
    def dateTime = column[String]("DATETIME")
    def * = (content, sender, dateTime)
  }
  val ribbits = TableQuery[Ribbits]
  val users = TableQuery[Users]


  def create(content: String, sender: Option[String]): (String, String, String, String) = {
    Database.forURL("jdbc:h2:ribbits", driver = "org.h2.Driver") withDynSession {
      try {
        ribbits.ddl.create
      } catch {
        case e: org.h2.jdbc.JdbcSQLException => println("Skipping table creation. It already exists.")
      }

      def senderEmail = sender match {
        case Some(email) => email
        case None => "Unknown@email.address"
      }

      ribbits.insert(content, senderEmail, new SimpleDateFormat("yyyy-MM-dd HH:mm").format(Calendar.getInstance.getTime))
    }
    findAll.last
  }

  def findAll(): Seq[(String, String, String, String)] = {
    val allRibbits = Database.forURL("jdbc:h2:ribbits", driver= "org.h2.Driver") withDynSession {
      try {
        ribbits.ddl.create
      } catch {
        case e: org.h2.jdbc.JdbcSQLException => println("Skipping table creation.  It already exists.")
      }

      val foundRibbits = for {
        r <- ribbits
        u <- users if u.email.toLowerCase === r.sender.toLowerCase
      } yield(r.content, r.sender, r.dateTime, u.name)
      foundRibbits.list
    }
    allRibbits
  }

}
