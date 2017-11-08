package controllers

import javax.inject.{Inject, Singleton}

import play.api.db._
import play.api.mvc._
import anorm._
import models.Build
import play.api.libs.json._
import ai.x.play.json.Jsonx

@Singleton
class DBController @Inject()(dbapi: DBApi) extends Controller {

  private val db = dbapi.database("default")
  val parser: RowParser[Build] = Macro.namedParser[Build]

  def testQuery = Action {
    db.withConnection { implicit connection =>
      val result: List[Build] = SQL("SELECT * FROM Obras LIMIT 10").as(parser.*)

      implicit val jsonExampleFormat = Jsonx.formatCaseClass[Build]

      val json = result.map(build => Json.toJson(build))

      println(json)

      Ok(Json.toJson(json))
    }
  }

}
