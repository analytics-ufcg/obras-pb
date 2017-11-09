package controllers

import javax.inject.{Inject, Singleton}

import play.api.db._
import play.api.mvc._
import anorm._
import models.Building
import play.api.libs.json._
import ai.x.play.json.Jsonx

@Singleton
class DBController @Inject()(dbapi: DBApi) extends Controller {

  private val db = dbapi.database("default")
  val parser: RowParser[Building] = Macro.namedParser[Building]

  def getBuildings(limit: Int, offset: Int) = Action {
    db.withConnection { implicit connection =>
      val result: List[Building] = SQL("SELECT * FROM Obras LIMIT {limit} OFFSET {offset}")
        .on("limit" -> limit, "offset" -> offset)
        .as(parser.*)

      implicit val jsonExampleFormat = Jsonx.formatCaseClass[Building]

      val json = result.map(building => Json.toJson(building))

      Ok(Json.toJson(json))
    }
  }

}
