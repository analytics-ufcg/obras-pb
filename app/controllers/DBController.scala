package controllers

import javax.inject.{Inject, Singleton}

import play.api.db._
import play.api.mvc._
import anorm._
import anorm.SqlParser._
import models.Building
import play.api.libs.json._
import ai.x.play.json.Jsonx

@Singleton
class DBController @Inject()(dbapi: DBApi) extends Controller {

  private val db = dbapi.database("default")
  private val FIRST_PAGE = 0
  private val INCLUDE_METADATA = 1


  /************* UTILITY FUNCTIONS *************/
  def getMetaData(limit: Int, offset: Int) = {
    val numberOfEntries = countBuildings
    Json.obj(
      "count"    -> JsNumber(countBuildings),
      "first"    -> "/buildings?limit=%d&offset=%d&metadata=%d".format(limit, FIRST_PAGE, INCLUDE_METADATA),
      "last"     -> "/buildings?limit=%d&offset=%d&metadata=%d".format(limit, numberOfEntries - limit, INCLUDE_METADATA),
      "next"     -> "/buildings?limit=%d&offset=%d&metadata=%d".format(limit,getNextPageOffset(limit, offset, numberOfEntries),INCLUDE_METADATA),
      "previous" -> "/buildings?limit=%d&offset=%d&metadata=%d".format(limit,getPreviousPageOffset(limit, offset),INCLUDE_METADATA)
    )
  }

  private def getPreviousPageOffset(limit: Int, offset: Int) = {
    if (offset - limit < 0) FIRST_PAGE else offset - limit
  }

  private def getNextPageOffset(limit: Int, offset: Int, numberOfEntries: Int) = {
    if (offset + limit >= numberOfEntries) numberOfEntries - 1 else offset + limit
  }

  def countBuildings() = {
    val rowParser = scalar[Long]
    db.withConnection { implicit connection =>
      val result: Int = SQL("SELECT COUNT(*) FROM Obras").as(scalar[Int].single)
      result
    }
  }


  /************* API *************/
  def getBuildings(limit: Int, offset: Int, metadata: Int) = Action {
    val parser: RowParser[Building] = Macro.namedParser[Building]
    db.withConnection { implicit connection =>
      val result: List[Building] = SQL("SELECT * FROM Obras LIMIT {limit} OFFSET {offset}")
        .on("limit" -> limit, "offset" -> offset)
        .as(parser.*)

      implicit val jsonExampleFormat = Jsonx.formatCaseClass[Building]
      val buildingsList = result.map(building => Json.toJson(building))

      if (metadata.equals(INCLUDE_METADATA)) {
        val myMetaData = getMetaData(limit, offset)
        Ok(Json.obj("metadata" -> myMetaData, "list" -> buildingsList))
      }
      else {
        Ok(Json.obj("lista" -> buildingsList))
      }

    }
  }

}
