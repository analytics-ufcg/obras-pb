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

  /**
    * Gera a string para o Header 'Link' da resposta seguindo a especificação do RFC 5988.
    */
  def getLinkHeaderString(limit: Int, offset: Int, hostName : String) = {
      val numberOfEntries = countBuildings()
      "<http://" + hostName + "/buildings?limit=%d&offset=%d&metadata=%d".format(limit, FIRST_PAGE, INCLUDE_METADATA) + ">; rel=\"first\"," +
      "<http://" + hostName + "/buildings?limit=%d&offset=%d&metadata=%d".format(limit, numberOfEntries - limit, INCLUDE_METADATA) + ">; rel=\"last\"," +
      "<http://" + hostName + "/buildings?limit=%d&offset=%d&metadata=%d".format(limit,getNextPageOffset(limit, offset, numberOfEntries), INCLUDE_METADATA) + ">; rel=\"next\"," +
      "<http://" + hostName + "/buildings?limit=%d&offset=%d&metadata=%d".format(limit,getPreviousPageOffset(limit, offset),INCLUDE_METADATA) + ">; rel=\"prev\""
  }

  /**
    * Obtém o offset da pagina interior.
    */
  private def getPreviousPageOffset(limit: Int, offset: Int) = {
    if (offset - limit < 0) FIRST_PAGE else offset - limit
  }

  /**
    * Obtém o offset da pagina seguinte.
    */
  private def getNextPageOffset(limit: Int, offset: Int, numberOfEntries: Int) = {
    if (offset + limit >= numberOfEntries) numberOfEntries - 1 else offset + limit
  }


  /**
    * Conta o numero de observações.
    */
  def countBuildings() = {
    val rowParser = scalar[Long]
    db.withConnection { implicit connection =>
      val result: Int = SQL("SELECT COUNT(*) FROM Obras").as(scalar[Int].single)
      result
    }
  }


  /************* API *************/

  def getBuildings(limit: Int, offset: Int, metadata: Int, orderingField: String) = Action { implicit request =>
    val parser: RowParser[Building] = Macro.namedParser[Building]
    db.withConnection { implicit connection =>

      val result: List[Building] = SQL(s"SELECT * FROM Obras ORDER BY $orderingField LIMIT $limit OFFSET $offset")
        .as(parser.*)

      implicit val jsonExampleFormat = Jsonx.formatCaseClass[Building]
      val buildingsList = result.map(building => Json.toJson(building))

      if (metadata.equals(INCLUDE_METADATA)) {
        val linkString = getLinkHeaderString(limit, offset, request.host)
        Ok(Json.obj("lista" -> buildingsList))
          .withHeaders("Link" -> linkString, "X-total-count" -> countBuildings().toString)
      }

      else {
        Ok(Json.obj("lista" -> buildingsList))
      }
    }
  }

}
