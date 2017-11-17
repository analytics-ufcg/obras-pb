package controllers

import java.util.Date
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
  private val FIRST_PAGE = 1
  private val INCLUDE_METADATA = 1


  /************* UTILITY FUNCTIONS *************/

  /**
    * Gera a string para o Header 'Link' da resposta seguindo a especificação do RFC 5988.
    */
  private def getLinkHeaderString(limit: Int, page: Int, hostName : String) = {
      val numberOfEntries = countBuildings()
      "<http://" + hostName + "/buildings?limit=%d&page=%d&metadata=%d".format(limit, FIRST_PAGE, INCLUDE_METADATA) + ">; rel=\"first\"," +
      "<http://" + hostName + "/buildings?limit=%d&page=%d&metadata=%d".format(limit, getLastPage(limit, numberOfEntries), INCLUDE_METADATA) + ">; rel=\"last\"," +
      "<http://" + hostName + "/buildings?limit=%d&page=%d&metadata=%d".format(limit, getNextPage(limit, page, numberOfEntries), INCLUDE_METADATA) + ">; rel=\"next\"," +
      "<http://" + hostName + "/buildings?limit=%d&page=%d&metadata=%d".format(limit, getPreviousPage(page),INCLUDE_METADATA) + ">; rel=\"prev\""
  }

  /**
    * Obtém o número da ultima pagina.
    */
  private def getLastPage(limit: Int, numberOfEntries: Int) = {
    val lastPageRoundedUp = math.ceil(numberOfEntries.toFloat/limit).toInt
    lastPageRoundedUp
  }


  /**
    * Obtem o numero da pagina seguinte.
    */
  private def getNextPage(limit: Int, page: Int, numberOfEntries: Int) = {
    val entriesUpToThisPage = limit * page
    if(entriesUpToThisPage >= numberOfEntries) page else page + 1
  }

  /**
    * Obtem o numero da pagina anterior.
    */
  private def getPreviousPage(page: Int) = {
    val previous = page - 1
    if(previous >= 1) previous else FIRST_PAGE
  }


  /**
    * Retorna o indice do primeiro elemento da pagina
    * dado o número da página e o limite de observações.
    */
  private def getPageOffset(page :Int, limit: Int) = {
    val firstElementIndex = page * limit - limit
    firstElementIndex
  }


  /**
    * Conta o numero de observações.
    */
  private def countBuildings() = {
    val rowParser = scalar[Long]
    db.withConnection { implicit connection =>
      val result: Int = SQL("SELECT COUNT(*) FROM Obras").as(scalar[Int].single)
      result
    }
  }


  /************* API *************/

  def getBuildings(limit: Int, page: Int, metadata: Int, orderingField: String, fields: String) = Action { implicit request =>
    val parser = {
      get[Int]("cd_UGestora").? ~
      get[Int]("dt_Ano").? ~
      get[String]("nu_Obra").? ~
      get[Date]("dt_Cadastro").? ~
      get[String]("tp_Patrimonio").? ~
      get[String]("de_Localizacao").? ~
      get[String]("de_Sucinta").? ~
      get[Int]("tp_Obra").? ~
      get[Int]("tp_CategoriaObra").? ~
      get[String]("tp_Previsto").? ~
      get[Date]("dt_Inicio").? ~
      get[Date]("dt_Conclusao").? ~
      get[Date]("dt_Recebimento").? ~
      get[Int]("tp_FonteObra").? ~
      get[BigDecimal]("vl_Obra").? ~
      get[String]("dt_MesAno").? ~
      get[BigDecimal]("dimensao").? ~
      get[Int]("tp_naturezaObra").? ~
      get[Int]("tp_naturezaObra_c").? ~
      get[Int]("tp_Obra_c").? ~
      get[String]("de_Sucinta_c").? ~
      get[String]("pop_beneficiada").? ~
      get[String]("foto").? ~
      get[String]("foto_c").? ~
      get[BigDecimal]("dimensao_c").? ~
      get[String]("cei").? ~
      get[String]("crea").? ~
      get[String]("obs").? map {
        case cd_UGestora ~
          dt_Ano ~ nu_Obra ~ dt_Cadastro ~ tp_Patrimonio ~ de_Localizacao ~ de_Sucinta ~ tp_Obra ~  tp_CategoriaObra ~
          tp_Previsto ~  dt_Inicio ~ dt_Conclusao ~ dt_Recebimento ~ tp_FonteObra ~ vl_Obra ~ dt_MesAno ~ dimensao ~
          tp_naturezaObra ~ tp_naturezaObra_c ~ tp_Obra_c ~ de_Sucinta_c ~ pop_beneficiada ~ foto ~ foto_c ~
          dimensao_c ~ cei ~ crea ~ obs =>
          Building(cd_UGestora, dt_Ano, nu_Obra, dt_Cadastro, tp_Patrimonio, de_Localizacao, de_Sucinta, tp_Obra,
            tp_CategoriaObra, tp_Previsto, dt_Inicio, dt_Conclusao, dt_Recebimento, tp_FonteObra, vl_Obra, dt_MesAno,
            dimensao, tp_naturezaObra, tp_naturezaObra_c, tp_Obra_c, de_Sucinta_c, pop_beneficiada, foto, foto_c,
            dimensao_c, cei, crea, obs)
      }
    }

    db.withConnection { implicit connection =>
      val offset = getPageOffset(page, limit)
      val result: List[Building] = SQL(s"SELECT $fields FROM Obras WHERE $orderingField IS NOT NULL " +
        s"ORDER BY $orderingField LIMIT $limit OFFSET $offset")
        .as(parser.*)

      implicit val jsonExampleFormat = Jsonx.formatCaseClass[Building]
      val buildingsList = result.map(building => Json.toJson(building))

      if (metadata.equals(INCLUDE_METADATA)) {
        val linkString = getLinkHeaderString(limit, page, request.host)
        Ok(Json.obj("lista" -> buildingsList))
          .withHeaders("Link" -> linkString, "X-total-count" -> countBuildings().toString)
      }

      else {
        Ok(Json.obj("lista" -> buildingsList))
      }
    }
  }

}
