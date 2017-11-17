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
  def getBuildings(limit: Int, offset: Int, metadata: Int, orderingField: String, fields: String) = Action { implicit request =>
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
      get[String]("obs").? ~
      get[String]("de_UGestora").? map {
        case cd_UGestora ~
          dt_Ano ~ nu_Obra ~ dt_Cadastro ~ tp_Patrimonio ~ de_Localizacao ~ de_Sucinta ~ tp_Obra ~  tp_CategoriaObra ~
          tp_Previsto ~  dt_Inicio ~ dt_Conclusao ~ dt_Recebimento ~ tp_FonteObra ~ vl_Obra ~ dt_MesAno ~ dimensao ~
          tp_naturezaObra ~ tp_naturezaObra_c ~ tp_Obra_c ~ de_Sucinta_c ~ pop_beneficiada ~ foto ~ foto_c ~
          dimensao_c ~ cei ~ crea ~ obs ~ de_UGestora =>
          Building(cd_UGestora, dt_Ano, nu_Obra, dt_Cadastro, tp_Patrimonio, de_Localizacao, de_Sucinta, tp_Obra,
            tp_CategoriaObra, tp_Previsto, dt_Inicio, dt_Conclusao, dt_Recebimento, tp_FonteObra, vl_Obra, dt_MesAno,
            dimensao, tp_naturezaObra, tp_naturezaObra_c, tp_Obra_c, de_Sucinta_c, pop_beneficiada, foto, foto_c,
            dimensao_c, cei, crea, obs, de_UGestora)
      }
    }

    def putTableAlias(field: String, uGestoraAlias: String, obrasAlias: String) : String = {
      var alias = ""
      var tableField = field.stripPrefix("-")
      if (field(0) == '-'){
        alias = "-"
      }
      if (tableField == "de_UGestora") {
        alias = alias.concat(uGestoraAlias)
      } else {
        alias = alias.concat(obrasAlias)
      }
      alias.concat(".").concat(tableField)
    }

    db.withConnection { implicit connection =>
      val obrasAlias = "o"
      val uGestoraAlias = "ug"
      val obrasFields = fields.split(",").map(
        e => putTableAlias(e, uGestoraAlias, obrasAlias)
      ).mkString(",")
      val obrasOrderingField = putTableAlias(orderingField, uGestoraAlias, obrasAlias)
      val uGestora: String = "SELECT DISTINCT cd_UGestora, de_UGestora FROM Acumulacao_Total"
      val result: List[Building] = SQL(s"SELECT $obrasFields FROM Obras $obrasAlias, ($uGestora) $uGestoraAlias " +
        s"WHERE $obrasOrderingField IS NOT NULL AND $obrasAlias.cd_UGestora = $uGestoraAlias.cd_UGestora " +
        s"ORDER BY $obrasOrderingField LIMIT $limit OFFSET $offset")
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
