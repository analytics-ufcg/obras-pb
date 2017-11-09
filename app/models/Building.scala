package models

import java.util.Date

case class Building(cd_UGestora: Option[Int], dt_Ano: Option[Int], nu_Obra: Option[String], dt_Cadastro: Option[Date],
                    tp_Patrimonio: Option[String], de_Localizacao: Option[String], de_Sucinta: Option[String],
                    tp_Obra: Option[Int], tp_CategoriaObra: Option[Int], tp_Previsto: Option[String],
                    dt_Inicio: Option[Date], dt_Conclusao: Option[Date], dt_Recebimento: Option[Date],
                    tp_FonteObra: Option[Int], vl_Obra: Option[BigDecimal], dt_MesAno: Option[String],
                    dimensao: Option[Float], tp_naturezaObra: Option[Int], tp_naturezaObra_c: Option[Int],
                    tp_Obra_c: Option[Int], de_Sucinta_c: Option[String], pop_beneficiada: Option[String],
                    foto: Option[String], foto_c: Option[String], dimensao_c: Option[Float], cei: Option[String],
                    crea: Option[String], obs: Option[String])



