# Lógica do sistema de georreferenciamento
server_georref <- function(input, output, session) {
    mapa_paraiba_georreferenciada <- get.mapa.paraiba(mapa_paraiba, localidades.georref, 
                                                      tipo.localidade.selecionada.georref, 
                                                      localidade.selecionada.georref)
    
    if (tipo.representacao.georref == "relativo") {
        dados.poligonos.georref <- mapa_paraiba_georreferenciada@data$porc.georref
        titulo.legenda <- "Obras georreferenciadas (%)"
    } else {
        dados.poligonos.georref <- mapa_paraiba_georreferenciada@data$qtde.georref
        titulo.legenda <- "Obras georreferenciadas"
    }
    
    cores.georref <- paleta.de.cores(dado = dados.poligonos.georref, reverse = TRUE)
    
    output$mapa_georref <- renderLeaflet({
        cria.mapa(
            mapa_paraiba_georreferenciada, 
            dados.poligonos.georref, 
            mapa_paraiba_georreferenciada@data$Nome_Munic, 
            get.popup.georref(mapa_paraiba_georreferenciada@data$Nome_Munic, 
                              mapa_paraiba_georreferenciada@data$total.obras, 
                              mapa_paraiba_georreferenciada@data$qtde.georref, 
                              mapa_paraiba_georreferenciada@data$porc.georref,
                              mapa_paraiba_georreferenciada@data$possui.georref.mas.tem.coordenadas.fora.municipio),
            cores.georref, 
            titulo.legenda,
            tag.mapa.georref,
            mapa_paraiba_georreferenciada@data$GEOCODIG_M, 
            localidade.selecionada.georref,
            tipo.localidade.selecionada.georref,
            localidades.desc,
            mapa_paraiba_georreferenciada@data$cor.borda,
            mapa_paraiba_georreferenciada@data$largura.borda
        )
    })
    
    dygraph.georref <- function(dado, tipo.localidade.selecionada, localidade.selecionada, tipo.representacao) {
        if (tipo.localidade.selecionada == "microrregiao") {
            dado.filtr <- dado %>% 
                filter(microregiao == localidade.selecionada)
        } else if (tipo.localidade.selecionada == "mesorregiao") {
            dado.filtr <- dado %>% 
                filter(mesoregiao == localidade.selecionada)
        } else {
            dado.filtr <- dado
        }
        
        dado.agrup <- dado.filtr %>% 
            group_by(ano) %>% 
            summarise(
                total.obras = n(),
                qtde.georref = sum(!is.inputado),
                porc.georref = (qtde.georref / total.obras) * 100
            )
        
        if (tipo.representacao == "relativo") {
            dado.agrup <- dado.agrup %>% 
                select(ano, porc.georref)
        } else {
            dado.agrup <- dado.agrup %>% 
                select(ano, qtde.georref)
        }
        
        renderDygraph({
            dado.agrup %>% 
                dygraph() %>% 
                dyRangeSelector() %>%
                dyLegend(show = "never")
        })
    }
    
    output$dygraph_georref <- dygraph.georref(obras.2013, tipo.localidade.selecionada.georref, 
                                              localidade.selecionada.georref, tipo.representacao.georref)
    
    output$ranking_georref <- renderPlot({
        plot.ranking.georref(localidades.georref, input$select_localidade_georref, 
                             tipo.representacao.georref, tipo.localidade.selecionada.georref,
                             cores.georref)
    })
    
    muda.mapa.e.ranking.georref <- function(localidades.georref) {
        localidades.mapa <- filtra.regiao(localidades.georref, tipo.localidade.selecionada.georref, localidade.selecionada.georref)
        mapa_paraiba_georreferenciada <- get.mapa.paraiba(mapa_paraiba, localidades.mapa, tipo.localidade.selecionada.georref, localidade.selecionada.georref)
        
        if (tipo.representacao.georref == "relativo") {
            dados.poligonos <- mapa_paraiba_georreferenciada@data$porc.georref
            titulo.legenda.georref <- "Obras georreferenciadas (%)"
        } else {
            dados.poligonos <- mapa_paraiba_georreferenciada@data$qtde.georref
            titulo.legenda.georref <- "Obras georreferenciadas"
        }

        cores.georref <- paleta.de.cores(dado = dados.poligonos, reverse = TRUE)
        
        if (tipo.localidade.selecionada.georref == "microrregiao") {
            localidades <- mapa_paraiba_georreferenciada@data$microregiao
        } else if (tipo.localidade.selecionada.georref == "mesorregiao") {
            localidades <- mapa_paraiba_georreferenciada@data$mesoregiao
        } else {
            localidades <- mapa_paraiba_georreferenciada@data$Nome_Munic
        }
        
        leafletProxy("mapa_georref", data = mapa_paraiba_georreferenciada) %>%
            clearGroup( group = tag.mapa.georref ) %>%
            clearControls() %>%
            adiciona.poligonos.e.legenda(cores.georref,
                                         dados.poligonos,
                                         mapa_paraiba_georreferenciada@data$Nome_Munic, 
                                         get.popup.georref(mapa_paraiba_georreferenciada@data$Nome_Munic, 
                                                           mapa_paraiba_georreferenciada@data$total.obras, 
                                                           mapa_paraiba_georreferenciada@data$qtde.georref, 
                                                           mapa_paraiba_georreferenciada@data$porc.georref,
                                                           mapa_paraiba_georreferenciada@data$possui.georref.mas.tem.coordenadas.fora.municipio),
                                         titulo.legenda.georref,
                                         tag.mapa.georref,
                                         mapa_paraiba_georreferenciada@data$GEOCODIG_M, 
                                         localidade.selecionada.georref,
                                         tipo.localidade.selecionada.georref,
                                         localidades.desc,
                                         mapa_paraiba_georreferenciada@data$cor.borda,
                                         mapa_paraiba_georreferenciada@data$largura.borda)
        
        output$ranking_georref <- renderPlot({
            plot.ranking.georref(localidades.mapa, localidade.selecionada.georref, 
                                 tipo.representacao.georref, tipo.localidade.selecionada.georref,
                                 cores.georref)
        })
    }
    
    muda.input.localidades.georref <- function(localidades.georref) {
        if (tipo.localidade.selecionada.georref == "municipio") {
            localidades.input <- localidades.georref %>% arrange(nome.x) %>% pull(nome.x)
        } else if (tipo.localidade.selecionada.georref == "microrregiao") {
            localidades.input <- localidades.georref %>% arrange(microregiao) %>% pull(microregiao)
        } else {
            localidades.input <- localidades.georref %>% arrange(mesoregiao) %>% pull(mesoregiao)
        }
        
        if (!localidade.selecionada.georref %in% localidades.input) { 
            localidade.selecionada.georref <<- localidades.input[1]
        }
        
        updateSelectInput(session, inputId = "select_localidade_georref", 
                          choices = localidades.input,
                          selected = localidade.selecionada.georref)
    }
    
    observeEvent({
        input$dygraph_georref_date_window
    }, {
        if(!is.null(input$dygraph_georref_date_window)){
            ano1 <- round(input$dygraph_georref_date_window[[1]])
            ano2 <- round(input$dygraph_georref_date_window[[2]])
            
            if (ano.inicial.georref != ano1 || ano.final.georref != ano2) {
                ano.inicial.georref <<- ano1
                ano.final.georref <<- ano2
                localidades.georref <- get.porc.municipios.georref(obras.2013, localidades.desc, localidade.selecionada.georref, ano.inicial.georref, ano.final.georref)
                
                muda.mapa.e.ranking.georref(localidades.georref)
            }
        }
    },
    priority = 2)
    
    observeEvent({
        input$select_localidade_georref
    }, {
        if(!is.null(input$dygraph_georref_date_window)){
            localidade.selecionada.georref <<- input$select_localidade_georref
            
            if(tipo.localidade.selecionada.georref != "municipio") {
                output$dygraph_georref <- dygraph.georref(obras.2013, tipo.localidade.selecionada.georref, 
                                                          localidade.selecionada.georref, tipo.representacao.georref)
            }
            
            localidades.georref <- get.porc.municipios.georref(obras.2013, localidades.desc, localidade.selecionada.georref, ano.inicial.georref, ano.final.georref)
            
            muda.mapa.e.ranking.georref(localidades.georref)
        }
    },
    priority = 1)
    
    observeEvent({
        input$select_tipo_localidade_georref
    }, {
        tipo.localidade.selecionada.georref <<- input$select_tipo_localidade_georref
        localidades.georref <- get.porc.municipios.georref(obras.2013, localidades.desc, localidade.selecionada.georref, ano.inicial.georref, ano.final.georref)
        
        
        muda.input.localidades.georref(localidades.georref)
        
        muda.mapa.e.ranking.georref(localidades.georref)
    })
    
    observeEvent({
        input$select_tipo_representacao_georref
    }, {
        tipo.representacao.georref <<- input$select_tipo_representacao_georref
        
        muda.mapa.e.ranking.georref(localidades.georref)
        
        output$dygraph_georref <- dygraph.georref(obras.2013, tipo.localidade.selecionada.georref, 
                                                  localidade.selecionada.georref, tipo.representacao.georref)
    })
}