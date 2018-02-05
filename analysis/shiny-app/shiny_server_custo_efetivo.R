# Lógica do sistema de custo efetivo
server_custo_efetivo <- function(input, output, session) {
    mapa_paraiba_custo_efetivo <- get.mapa.paraiba(mapa_paraiba, localidades.custo.efetivo, 
                                                   tipo.localidade.selecionada.tipo.obra, 
                                                   localidade.selecionada.tipo.obra)
    
    cores.custo.efetivo <- paleta.de.cores(dado = mapa_paraiba_custo_efetivo@data$custo.efetivo.log)
    
    output$mapa_tipo_obra <- renderLeaflet({
        cria.mapa(
            mapa_paraiba_custo_efetivo, 
            mapa_paraiba_custo_efetivo@data$custo.efetivo.log, 
            mapa_paraiba_custo_efetivo@data$Nome_Munic, 
            paste("Município: ", mapa_paraiba_custo_efetivo@data$Nome_Munic, 
                  "</br>Custo efetivo: R$", format(mapa_paraiba_custo_efetivo@data$custo.efetivo, 
                                                   digits = 2, big.mark=".", decimal.mark = ",", 
                                                   scientific = FALSE), 
                  "por", get.unidade.medida(tipos.das.obras, tipo.obra.selecionada)
            ),
            cores.custo.efetivo, 
            "Custo efetivo das obras",
            tag.mapa.custo.efetivo,
            mapa_paraiba_custo_efetivo@data$GEOCODIG_M, 
            localidade.selecionada.tipo.obra,
            tipo.localidade.selecionada.tipo.obra,
            localidades.desc,
            mapa_paraiba_custo_efetivo@data$cor.borda,
            mapa_paraiba_custo_efetivo@data$largura.borda
        )
    })
    
    dygraph.tipo.obra <- function(dado, tipo.obra, tipo.localidade.selecionada, localidade.selecionada) {
        
        if(localidade.selecionada == "Todos"){
            dado.filtr <- dado
        }
        else if (tipo.localidade.selecionada == "microrregiao") {
            dado.filtr <- dado %>% 
                filter(microregiao == localidade.selecionada)
        } else if (tipo.localidade.selecionada == "mesorregiao") {
            dado.filtr <- dado %>% 
                filter(mesoregiao == localidade.selecionada)
        } else {
            dado.filtr <- dado %>%
                filter(nome == localidade.selecionada)
        }
        
        renderDygraph({
            dado.filtr %>%
                filter(tipo_obra == tipo.obra) %>%
                mutate(custo.efetivo = valor_obra/dimensao) %>%
                group_by(ano) %>%
                summarise(
                    custo.efetivo = median(custo.efetivo)
                ) %>%
                select(ano, custo.efetivo) %>%
                dygraph() %>%
                dyRangeSelector(dateWindow = c(menor.ano, maior.ano)) %>%
                dyLegend(show = "never")
        })
    }
    
    output$dygraph_tipo_obra <- dygraph.tipo.obra(custo.efetivo.obras, get.top.10.tipo.obra(custo.efetivo.obras)[1], 
                                                  tipo.localidade.selecionada.tipo.obra, localidade.selecionada.tipo.obra)
    
    output$ranking_tipo_obra <- renderPlot({
        plot.ranking.tipo.obra(localidades.custo.efetivo, input$select_localidade_tipo_obra,
                               tipo.localidade.selecionada.tipo.obra, tipos.das.obras, tipo.obra.selecionada, cores.custo.efetivo)
    })
    
    muda.listagem.obras <- function() {
        output$obras_custo_efetivo <- DT::renderDataTable({
            get.obras.custo.efetivo(obras.2013, ano.inicial.tipo.obra, ano.final.tipo.obra,
                                    localidade.selecionada.tipo.obra, tipo.localidade.selecionada.tipo.obra, 
                                    tipo.obra.selecionada)
            },
            options = list(
                autoWidth = TRUE,
                language = list(
                    info = "Mostrando obras de _START_ à _END_ de um total de _TOTAL_",
                    lengthMenu = "Mostrar _MENU_ itens",
                    paginate = list(previous = "Anterior", `next` = "Próxima", 
                                    first = "Primeira", last = "Última"),
                    search = "Pesquisar:"
                ))
        )
    }
    
    muda.listagem.obras()
    
    muda.input.localidade.tipo.obra <- function(localidades.custo.efetivo) {
        if (tipo.localidade.selecionada.tipo.obra == "municipio") {
            localidades.input <- localidades.custo.efetivo %>% arrange(nome) %>% pull(nome)
            localidades.input <- c("Todos", localidades.input)
        } else if (tipo.localidade.selecionada.tipo.obra == "microrregiao") {
            localidades.input <- localidades.custo.efetivo %>% arrange(microregiao) %>% pull(microregiao)
        } else {
            localidades.input <- localidades.custo.efetivo %>% arrange(mesoregiao) %>% pull(mesoregiao)
        }
        
        if (!localidade.selecionada.tipo.obra %in% localidades.input) {
            localidade.selecionada.tipo.obra <<- localidades.input[1]
        }
        
        updateSelectInput(session, inputId = "select_localidade_tipo_obra", 
                          choices = localidades.input,
                          selected = localidade.selecionada.tipo.obra)
    }
    
    get.localidades.custo.efetivo <- function(localidades.desc) {
        localidades.custo.efetivo <- get.custo.efetivo.tipo.obra(custo.efetivo.obras, 
                                                                 localidade.selecionada.tipo.obra, 
                                                                 tipo.obra.selecionada,
                                                                 ano.inicial.tipo.obra, 
                                                                 ano.final.tipo.obra)
        
        localidades.custo.efetivo %>% 
            add.borda(localidade.selecionada.tipo.obra) %>% 
            left_join(localidades.desc, by = "codigo_ibge") %>% 
            rename(nome = nome.x)
    }
    
    muda.mapa.tipo.obra.e.ranking <- function(localidades.custo.efetivo) {
        localidades.mapa <- filtra.regiao(localidades.custo.efetivo, tipo.localidade.selecionada.tipo.obra, localidade.selecionada.tipo.obra)
        mapa_paraiba_custo_efetivo <- get.mapa.paraiba(mapa_paraiba, localidades.mapa, tipo.localidade.selecionada.tipo.obra, localidade.selecionada.tipo.obra)
        
        cores.custo.efetivo <- paleta.de.cores(dado = mapa_paraiba_custo_efetivo@data$custo.efetivo.log)
        
        if (tipo.localidade.selecionada.tipo.obra == "microrregiao") {
            localidades <- mapa_paraiba_custo_efetivo@data$microregiao
        } else if (tipo.localidade.selecionada.tipo.obra == "mesorregiao") {
            localidades <- mapa_paraiba_custo_efetivo@data$mesoregiao
        } else {
            localidades <- mapa_paraiba_custo_efetivo@data$Nome_Munic
        }
        
        leafletProxy("mapa_tipo_obra", data = mapa_paraiba_custo_efetivo) %>%
            clearGroup( group = tag.mapa.custo.efetivo ) %>%
            clearControls() %>%
            adiciona.poligonos.e.legenda(cores.custo.efetivo,
                                         mapa_paraiba_custo_efetivo@data$custo.efetivo.log,
                                         mapa_paraiba_custo_efetivo@data$Nome_Munic,
                                         paste("Município: ", mapa_paraiba_custo_efetivo@data$Nome_Munic,
                                               "</br>Custo efetivo: R$", format(mapa_paraiba_custo_efetivo@data$custo.efetivo,
                                                                                digits = 2, big.mark=".", decimal.mark = ",",
                                                                                scientific = FALSE), 
                                               "por", get.unidade.medida(tipos.das.obras, tipo.obra.selecionada)
                                         ),
                                         "Custo efetivo das obras",
                                         tag.mapa.custo.efetivo,
                                         mapa_paraiba_custo_efetivo@data$GEOCODIG_M, 
                                         localidade.selecionada.tipo.obra,
                                         tipo.localidade.selecionada.tipo.obra,
                                         localidades.desc,
                                         mapa_paraiba_custo_efetivo@data$cor.borda,
                                         mapa_paraiba_custo_efetivo@data$largura.borda)
        
        output$ranking_tipo_obra <- renderPlot({
            plot.ranking.tipo.obra(localidades.mapa, localidade.selecionada.tipo.obra, 
                                   tipo.localidade.selecionada.tipo.obra, tipos.das.obras, tipo.obra.selecionada, cores.custo.efetivo)
        })
    }
    
    observeEvent({
        input$select_tipo_obra
    }, {
        if(!is.null(input$dygraph_tipo_obra_date_window)){
            tipo.obra.selecionada <<- input$select_tipo_obra
            
            localidades.custo.efetivo <- get.localidades.custo.efetivo(localidades.desc)
            muda.input.localidade.tipo.obra(localidades.custo.efetivo)
            
            output$dygraph_tipo_obra <- dygraph.tipo.obra(custo.efetivo.obras, tipo.obra.selecionada, 
                                                          tipo.localidade.selecionada.tipo.obra, localidade.selecionada.tipo.obra)
            
            muda.mapa.tipo.obra.e.ranking(localidades.custo.efetivo)
            muda.listagem.obras()
        }
    })
    
    observeEvent({
        input$dygraph_tipo_obra_date_window
    }, {
        if(!is.null(input$dygraph_tipo_obra_date_window)){
            ano1 <- round(input$dygraph_tipo_obra_date_window[[1]])
            ano2 <- round(input$dygraph_tipo_obra_date_window[[2]])
            
            if (!exists("ano.inicial.tipo.obra") || !exists("ano.final.tipo.obra") ||
                ano.inicial.tipo.obra != ano1 || ano.final.tipo.obra != ano2) {
                
                ano.inicial.tipo.obra <<- ano1
                ano.final.tipo.obra <<- ano2
                
                localidades.custo.efetivo <- get.localidades.custo.efetivo(localidades.desc)
                
                muda.input.localidade.tipo.obra(localidades.custo.efetivo)
                
                muda.mapa.tipo.obra.e.ranking(localidades.custo.efetivo)
                
                muda.listagem.obras()
            }
        }
    })
    
    observeEvent({
        input$select_localidade_tipo_obra
    }, {
        if(!is.null(input$dygraph_tipo_obra_date_window)){
            localidade.selecionada.tipo.obra <<- input$select_localidade_tipo_obra
            output$dygraph_tipo_obra <- dygraph.tipo.obra(custo.efetivo.obras, tipo.obra.selecionada, 
                                                          tipo.localidade.selecionada.tipo.obra, 
                                                          localidade.selecionada.tipo.obra)
            
            ano.inicial.tipo.obra <<- round(input$dygraph_tipo_obra_date_window[[1]])
            ano.final.tipo.obra <<- round(input$dygraph_tipo_obra_date_window[[2]])
        
            localidades.custo.efetivo <- get.localidades.custo.efetivo(localidades.desc)
            
            muda.mapa.tipo.obra.e.ranking(localidades.custo.efetivo)
            
            muda.listagem.obras()
        }
    })
    
    observeEvent({
        input$select_tipo_localidade_tipo_obra
    }, {
        tipo.localidade.selecionada.tipo.obra <<- input$select_tipo_localidade_tipo_obra
        localidades.custo.efetivo <- get.localidades.custo.efetivo(localidades.desc)
        
        muda.input.localidade.tipo.obra(localidades.custo.efetivo)
        
        muda.mapa.tipo.obra.e.ranking(localidades.custo.efetivo)
        
        muda.listagem.obras()
    })
}