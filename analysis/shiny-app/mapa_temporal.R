library(shiny)
library(shinydashboard)
library(leaflet)
library(dygraphs)
library(rgdal)
library(tidyverse)
library(RPostgreSQL)
#devtools::install_github("analytics-ufcg/geopbutils")
library(GeoPBUtils)

# Importa tabelas csv
tipos.das.obras <- read.csv("../dados/tipos_obra.csv")
municipios.pb <- read.csv("../dados/municipios_pb.csv")
mapa_paraiba <- readOGR("../dados/mapa-paraiba-ibge/Municipios.shp")

# Pega dados
drv <- DBI::dbDriver("PostgreSQL")

CONFIG_FILE_PATH = "../config/config.yml"

con1 <- DBI::dbConnect(drv,
                       host = config::get("host", file = CONFIG_FILE_PATH),
                       port = config::get("port", file = CONFIG_FILE_PATH),
                       user = config::get("user", file = CONFIG_FILE_PATH),
                       password = config::get("password", file = CONFIG_FILE_PATH),
                       dbname = config::get("dbname1", file = CONFIG_FILE_PATH)
)

con2 <- DBI::dbConnect(drv,
                       host = config::get("host", file = CONFIG_FILE_PATH),
                       port = config::get("port", file = CONFIG_FILE_PATH),
                       user = config::get("user", file = CONFIG_FILE_PATH),
                       password = config::get("password", file = CONFIG_FILE_PATH),
                       dbname = config::get("dbname2", file = CONFIG_FILE_PATH)
)

GeoPBUtils::get.data(con1, con2, mapa_paraiba)

dbDisconnect(con1)
dbDisconnect(con2)

# Manipulação de dados
municipios <- data.frame(codigo_ibge = mapa_paraiba$GEOCODIG_M, lat = coordinates(mapa_paraiba)[,2], lon = coordinates(mapa_paraiba)[,1])

obras.2013 <- get.georreferencia.inputada(obra, localidade, tipos.das.obras, municipios, obra.georref.centroide.sumarizado, 2013) %>% 
    filter(codigo_ibge != 0)

localidades.desc <- localidade %>% 
    filter(uf == "Paraíba") %>% 
    select(codigo_ibge, codigo_microregiao, codigo_mesoregiao, nome, microregiao, mesoregiao)

custo.efetivo.obras <- get.custos.efetivos(obras.2013)

tipo.obra.selecionada <<- get.top.10.tipo.obra(custo.efetivo.obras)[1]

localidades.georref <<- get.porc.municipios.georref(obras.2013, localidades.desc, cidade.default(obras.2013, "nome.x"))
localidades.custo.efetivo <<- get.custo.efetivo.tipo.obra(custo.efetivo.obras, cidade.default(custo.efetivo.obras, "nome"),
                                                                   tipo.obra.selecionada)

tipo.localidade.selecionada.tipo.obra <<- "municipio"
localidade.selecionada.tipo.obra <<- cidade.default(localidades.custo.efetivo, "nome")
ano.inicial.tipo.obra <<- 0
ano.final.tipo.obra <<- 3000
tag.mapa.custo.efetivo <<- "municipios-poligono-custo-efetivo"

tipo.representacao.georref <<- "relativo"
tipo.localidade.selecionada.georref <<- "municipio"
localidade.selecionada.georref <<- cidade.default(localidades.georref, "nome.x")
ano.inicial.georref <<- 0
ano.final.georref <<- 3000
tag.mapa.georref <<- "municipios-poligono-georref"

localidades.custo.efetivo <<- add.borda(localidades.custo.efetivo, localidade.selecionada.tipo.obra)

mapa_paraiba_georreferenciada <- get.mapa.paraiba(mapa_paraiba, localidades.georref, tipo.localidade.selecionada.georref, localidade.selecionada.georref)
mapa_paraiba_custo_efetivo <- get.mapa.paraiba(mapa_paraiba, localidades.custo.efetivo, tipo.localidade.selecionada.tipo.obra, localidade.selecionada.tipo.obra)

# Interface do usuário
ui <- dashboardPage(
    title = "Obras georreferenciadas",
    dashboardHeader( 
        title = span(
                    tagList(
                        img(src = "tce-cropped.png", height = 45), 
                        "Obras georreferenciadas"
                    )
                ),
        titleWidth = 330
    ),
    dashboardSidebar( 
        sidebarMenu(
            menuItem("Obras georreferenciadas", tabName = "obras-georref", icon = icon("map-marker")),
            menuItem("Tipos de obras", icon = icon("th"), tabName = "tipos-obras", badgeLabel = "new", badgeColor = "green")
        )
    ),
    dashboardBody(
        tags$head(tags$style(HTML('.box-header {min-height: 35px;}'))),
        tags$head(tags$link(rel="shortcut icon", href="tce-cropped.png")),
        tags$head(
            HTML("<script>
                     var socket_timeout_interval
                     var n = 0
                     $(document).on('shiny:connected', function(event) {
                     socket_timeout_interval = setInterval(function(){
                     Shiny.onInputChange('count', n++)
                     }, 15000)
                     });
                     $(document).on('shiny:disconnected', function(event) {
                     clearInterval(socket_timeout_interval)
                     });
                 </script>")
        ),
        tabItems(
            tabItem(
                tabName = "obras-georref",
                fluidRow(
                    height = "100vh",
                    column(width = 12,
                           box(width = NULL,
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               radioButtons("select_tipo_localidade_georref", "Tipo de localidade:",
                                            c("Município" = "municipio",
                                              "Microrregião" = "microrregiao",
                                              "Mesorregião" = "mesorregiao"),
                                            inline = TRUE
                               ),
                               radioButtons("select_tipo_representacao_georref", "Valor representado:",
                                            c("Relativo" = "relativo",
                                              "Absoluto" = "absoluto"),
                                            inline = TRUE
                               ),
                               selectInput("select_localidade_georref", label = h3("Selecione a localidade"),
                                           choices = localidades.georref$nome.x,
                                           selected = cidade.default(localidades.georref, "nome.x"))
                               
                           )
                    ),  
                    column(width = 12,
                        box(width = NULL,
                            collapsible = TRUE,
                            leafletOutput("mapa_georref", height = "50vh")
                        ),
                        box(width = NULL,
                            collapsible = TRUE,
                            dygraphOutput("dygraph_georref",  height = "25vh")
                        ),
                        box(width = "50vh",
                            collapsible = TRUE,
                            plotOutput("ranking_georref", height = "57vh")
                        )
                    )
                )
            ),
            tabItem(
                tabName = "tipos-obras",
                fluidRow(
                    column(width = 12,
                           box(width = NULL,
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               radioButtons("select_tipo_localidade_tipo_obra", "Tipo de localidade:",
                                            c("Município" = "municipio",
                                              "Microrregião" = "microrregiao",
                                              "Mesorregião" = "mesorregiao"),
                                            inline = TRUE
                               ),
                               selectInput("select_tipo_obra", label = h3("Selecione o tipo da obra"),
                                           choices = get.top.10.tipo.obra(custo.efetivo.obras),
                                           selected = tipo.obra.selecionada),
                               selectInput("select_localidade_tipo_obra", label = h3("Selecione a localidade"),
                                           choices = localidades.custo.efetivo$nome, 
                                           selected = localidade.selecionada.tipo.obra)
                           ),
                           box(width = NULL,
                               collapsible = TRUE,
                               leafletOutput("mapa_tipo_obra", height = "50vh")
                           ),
                           box(width = NULL,
                               collapsible = TRUE,
                               dygraphOutput("dygraph_tipo_obra", height = "25vh")
                           )
                    ),
                    column(width = 12,
                           box(width = NULL,
                               collapsible = TRUE,
                               plotOutput("ranking_tipo_obra", height = "51vh")
                           )
                    )
                )
            )
        )
    )
)

# Lógica do sistema
server <- function(input, output, session) {
    output$keepAlive <- renderText({
        req(input$count)
        paste("keep alive ", input$count)
    })
    
    v <- reactiveValues(msg = "")
    
    if (tipo.representacao.georref == "relativo") {
        dados.poligonos.georref <- mapa_paraiba_georreferenciada@data$porc.georref
        titulo.legenda <- "Obras georreferenciadas (%)"
    } else {
        dados.poligonos.georref <- mapa_paraiba_georreferenciada@data$qtde.georref
        titulo.legenda <- "Obras georreferenciadas"
    }
    
    cores.georref <- paleta.de.cores(dado = dados.poligonos.georref, reverse = TRUE)
    cores.custo.efetivo <- paleta.de.cores(dado = mapa_paraiba_custo_efetivo@data$custo.efetivo.log)

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
    
    output$mapa_tipo_obra <- renderLeaflet({
        cria.mapa(
            mapa_paraiba_custo_efetivo, 
            mapa_paraiba_custo_efetivo@data$custo.efetivo.log, 
            mapa_paraiba_custo_efetivo@data$Nome_Munic, 
            paste("Município: ", mapa_paraiba_custo_efetivo@data$Nome_Munic, 
                  "</br>Custo efetivo: R$", format(mapa_paraiba_custo_efetivo@data$custo.efetivo, 
                                                   digits = 2, big.mark=".", decimal.mark = ",", 
                                                   scientific = FALSE)
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
    
    dygraph.georref <- function(dado, tipo.localidade.selecionada, localidade.selecionada) {
        if (tipo.localidade.selecionada == "microrregiao") {
            dado.filtr <- dado %>% 
                filter(microregiao == localidade.selecionada)
        } else if (tipo.localidade.selecionada == "mesorregiao") {
            dado.filtr <- dado %>% 
                filter(mesoregiao == localidade.selecionada)
        } else {
            dado.filtr <- dado
        }
        
        renderDygraph({
            dado.filtr %>% 
                group_by(ano) %>% 
                summarise(
                    total.obras = n(),
                    qtde.georref = sum(!is.inputado),
                    porc.georref = (qtde.georref / total.obras) * 100
                ) %>% 
                select(ano, porc.georref) %>% 
                dygraph() %>% 
                dyRangeSelector() %>%
                dyLegend(show = "never")
        })
    }
    
    dygraph.tipo.obra <- function(dado, tipo.obra, tipo.localidade.selecionada, localidade.selecionada) {
        if (tipo.localidade.selecionada == "microrregiao") {
            dado.filtr <- dado %>% 
                filter(microregiao == localidade.selecionada)
        } else if (tipo.localidade.selecionada == "mesorregiao") {
            dado.filtr <- dado %>% 
                filter(mesoregiao == localidade.selecionada)
        } else {
            dado.filtr <- dado
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
                dyRangeSelector() %>%
                dyLegend(show = "never")
        })
    }
    
    output$dygraph_georref <- dygraph.georref(obras.2013, tipo.localidade.selecionada.georref, localidade.selecionada.georref)
    
    output$dygraph_tipo_obra <- dygraph.tipo.obra(custo.efetivo.obras, get.top.10.tipo.obra(custo.efetivo.obras)[1], 
                                                  tipo.localidade.selecionada.tipo.obra, localidade.selecionada.tipo.obra)
    
    output$ranking_georref <- renderPlot({
        plot.ranking.georref(localidades.georref, input$select_localidade_georref)
    })
    
    output$ranking_tipo_obra <- renderPlot({
        plot.ranking.tipo.obra(localidades.custo.efetivo, input$select_localidade_tipo_obra)
    })
    
    filtra.regiao <- function(dado, tipo.localidade, mapa) {
        localidade.selecionada <- ifelse(mapa == "georref", localidade.selecionada.georref, localidade.selecionada.tipo.obra)
        localidades.mapa <- dado
        if (tipo.localidade == "microrregiao") {
            localidades.mapa <- localidades.mapa %>% 
                filter(microregiao == localidade.selecionada)
        } else if (tipo.localidade == "mesorregiao") {
            localidades.mapa <- localidades.mapa %>% 
                filter(mesoregiao == localidade.selecionada)
        }
        localidades.mapa
    }
    
    muda.mapa.e.ranking.georref <- function(localidades.georref) {
        localidades.mapa <- filtra.regiao(localidades.georref, tipo.localidade.selecionada.georref, "georref")
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
            plot.ranking.georref(localidades.mapa, localidade.selecionada.georref)
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

                muda.input.localidades.georref(localidades.georref)
                
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
                output$dygraph_georref <- dygraph.georref(obras.2013, tipo.localidade.selecionada.georref, localidade.selecionada.georref)
            }

            localidades.georref <- get.porc.municipios.georref(obras.2013, localidades.desc, localidade.selecionada.georref, ano.inicial.georref, ano.final.georref)

            muda.mapa.e.ranking.georref(localidades.georref)
        }
    },
    priority = 1)
    
    muda.input.localidade.tipo.obra <- function(localidades.custo.efetivo) {
        if (tipo.localidade.selecionada.tipo.obra == "municipio") {
            localidades.input <- localidades.custo.efetivo %>% arrange(nome) %>% pull(nome)
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
        localidades.mapa <- filtra.regiao(localidades.custo.efetivo, tipo.localidade.selecionada.tipo.obra, "tipo.obra")
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
                                                                                scientific = FALSE)
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
            plot.ranking.tipo.obra(localidades.mapa, localidade.selecionada.tipo.obra)
        })
    }
    
    observeEvent({
        input$select_tipo_obra
    }, {
        print("input$select_tipo_obra")
        if(!is.null(input$dygraph_tipo_obra_date_window)){
            tipo.obra.selecionada <<- input$select_tipo_obra
            
            localidades.custo.efetivo <- get.localidades.custo.efetivo(localidades.desc)
            muda.input.localidade.tipo.obra(localidades.custo.efetivo)
            
            output$dygraph_tipo_obra <- dygraph.tipo.obra(custo.efetivo.obras, tipo.obra.selecionada, 
                                                          tipo.localidade.selecionada.tipo.obra, localidade.selecionada.tipo.obra)
            
            muda.mapa.tipo.obra.e.ranking(localidades.custo.efetivo)
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
                print("input$dygraph_tipo_obra_date_window")
                
                ano.inicial.tipo.obra <<- ano1
                ano.final.tipo.obra <<- ano2
                
                localidades.custo.efetivo <- get.localidades.custo.efetivo(localidades.desc)
                
                muda.input.localidade.tipo.obra(localidades.custo.efetivo)
                
                muda.mapa.tipo.obra.e.ranking(localidades.custo.efetivo)
            }
        }
    })
    
    observeEvent({
        input$select_localidade_tipo_obra
    }, {
        if(!is.null(input$dygraph_tipo_obra_date_window)){
            localidade.selecionada.tipo.obra <<- input$select_localidade_tipo_obra
            if (tipo.localidade.selecionada.tipo.obra != "municipio") {
                print("input$select_localidade_tipo_obra")
                output$dygraph_tipo_obra <- dygraph.tipo.obra(custo.efetivo.obras, tipo.obra.selecionada, 
                                                              tipo.localidade.selecionada.tipo.obra, 
                                                              localidade.selecionada.tipo.obra)

                ano.inicial.tipo.obra <<- round(input$dygraph_tipo_obra_date_window[[1]])
                ano.final.tipo.obra <<- round(input$dygraph_tipo_obra_date_window[[2]])
            }
            
            localidades.custo.efetivo <- get.localidades.custo.efetivo(localidades.desc)
            
            muda.mapa.tipo.obra.e.ranking(localidades.custo.efetivo)
        }
    })
    
    observeEvent({
        input$select_tipo_localidade_georref
    }, {
        tipo.localidade.selecionada.georref <<- input$select_tipo_localidade_georref
        localidades.georref <- get.porc.municipios.georref(obras.2013, localidades.desc, localidade.selecionada.georref, ano.inicial.georref, ano.final.georref)
        

        muda.input.localidades.georref(localidades.georref)
        
        muda.mapa.e.ranking.georref(localidades.georref)
    })
    
    observeEvent({
        input$select_tipo_localidade_tipo_obra
    }, {
        tipo.localidade.selecionada.tipo.obra <<- input$select_tipo_localidade_tipo_obra
        localidades.custo.efetivo <- get.localidades.custo.efetivo(localidades.desc)
        
        muda.input.localidade.tipo.obra(localidades.custo.efetivo)
        
        muda.mapa.tipo.obra.e.ranking(localidades.custo.efetivo)
    })
    
    observeEvent({
        input$select_tipo_representacao_georref
    }, {
        tipo.representacao.georref <<- input$select_tipo_representacao_georref
        
        muda.mapa.e.ranking.georref(localidades.georref)
    })
}

shinyApp(ui, server)
