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
tipos.das.obras <- read.csv("tipos_obra.csv")
municipios.pb <- read.csv("municipios_pb.csv")
mapa_paraiba <- readOGR("mapa_paraiba_ibge/Municipios.shp")

# Pega dados
drv <- DBI::dbDriver("PostgreSQL")

con1 <- DBI::dbConnect(drv,
                       host = config::get("host"),
                       port = config::get("port"),
                       user = config::get("user"),
                       password = config::get("password"),
                       dbname = config::get("dbname1")
)

con2 <- DBI::dbConnect(drv,
                       host = config::get("host"),
                       port = config::get("port"),
                       user = config::get("user"),
                       password = config::get("password"),
                       dbname = config::get("dbname2")
)

GeoPBUtils::get.data(con1, con2, mapa_paraiba)

dbDisconnect(con1)
dbDisconnect(con2)

# Manipulação de dados
municipios <- data.frame(codigo_ibge = mapa_paraiba$GEOCODIG_M, lat = coordinates(mapa_paraiba)[,2], lon = coordinates(mapa_paraiba)[,1])

obras.2013 <- get.georreferencia.inputada(obra, localidade, tipos.das.obras, municipios, obra.georref.centroide.sumarizado, 2013) %>% 
    filter(codigo_ibge != 0)

custo.efetivo.obras <- get.custos.efetivos(obras.2013)

municipios.georref.porc <<- get.porc.municipios.georref(obras.2013, "Não selecionado")
municipios.tipo.obra.custo.efetivo <<- get.custo.efetivo.tipo.obra(custo.efetivo.obras, "Não selecionado",
                                                                   get.top.10.tipo.obra(custo.efetivo.obras)[1])

mapa_paraiba_georreferenciada <- get.mapa.paraiba.georref(mapa_paraiba, municipios.georref.porc)
mapa_paraiba_custo_efetivo <- get.mapa.paraiba.custo.efetivo(mapa_paraiba, municipios.tipo.obra.custo.efetivo)

# Interface do usuário
altura.mapa <- 650
altura.linha.tempo <- 200
altura.input.municipio <- 150
altura.ajusta.margem <- 20

ui <- dashboardPage(
    dashboardHeader( 
        title = span(
                    tagList(
                        img(src = "tce-cropped.png", height = 45), 
                        "Obras georreferenciadas"
                    )
                ),
        titleWidth = 400
    ),
    dashboardSidebar( 
        disable = TRUE
    ),
    dashboardBody(
        tabsetPanel(type = "tabs",
            tabPanel(
                "Obras georreferenciadas por município", 
                 fluidRow(
                     column(width = 8,
                            box(width = NULL, solidHeader = TRUE,
                                leafletOutput("mapa_georref", height = altura.mapa)
                            ),
                            box(width = NULL,
                                dygraphOutput("dygraph_georref", height = altura.linha.tempo)
                            )
                     ),
                     column(width = 4,
                            box(width = NULL,
                                height = altura.input.municipio,
                                status = "warning",
                                selectInput("select_municipio_georref", label = h3("Selecione o município"), 
                                            choices = municipios.georref.porc$nome.x)
                            ),
                            box(width = NULL,
                                status = "warning",
                                plotOutput("ranking_georref", height = altura.mapa + altura.linha.tempo - altura.input.municipio + altura.ajusta.margem)
                            )
                     )  
                 )
             ),
            tabPanel(
                "Tipos de obras",
                fluidRow(
                    column(width = 8,
                           box(width = NULL, solidHeader = TRUE,
                               leafletOutput("mapa_tipo_obra", height = altura.mapa)
                           ),
                           box(width = NULL,
                               dygraphOutput("dygraph_tipo_obra", height = altura.linha.tempo)
                           )
                    ),
                    column(width = 4,
                           box(width = NULL,
                               height = altura.input.municipio,
                               status = "warning",
                               selectInput("select_tipo_obra", label = h3("Selecione o tipo da obra"),
                                           choices = get.top.10.tipo.obra(custo.efetivo.obras))
                           ),
                           box(width = NULL,
                               height = altura.input.municipio,
                               status = "warning",
                               selectInput("select_municipio_tipo_obra", label = h3("Selecione o município"),
                                           choices = municipios.tipo.obra.custo.efetivo$nome)
                           ),
                           box(width = NULL,
                               status = "warning",
                               plotOutput("ranking_tipo_obra", height = altura.mapa + altura.linha.tempo - altura.input.municipio - altura.input.municipio + altura.ajusta.margem)
                           )
                    )
                )
            )
        )
    )
)

# Lógica do sistema
server <- function(input, output, session) {
    v <- reactiveValues(msg = "")
    
    cores.georref <- paleta.de.cores(dado = mapa_paraiba_georreferenciada@data$porc.georref, reverse = TRUE)
    cores.custo.efetivo <- paleta.de.cores(dado = mapa_paraiba_custo_efetivo@data$custo.efetivo.log)
    
    output$mapa_georref <- renderLeaflet({
        cria.mapa(
            mapa_paraiba_georreferenciada, 
            mapa_paraiba_georreferenciada@data$porc.georref, 
            mapa_paraiba_georreferenciada@data$Nome_Munic, 
            get.popup.georref(mapa_paraiba_georreferenciada@data$Nome_Munic, 
                      mapa_paraiba_georreferenciada@data$total.obras, 
                      mapa_paraiba_georreferenciada@data$qtde.georref, 
                      mapa_paraiba_georreferenciada@data$porc.georref,
                      mapa_paraiba_georreferenciada@data$possui.georref.mas.tem.coordenadas.fora.municipio),
            cores.georref, 
            "Obras georreferenciadas (%)",
            "municipios-poligono-georref",
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
            "municipios-poligono-custo-efetivo",
            mapa_paraiba_custo_efetivo@data$cor.borda,
            mapa_paraiba_custo_efetivo@data$largura.borda
        )
    })
    
    output$dygraph_georref <- renderDygraph({
        # start dygraph with all the states
        obras.2013 %>% 
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
    
    output$dygraph_tipo_obra <- dygraph.tipo.obra(custo.efetivo.obras, get.top.10.tipo.obra(custo.efetivo.obras)[1])
    
    output$ranking_georref <- renderPlot({
        plot.ranking.georref(municipios.georref.porc, input$select_municipio_georref)
    })
    
    output$ranking_tipo_obra <- renderPlot({
        plot.ranking.tipo.obra(municipios.tipo.obra.custo.efetivo, input$select_municipio_tipo_obra)
    })
    
    observeEvent({
        input$select_municipio_georref
        input$dygraph_georref_date_window
        }, {
        if(!is.null(input$dygraph_georref_date_window)){
            ano1 <- round(input$dygraph_georref_date_window[[1]])
            ano2 <- round(input$dygraph_georref_date_window[[2]])
            municipio <- input$select_municipio_georref
            
            if (!exists("ano.inicial") || !exists("ano.final") || !exists("municipio.selecionado") || 
               
                ano.inicial != ano1 || ano.final != ano2 || municipio != municipio.selecionado) {
                ano.inicial <<- ano1
                ano.final <<- ano2
                municipio.selecionado <<- municipio
                

                
                
                municipios.georref.porc <<- get.porc.municipios.georref(obras.2013, municipio.selecionado, ano.inicial, ano.final)
                
                mapa_paraiba_georreferenciada <- get.mapa.paraiba.georref(mapa_paraiba, municipios.georref.porc)
                
                if (ano1 == ano.inicial || ano2 == ano.final){
                    municipios.input <- municipios.georref.porc %>% arrange(nome.x) %>% pull(nome.x)
                    if (municipio.selecionado %in% municipios.input) { 
                        updateSelectInput(session, inputId = "select_municipio_georref", 
                                      choices = municipios.input,
                                      selected = municipio.selecionado)
                    } else {
                        updateSelectInput(session, inputId = "select_municipio_georref", 
                                      choices = municipios.input,
                                      selected = municipios.input[1])
                        municipio.selecionado <<- municipios.input[1]
                                      
                    }
                }
                
                cores.georref <- paleta.de.cores(dado = mapa_paraiba_georreferenciada@data$porc.georref, reverse = TRUE)
    
            
                leafletProxy("mapa_georref", data = mapa_paraiba_georreferenciada) %>%
                    clearGroup( group = "municipios-poligono-georref" ) %>%
                    clearControls() %>%
                    adiciona.poligonos.e.legenda(cores.georref,
                                                 mapa_paraiba_georreferenciada@data$porc.georref, 
                                                 mapa_paraiba_georreferenciada@data$Nome_Munic, 
                                                 get.popup.georref(mapa_paraiba_georreferenciada@data$Nome_Munic, 
                                                           mapa_paraiba_georreferenciada@data$total.obras, 
                                                           mapa_paraiba_georreferenciada@data$qtde.georref, 
                                                           mapa_paraiba_georreferenciada@data$porc.georref,
                                                           mapa_paraiba_georreferenciada@data$possui.georref.mas.tem.coordenadas.fora.municipio),
                                                 "Obras georreferenciadas (%)",
                                                 "municipios-poligono-georref",
                                                 mapa_paraiba_georreferenciada@data$cor.borda,
                                                 mapa_paraiba_georreferenciada@data$largura.borda)
                
                output$ranking_georref <- renderPlot({
                    plot.ranking.georref(municipios.georref.porc, municipio.selecionado)
                })
            }
        }
    })
    
    observeEvent({
        input$select_municipio_tipo_obra
        input$select_tipo_obra
        input$dygraph_tipo_obra_date_window
    }, {
        if(!is.null(input$dygraph_tipo_obra_date_window)){
            ano1 <- round(input$dygraph_tipo_obra_date_window[[1]])
            ano2 <- round(input$dygraph_tipo_obra_date_window[[2]])
            tipo.obra <- input$select_tipo_obra
            municipio <- input$select_municipio_tipo_obra
            
            if (!exists("ano.inicial.tipo.obra") || !exists("ano.final.tipo.obra") || !exists("tipo.obra.selecionada") || !exists("municipio.selecionado.tipo.obra") || 
                ano.inicial.tipo.obra != ano1 || ano.final.tipo.obra != ano2 || tipo.obra != tipo.obra.selecionada || municipio != municipio.selecionado.tipo.obra) {
                
                ano.inicial.tipo.obra <<- ano1
                ano.final.tipo.obra <<- ano2
                
                
                municipios.tipo.obra.custo.efetivo <<- get.custo.efetivo.tipo.obra(custo.efetivo.obras, 
                                                                                   municipio, 
                                                                                   tipo.obra,
                                                                                   ano.inicial.tipo.obra, 
                                                                                   ano.final.tipo.obra)
                
                if (exists("municipio.selecionado.tipo.obra") && municipio == municipio.selecionado.tipo.obra) {
                    updateSelectInput(session, inputId = "select_municipio_tipo_obra", 
                                      choices = municipios.tipo.obra.custo.efetivo %>% arrange(nome) %>% pull(nome))
                }
                
                if (exists("tipo.obra.selecionada") && tipo.obra != tipo.obra.selecionada){
                    output$dygraph_tipo_obra <- dygraph.tipo.obra(custo.efetivo.obras, tipo.obra)
                }
                
                tipo.obra.selecionada <<- tipo.obra
                municipio.selecionado.tipo.obra <<- municipio
                
                mapa_paraiba_custo_efetivo <- get.mapa.paraiba.custo.efetivo(mapa_paraiba, municipios.tipo.obra.custo.efetivo)
                
                cores.custo.efetivo <- paleta.de.cores(dado = mapa_paraiba_custo_efetivo@data$custo.efetivo.log)
                
                leafletProxy("mapa_tipo_obra", data = mapa_paraiba_custo_efetivo) %>%
                    clearGroup( group = "municipios-poligono-custo-efetivo" ) %>%
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
                                                 "municipios-poligono-tipo-obra",
                                                 mapa_paraiba_custo_efetivo@data$cor.borda,
                                                 mapa_paraiba_custo_efetivo@data$largura.borda) 
                
                output$ranking_tipo_obra <- renderPlot({
                    plot.ranking.tipo.obra(municipios.tipo.obra.custo.efetivo, input$select_municipio_tipo_obra)
                })
            }
        }
    })
}

shinyApp(ui, server)
