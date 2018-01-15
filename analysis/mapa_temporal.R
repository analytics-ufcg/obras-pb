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

tipo.obra.selecionada <<- get.top.10.tipo.obra(custo.efetivo.obras)[1]

municipios.georref.porc <<- get.porc.municipios.georref(obras.2013, cidade.default(obras.2013, "nome.x"))
municipios.tipo.obra.custo.efetivo <<- get.custo.efetivo.tipo.obra(custo.efetivo.obras, cidade.default(custo.efetivo.obras, "nome"),
                                                                   tipo.obra.selecionada)

municipio.selecionado.tipo.obra <<- cidade.default(municipios.tipo.obra.custo.efetivo, "nome")
ano.inicial.tipo.obra <<- 0
ano.final.tipo.obra <<- 3000
tag.mapa.custo.efetivo <<- "municipios-poligono-custo-efetivo"

municipio.selecionado.georref <<- cidade.default(municipios.georref.porc, "nome.x")
ano.inicial.georref <<- 0
ano.final.georref <<- 3000
tag.mapa.georref <<- "municipios-poligono-georref"

municipios.tipo.obra.custo.efetivo <<- add.borda(municipios.tipo.obra.custo.efetivo, municipio.selecionado.tipo.obra)

mapa_paraiba_georreferenciada <- get.mapa.paraiba.georref(mapa_paraiba, municipios.georref.porc)
mapa_paraiba_custo_efetivo <- get.mapa.paraiba.custo.efetivo(mapa_paraiba, municipios.tipo.obra.custo.efetivo)

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
        titleWidth = 400
    ),
    dashboardSidebar( 
        disable = TRUE
    ),
    dashboardBody(
        tags$head(tags$link(rel="shortcut icon", href="tce-cropped.png")),
        tabsetPanel(type = "tabs",
            tabPanel(
                "Obras georreferenciadas por município", 
                fluidRow(
                    height = "100vh",
                         column(width = 7,
                                box(width = NULL, solidHeader = TRUE,
                                    leafletOutput("mapa_georref", height = "50vh")
                                ),
                                box(width = NULL,
                                    dygraphOutput("dygraph_georref",  height = "25vh")
                                )
                         ),
                         column(width = 5,
                                height = "100vh",
                                box(width = NULL,
                                    height = "15vh",
                                    status = "warning",
                                    selectInput("select_municipio_georref", label = h3("Selecione o município"), 
                                                choices = municipios.georref.porc$nome.x, 
                                                selected = cidade.default(municipios.georref.porc, "nome.x"))
                                ),
                                box(height = "64vh",
                                    width = "50vw",
                                    status = "warning",
                                    plotOutput("ranking_georref", height = "60vh")
                                )
                         )  
                )
            ),
            tabPanel(
                "Tipos de obras",
                fluidRow(
                    height = "100vh",
                    column(width = 7,
                           box(width = NULL, solidHeader = TRUE,
                               leafletOutput("mapa_tipo_obra", height = "50vh")
                           ),
                           box(width = NULL,
                               dygraphOutput("dygraph_tipo_obra", height = "25vh")
                           )
                    ),
                    column(width = 5,
                           height = "100vh",
                           box(width = NULL,
                               height = "15vh",
                               status = "warning",
                               selectInput("select_tipo_obra", label = h3("Selecione o tipo da obra"),
                                           choices = get.top.10.tipo.obra(custo.efetivo.obras),
                                           selected = tipo.obra.selecionada)
                           ),
                           box(width = NULL,
                               height = "15vh",
                               status = "warning",
                               selectInput("select_municipio_tipo_obra", label = h3("Selecione o município"),
                                           choices = municipios.tipo.obra.custo.efetivo$nome, 
                                           selected = municipio.selecionado.tipo.obra)
                           ),
                           box(width = NULL,
                               height = "47vh",
                               status = "warning",
                               plotOutput("ranking_tipo_obra", height = "45vh")
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
            tag.mapa.georref,
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
    
    muda.mapa.e.ranking.georref <- function(municipios.georref.porc) {
        mapa_paraiba_georreferenciada <- get.mapa.paraiba.georref(mapa_paraiba, municipios.georref.porc)
        
        cores.georref <- paleta.de.cores(dado = mapa_paraiba_georreferenciada@data$porc.georref, reverse = TRUE)
        
        leafletProxy("mapa_georref", data = mapa_paraiba_georreferenciada) %>%
            clearGroup( group = tag.mapa.georref ) %>%
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
                                         tag.mapa.georref,
                                         mapa_paraiba_georreferenciada@data$cor.borda,
                                         mapa_paraiba_georreferenciada@data$largura.borda)
        
        output$ranking_georref <- renderPlot({
            plot.ranking.georref(municipios.georref.porc, municipio.selecionado.georref)
        })
    }
    
    muda.input.municipios.georref <- function(municipios.georref.porc) {
        municipios.input <- municipios.georref.porc %>% arrange(nome.x) %>% pull(nome.x)
        if (!municipio.selecionado.georref %in% municipios.input) { 
            municipio.selecionado.georref <<- municipios.input[1]
        }
        updateSelectInput(session, inputId = "select_municipio_georref", 
                          choices = municipios.input,
                          selected = municipio.selecionado.georref)
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
                municipios.georref.porc <- get.porc.municipios.georref(obras.2013, municipio.selecionado.georref, ano.inicial.georref, ano.final.georref)

                muda.input.municipios.georref(municipios.georref.porc)
                
                muda.mapa.e.ranking.georref(municipios.georref.porc)
            }
        }
    },
    priority = 2)

    observeEvent({
        input$select_municipio_georref
    }, {
        if(!is.null(input$dygraph_georref_date_window)){
            municipio.selecionado.georref <<- input$select_municipio_georref

            municipios.georref.porc <- get.porc.municipios.georref(obras.2013, municipio.selecionado.georref, ano.inicial.georref, ano.final.georref)

            muda.mapa.e.ranking.georref(municipios.georref.porc)
        }
    },
    priority = 1)
    
    muda.input.municipio.tipo.obra <- function(municipios.tipo.obra.custo.efetivo) {
        municipios.input <- municipios.tipo.obra.custo.efetivo %>% arrange(nome) %>% pull(nome)
        if (!municipio.selecionado.tipo.obra %in% municipios.input) {
            municipio.selecionado.tipo.obra <<- municipios.input[1]
        } 
        updateSelectInput(session, inputId = "select_municipio_tipo_obra", 
                          choices = municipios.input,
                          selected = municipio.selecionado.tipo.obra)
    }
    
    muda.mapa.tipo.obra <- function(municipios.tipo.obra.custo.efetivo) {
        mapa_paraiba_custo_efetivo <- get.mapa.paraiba.custo.efetivo(mapa_paraiba, municipios.tipo.obra.custo.efetivo)
        
        cores.custo.efetivo <- paleta.de.cores(dado = mapa_paraiba_custo_efetivo@data$custo.efetivo.log)
        
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
                                         mapa_paraiba_custo_efetivo@data$cor.borda,
                                         mapa_paraiba_custo_efetivo@data$largura.borda)
    }
    
    get.municipios.tipo.obra.custo.efetivo <- function() {
        municipios.tipo.obra.custo.efetivo <- get.custo.efetivo.tipo.obra(custo.efetivo.obras, 
                                                                          municipio.selecionado.tipo.obra, 
                                                                          tipo.obra.selecionada,
                                                                          ano.inicial.tipo.obra, 
                                                                          ano.final.tipo.obra)
        
        municipios.tipo.obra.custo.efetivo %>% 
            add.borda(municipio.selecionado.tipo.obra)
    }
    
    muda.mapa.tipo.obra.e.ranking <- function(municipios.tipo.obra.custo.efetivo) {
        muda.mapa.tipo.obra(municipios.tipo.obra.custo.efetivo)
        
        output$ranking_tipo_obra <- renderPlot({
            plot.ranking.tipo.obra(municipios.tipo.obra.custo.efetivo, municipio.selecionado.tipo.obra)
        })
    }
    
    observeEvent({
        input$select_tipo_obra
    }, {
        if(!is.null(input$dygraph_tipo_obra_date_window)){
            tipo.obra.selecionada <<- input$select_tipo_obra
            
            municipios.tipo.obra.custo.efetivo <- get.municipios.tipo.obra.custo.efetivo()
            muda.input.municipio.tipo.obra(municipios.tipo.obra.custo.efetivo)
            
            output$dygraph_tipo_obra <- dygraph.tipo.obra(custo.efetivo.obras, tipo.obra.selecionada)
            
            muda.mapa.tipo.obra.e.ranking(municipios.tipo.obra.custo.efetivo)
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
                
                municipios.tipo.obra.custo.efetivo <- get.municipios.tipo.obra.custo.efetivo()
                
                muda.input.municipio.tipo.obra(municipios.tipo.obra.custo.efetivo)
                
                muda.mapa.tipo.obra.e.ranking(municipios.tipo.obra.custo.efetivo)
            }
        }
    })
    
    observeEvent({
        input$select_municipio_tipo_obra
    }, {
        if(!is.null(input$dygraph_tipo_obra_date_window)){
            municipio.selecionado.tipo.obra <<- input$select_municipio_tipo_obra
            
            municipios.tipo.obra.custo.efetivo <- get.municipios.tipo.obra.custo.efetivo()
            
            muda.mapa.tipo.obra.e.ranking(municipios.tipo.obra.custo.efetivo)
        }
    })
}

shinyApp(ui, server)
