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

municipios.georref.porc <<- get.porc.municipios.georref(obras.2013, "Não selecionado")

mapa_paraiba_georreferenciada <- get.mapa.paraiba.georref(mapa_paraiba, municipios.georref.porc)

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
                                leafletOutput("map1", height = altura.mapa)
                            ),
                            box(width = NULL,
                                dygraphOutput("dygraph1", height = altura.linha.tempo)
                            )
                     ),
                     column(width = 4,
                            box(width = NULL,
                                height = altura.input.municipio,
                                status = "warning",
                                selectInput("select_municipio", label = h3("Selecione o município"), 
                                            choices = municipios.georref.porc$nome.x)
                            ),
                            box(width = NULL,
                                status = "warning",
                                plotOutput("ranking1", height = altura.mapa + altura.linha.tempo - altura.input.municipio + altura.ajusta.margem)
                            )
                     )  
                 )
                     
             ),
            tabPanel(
                "Tipoos de obras", 
                 p("Inserir aqui mapa dos tipos de obras")
            )
        )
    )
)

# Lógica do sistema
server <- function(input, output, session) {
    v <- reactiveValues(msg = "")

    municipios.georref.porc.top.3 <<- get.top.3.municipios.georref(municipios.georref.porc, municipios)
    
    trofeu.icon <- icons(
        iconUrl = "trofeu.png",
        iconWidth = 38, iconHeight = 38,
        iconAnchorX = 19, iconAnchorY = 30
    )
    
    cores <- paleta.de.cores(dado = mapa_paraiba_georreferenciada@data$porc.georref, reverse = TRUE)
    
    output$map1 <- renderLeaflet({
        cria.mapa(
            mapa_paraiba_georreferenciada, 
            mapa_paraiba_georreferenciada@data$porc.georref, 
            mapa_paraiba_georreferenciada@data$Nome_Munic, 
            get.popup.georref(mapa_paraiba_georreferenciada@data$Nome_Munic, 
                      mapa_paraiba_georreferenciada@data$total.obras, 
                      mapa_paraiba_georreferenciada@data$qtde.georref, 
                      mapa_paraiba_georreferenciada@data$porc.georref,
                      mapa_paraiba_georreferenciada@data$possui.georref.mas.tem.coordenadas.fora.municipio),
            cores, 
            "Obras georreferenciadas (%)",
            mapa_paraiba_georreferenciada@data$cor.borda,
            mapa_paraiba_georreferenciada@data$largura.borda
        ) %>% 
            addMarkers(lng = ~lon, lat = ~lat, icon = trofeu.icon, data = municipios.georref.porc.top.3)
    })
    
    output$dygraph1 <- renderDygraph({
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
    
    output$ranking1 <- renderPlot({
        plot.ranking(municipios.georref.porc, input$select_municipio)
    })
    
    observeEvent({
        input$select_municipio
        input$dygraph1_date_window
        }, {
        if(!is.null(input$dygraph1_date_window)){
            ano1 <- round(input$dygraph1_date_window[[1]])
            ano2 <- round(input$dygraph1_date_window[[2]])
            municipio <- input$select_municipio
            
            if (!exists("ano.inicial") || !exists("ano.final") || !exists("municipio.selecionado") || 
                ano.inicial != ano1 || ano.final != ano2 || municipio != municipio.selecionado) {
                ano.inicial <<- ano1
                ano.final <<- ano2
                municipio.selecionado <<- municipio
                
                municipios.georref.porc <<- get.porc.municipios.georref(obras.2013, municipio.selecionado, ano.inicial, ano.final)
                
                updateSelectInput(session, inputId = "municipio", choices = municipios.georref.porc$nome.x)
                
                mapa_paraiba_georreferenciada <- get.mapa.paraiba.georref(mapa_paraiba, municipios.georref.porc)
                
                cores <- paleta.de.cores(dado = mapa_paraiba_georreferenciada@data$porc.georref, reverse = TRUE)
                
                municipios.georref.porc.top.3 <<- get.top.3.municipios.georref(municipios.georref.porc, municipios)
            
                leafletProxy("map1", data = mapa_paraiba_georreferenciada) %>%
                    clearGroup( group = "municipios-poligono" ) %>%
                    clearMarkers() %>%
                    clearControls() %>%
                    adiciona.poligonos.e.legenda(cores,
                                                 mapa_paraiba_georreferenciada@data$porc.georref, 
                                                 mapa_paraiba_georreferenciada@data$Nome_Munic, 
                                                 get.popup.georref(mapa_paraiba_georreferenciada@data$Nome_Munic, 
                                                           mapa_paraiba_georreferenciada@data$total.obras, 
                                                           mapa_paraiba_georreferenciada@data$qtde.georref, 
                                                           mapa_paraiba_georreferenciada@data$porc.georref,
                                                           mapa_paraiba_georreferenciada@data$possui.georref.mas.tem.coordenadas.fora.municipio),
                                                 "Obras georreferenciadas (%)",
                                                 mapa_paraiba_georreferenciada@data$cor.borda,
                                                 mapa_paraiba_georreferenciada@data$largura.borda) %>% 
                    addMarkers(lng = ~lon, lat = ~lat, icon = trofeu.icon, data = municipios.georref.porc.top.3)
                
                output$ranking1 <- renderPlot({
                    plot.ranking(municipios.georref.porc, input$select_municipio)
                })
            }
        }
    })
}

shinyApp(ui, server)
