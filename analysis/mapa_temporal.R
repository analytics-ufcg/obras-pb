library(shiny)
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
altura.mapa <- 400
altura.linha.tempo <- 200
ui <- fluidPage(
    selectInput("select_municipio", label = h3("Selecione o município"), 
                choices = municipios.georref.porc$nome.x),
    sidebarLayout(  
        sidebarPanel(
            plotOutput("ranking1", height = altura.mapa + altura.linha.tempo)  
        ),
        
        # Create a spot for the barplot
        mainPanel(
            leafletOutput("map1", height = altura.mapa),
            dygraphOutput("dygraph1", height = altura.linha.tempo)
        ),
        position = "right"
    )
)

# Lógica do sistema
server <- function(input, output, session) {
    v <- reactiveValues(msg = "")

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
            "Proporção de obras</br> georreferenciadas em %",
            mapa_paraiba_georreferenciada@data$cor.borda,
            mapa_paraiba_georreferenciada@data$largura.borda
        )
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
                
                leafletProxy("map1", data = mapa_paraiba_georreferenciada) %>%
                    clearGroup( group = "municipios-poligono" ) %>%
                    clearControls() %>%
                    adiciona.poligonos.e.legenda(cores,
                                                 mapa_paraiba_georreferenciada@data$porc.georref, 
                                                 mapa_paraiba_georreferenciada@data$Nome_Munic, 
                                                 get.popup.georref(mapa_paraiba_georreferenciada@data$Nome_Munic, 
                                                           mapa_paraiba_georreferenciada@data$total.obras, 
                                                           mapa_paraiba_georreferenciada@data$qtde.georref, 
                                                           mapa_paraiba_georreferenciada@data$porc.georref,
                                                           mapa_paraiba_georreferenciada@data$possui.georref.mas.tem.coordenadas.fora.municipio),
                                                 "Proporção de obras</br> georreferenciadas em %",
                                                 mapa_paraiba_georreferenciada@data$cor.borda,
                                                 mapa_paraiba_georreferenciada@data$largura.borda)
                
                output$ranking1 <- renderPlot({
                    plot.ranking(municipios.georref.porc, input$select_municipio)
                })
            }
        }
    })
}

shinyApp(ui, server)
