library(shiny)
library(leaflet)
library(rgdal)
library(tidyverse)
library(RColorBrewer)
library(dygraphs)
#devtools::install_github("analytics-ufcg/geopbutils")
library(GeoPBUtils)
library(RPostgreSQL)

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

GeoPBUtils::get.data(con1, con2)

dbDisconnect(con1)
dbDisconnect(con2)

# Importa tabelas restantes
tipos.das.obras <- read.csv("tipos_obra.csv")
municipios.pb <- read.csv("municipios_pb.csv")
mapa_paraiba <- readOGR("mapa_paraiba_ibge/Municipios.shp")

# Funções que auxiliam plots
plot.ranking <- function(dado) {
    renderPlot({
        dado %>% 
            arrange(prop.nao.georref) %>% 
            head(10) %>% 
            ggplot(aes(x = reorder(nome.x, -prop.nao.georref), y = prop.nao.georref)) +
            geom_bar(stat="identity") +
            labs(x = "Município",
                 y = "Proporção de obras não georreferenciadas", 
                 title = "Top 10 municípios que\n mais georreferenciam") +
            coord_flip()
    })
}

get.mapa.paraiba.nao.georref <- function(mapa_paraiba, municipios.nao.georref.prop) {
    mapa_paraiba_nao_georreferenciada <- mapa_paraiba
    
    mapa_paraiba_nao_georreferenciada@data <- mapa_paraiba_nao_georreferenciada@data %>%
        left_join(municipios.nao.georref.prop,
                  by = c("GEOCODIG_M" = "codigo_ibge"))
    
    mapa_paraiba_nao_georreferenciada
}

get.municipios.nao.georref.prop <- function(dado, ano.inicial = 0, ano.final = 3000) {
    dado %>%
        filter(ano >= ano.inicial,
               ano <= ano.final) %>%
        group_by(codigo_ibge, nome.x) %>%
        summarise(
            total.nao.georref = n(),
            qtde.nao.georref = sum(is.inputado),
            prop.nao.georref = (qtde.nao.georref / total.nao.georref) * 100
        )
}

# Manipulação de dados
municipios <- data.frame(codigo_ibge = mapa_paraiba$GEOCODIG_M, lat = coordinates(mapa_paraiba)[,2], lon = coordinates(mapa_paraiba)[,1])

obras.2013 <- get.georreferencia.inputada(obra, localidade, tipos.das.obras, municipios, obra.georref.centroide.sumarizado, 2013) %>% 
    filter(codigo_ibge != 0)

municipios.nao.georref.prop <- get.municipios.nao.georref.prop(obras.2013)

mapa_paraiba_nao_georreferenciada <- get.mapa.paraiba.nao.georref(mapa_paraiba, municipios.nao.georref.prop)

# Interface do usuário
altura.mapa <- 400
altura.linha.tempo <- 200
ui <- fluidPage(
    textOutput("message", container = h3),
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

    cores <- GeoPBUtils::paleta.de.cores(dado = mapa_paraiba_nao_georreferenciada@data$prop.nao.georref)
    
    output$map1 <- renderLeaflet({
        cria.mapa(
            mapa_paraiba_nao_georreferenciada, 
            mapa_paraiba_nao_georreferenciada@data$prop.nao.georref, 
            mapa_paraiba_nao_georreferenciada@data$Nome_Munic, 
            paste0("Município: ", mapa_paraiba_nao_georreferenciada@data$Nome_Munic, "</br>Total de obras: ", mapa_paraiba_nao_georreferenciada@data$total.nao.georref, "</br>Quantidade de obras não georreferenciadas: ", mapa_paraiba_nao_georreferenciada@data$qtde.nao.georref, "</br>Proporção de obras não georreferenciadas em %: ", round(mapa_paraiba_nao_georreferenciada@data$prop.nao.georref, 2), "%"), 
            cores, 
            "Proporção de obras não</br> georreferenciadas em %"
        )
    })
    
    output$dygraph1 <- renderDygraph({
        # start dygraph with all the states
        obras.2013 %>% 
            group_by(ano) %>% 
            summarise(
                total.nao.georref = n(),
                qtde.nao.georref = sum(is.inputado),
                prop.nao.georref = (qtde.nao.georref / total.nao.georref) * 100
            ) %>% 
            select(ano, prop.nao.georref) %>% 
            dygraph() %>% 
            dyRangeSelector() %>%
            dyLegend(show = "never")
    })
    
    output$ranking1 <- plot.ranking(municipios.nao.georref.prop)
    
    observeEvent(input$dygraph1_date_window, {
        if(!is.null(input$dygraph1_date_window)){
            ano1 <- round(input$dygraph1_date_window[[1]])
            ano2 <- round(input$dygraph1_date_window[[2]])
            
            if (!exists("ano.inicial") || !exists("ano.final") || ano.inicial != ano1 || ano.final != ano2) {
                ano.inicial <<- ano1
                ano.final <<- ano2
                
                municipios.nao.georref.prop <- get.municipios.nao.georref.prop(obras.2013, ano.inicial, ano.final)
                
                mapa_paraiba_nao_georreferenciada <- mapa_paraiba_nao_georreferenciada <- get.mapa.paraiba.nao.georref(mapa_paraiba, municipios.nao.georref.prop)
                
                cores <- GeoPBUtils::paleta.de.cores(dado = mapa_paraiba_nao_georreferenciada@data$prop.nao.georref)
                
                leafletProxy("map1", data = mapa_paraiba_nao_georreferenciada) %>%
                    removeShape( layerId = ~GEOCODIG_M ) %>%
                    clearControls() %>%
                    adiciona.poligonos.e.legenda(cores,
                                                 mapa_paraiba_nao_georreferenciada@data$prop.nao.georref, 
                                                 mapa_paraiba_nao_georreferenciada@data$Nome_Munic, 
                                                 paste0("Município: ", 
                                                        mapa_paraiba_nao_georreferenciada@data$Nome_Munic, 
                                                        "</br>Total de obras: ", 
                                                        mapa_paraiba_nao_georreferenciada@data$total.nao.georref, 
                                                        "</br>Quantidade de obras não georreferenciadas: ", 
                                                        mapa_paraiba_nao_georreferenciada@data$qtde.nao.georref, 
                                                        "</br>Proporção de obras não georreferenciadas em %: ", 
                                                        round(mapa_paraiba_nao_georreferenciada@data$prop.nao.georref, 2), "%"), 
                                                 "Proporção de obras não</br> georreferenciadas em %")
                
                output$ranking1 <- plot.ranking(municipios.nao.georref.prop)
            }
        }
    })
}

shinyApp(ui, server)
