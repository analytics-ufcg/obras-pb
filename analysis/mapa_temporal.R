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

get.top.10.tipo.obra <- function(dado) {
    dado %>% 
        group_by(tipo_obra) %>% 
        summarise(quantidade.tipo.obra = n()) %>% 
        top_n(10, quantidade.tipo.obra) %>% 
        arrange(tipo_obra) %>% 
        pull(tipo_obra) 
}

get.custos.efetivos <- function(dado) {
    dado.filtrado <- dado %>% 
        filter(valor_obra > 1000,
               dimensao > 50) %>% 
        rename(nome = nome.x,
               tipo_obra = nome.y) %>% 
        filter(tipo_obra != "OUTRAS")
    
    top.10.tipo.obra <- get.top.10.tipo.obra(dado.filtrado)
    
    dado.filtrado %>% 
        filter(tipo_obra %in% top.10.tipo.obra)
}

get.custo.efetivo.tipo.obra <- function(dado, municipio.selecionado, tipo.obra = "PAVIMENTAÇÃO PARALEPÍPEDO", ano.inicial = 0, ano.final = 3000) {
    dado %>%
        filter(tipo_obra == tipo.obra) %>%
        select(valor_obra, dimensao, nome, codigo_ibge) %>%
        mutate(custo.efetivo = valor_obra/dimensao) %>% 
        group_by(nome, codigo_ibge) %>%
        summarise(
            custo.efetivo = median(custo.efetivo),
            custo.efetivo.log = log(custo.efetivo)
        ) %>%
        mutate(
            cor.borda = if_else(nome == municipio.selecionado, "blue", "black"),
            largura.borda = if_else(nome == municipio.selecionado, 5, 1)
        )
}

get.mapa.paraiba.custo.efetivo <- function(mapa_paraiba, municipios.custo.efetivo) {
    mapa_paraiba_custo_efetivo <- mapa_paraiba
    
    mapa_paraiba_custo_efetivo@data <- mapa_paraiba_custo_efetivo@data %>%
        left_join(municipios.custo.efetivo,
                  by = c("GEOCODIG_M" = "codigo_ibge"))
    
    mapa_paraiba_custo_efetivo
}

get.top.3.municipios.custo.efetivo <- function(dado, municipios) {
    dado %>%
        filter(codigo_ibge != 0) %>%
        ungroup() %>%
        top_n(-3, custo.efetivo) %>%
        left_join(municipios, by = "codigo_ibge")
}

adiciona.poligonos.e.legenda <- function(mapa, cores, valor.municipio, tooltip, janela, titulo, group, cor.borda = "black", largura.borda = 1) {
    addPolygons(mapa,
                opacity = 0.5,
                weight = largura.borda,
                fillColor = cores(valor.municipio),
                color = cor.borda,
                label = tooltip,
                popup = janela,
                fillOpacity = 1,
                group = group) %>%
        addLegend(position = "bottomright", pal = cores, values = valor.municipio,
                  title = titulo,
                  opacity = 1)
}

cria.mapa <- function(dado, valor.municipio, tooltip, janela, cores, titulo, group, cor.borda = "black", largura.borda = 1) {
    dado %>%
        leaflet() %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        adiciona.poligonos.e.legenda(cores, valor.municipio, tooltip, janela, titulo, group, cor.borda, largura.borda)
}

dygraph.tipo.obra <- function(dado, tipo.obra) {
    renderDygraph({
        dado %>%
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

municipios.georref.porc <<- get.porc.municipios.georref(obras.2013, municipios.georref.porc$nome.x[1])
municipios.tipo.obra.custo.efetivo <<- get.custo.efetivo.tipo.obra(custo.efetivo.obras, municipios.tipo.obra.custo.efetivo$nome[1],
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
                           )#,
                           # box(width = NULL,
                           #     status = "warning",
                           #     plotOutput("ranking_tipo_obra", height = altura.mapa + altura.linha.tempo - altura.input.municipio + altura.ajusta.margem)
                           # )
                    )
                )
            )
        )
    )
)

# Lógica do sistema
server <- function(input, output, session) {
    v <- reactiveValues(msg = "")

    municipios.georref.porc.top.3 <<- get.top.3.municipios.georref(municipios.georref.porc, municipios)
    municipios.custo.efetivo.top.3 <<- get.top.3.municipios.custo.efetivo(municipios.tipo.obra.custo.efetivo, municipios)
    
    trofeu.icon <- icons(
        iconUrl = "trofeu.png",
        iconWidth = 38, iconHeight = 38,
        iconAnchorX = 19, iconAnchorY = 30
    )
    
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
        ) %>% 
            addMarkers(lng = ~lon, lat = ~lat, icon = trofeu.icon, data = municipios.georref.porc.top.3)
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
        ) %>% 
            addMarkers(lng = ~lon, lat = ~lat, icon = trofeu.icon, data = municipios.custo.efetivo.top.3)
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
        plot.ranking(municipios.georref.porc, input$select_municipio_georref)
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
                
                updateSelectInput(session, inputId = "municipio", choices = municipios.georref.porc$nome.x)
                
                mapa_paraiba_georreferenciada <- get.mapa.paraiba.georref(mapa_paraiba, municipios.georref.porc)
                
                cores.georref <- paleta.de.cores(dado = mapa_paraiba_georreferenciada@data$porc.georref, reverse = TRUE)
                
                municipios.georref.porc.top.3 <<- get.top.3.municipios.georref(municipios.georref.porc, municipios)
            
                leafletProxy("mapa_georref", data = mapa_paraiba_georreferenciada) %>%
                    clearGroup( group = "municipios-poligono-georref" ) %>%
                    clearMarkers() %>%
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
                                                 mapa_paraiba_georreferenciada@data$largura.borda) %>% 
                    addMarkers(lng = ~lon, lat = ~lat, icon = trofeu.icon, data = municipios.georref.porc.top.3)
                
                output$ranking_georref <- renderPlot({
                    plot.ranking(municipios.georref.porc, input$select_municipio_georref)
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
                                                                                   tipo.obra.selecionada,
                                                                                   ano.inicial.tipo.obra, 
                                                                                   ano.final.tipo.obra)
                
                if (municipio == municipio.selecionado.tipo.obra) {
                    updateSelectInput(session, inputId = "select_municipio_tipo_obra", 
                                      choices = municipios.tipo.obra.custo.efetivo %>% arrange(nome) %>% pull(nome))
                }
                
                if (tipo.obra != tipo.obra.selecionada){
                    output$dygraph_tipo_obra <- dygraph.tipo.obra(custo.efetivo.obras, tipo.obra)
                }
                
                tipo.obra.selecionada <<- tipo.obra
                municipio.selecionado.tipo.obra <<- municipio
                
                mapa_paraiba_custo_efetivo <- get.mapa.paraiba.custo.efetivo(mapa_paraiba, municipios.tipo.obra.custo.efetivo)
                
                cores.custo.efetivo <- paleta.de.cores(dado = mapa_paraiba_custo_efetivo@data$custo.efetivo.log)
                
                municipios.custo.efetivo.top.3 <<- get.top.3.municipios.custo.efetivo(municipios.tipo.obra.custo.efetivo, municipios)
                
                leafletProxy("mapa_tipo_obra", data = mapa_paraiba_custo_efetivo) %>%
                    clearGroup( group = "municipios-poligono-custo-efetivo" ) %>%
                    clearMarkers() %>%
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
                                                 mapa_paraiba_custo_efetivo@data$largura.borda) %>%
                    addMarkers(lng = ~lon, lat = ~lat, icon = trofeu.icon, data = municipios.custo.efetivo.top.3)
            }
        }
    })
}

shinyApp(ui, server)
