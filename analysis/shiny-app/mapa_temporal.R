library(shiny)
library(shinydashboard)
library(RPostgreSQL)
library(tidyverse)
library(leaflet)
library(dygraphs)
library(rgdal)
source("../utils/get-functions.R")
#devtools::install_github("analytics-ufcg/geopbutils")
library(GeoPBUtils)
library(DT)

# Importa tabelas csv
tipos.das.obras <<- read.csv("../dados/tipos_obra.csv")
municipios.pb <- read.csv("../dados/municipios_pb.csv")
mapa_paraiba <<- readOGR("../dados/mapa-paraiba-ibge/Municipios.shp")

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
municipios <- data.frame(codigo_ibge = mapa_paraiba$GEOCODIG_M, 
                         lat = coordinates(mapa_paraiba)[,2], 
                         lon = coordinates(mapa_paraiba)[,1])

obras.2013 <<- get.georreferencia.inputada(obra, localidade, tipos.das.obras, municipios,
                                          obra.georref.centroide.sumarizado, 2013) %>% 
    filter(codigo_ibge != 0)

menor.ano <<- obras.2013 %>% pull(ano) %>% min()
maior.ano <<- obras.2013 %>% pull(ano) %>% max()

localidades.desc <<- localidade %>% 
    filter(uf == "Paraíba") %>% 
    select(codigo_ibge, codigo_microregiao, codigo_mesoregiao, nome, microregiao, mesoregiao)

custo.efetivo.obras <<- get.custos.efetivos(obras.2013)

tipo.obra.selecionada <<- get.top.10.tipo.obra(custo.efetivo.obras)[1]

localidades.georref <<- get.porc.municipios.georref(obras.2013, localidades.desc, 
                                                    cidade.default(obras.2013, "nome.x"))
localidades.custo.efetivo <<- get.custo.efetivo.tipo.obra(custo.efetivo.obras, 
                                                          cidade.default(custo.efetivo.obras, "nome"),
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

source("shiny_ui.R")
source("shiny_server.R")

shinyApp(ui, server)
