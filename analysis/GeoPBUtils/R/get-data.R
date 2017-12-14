# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

get.data <- function(con1, con2) {
  obra <<- dbGetQuery(con1, "select * from t_obra")
  acompanhamento <<- dbGetQuery(con1, "select * from t_acompanhamento")
  complexo <<- dbGetQuery(con1, "select * from t_complexo")
  evolucao <<- dbGetQuery(con1, "select * from t_evolucao")

  jurisdicionado_db2 <<- dbGetQuery(con2, "select * from t_jurisdicionado")
  localidade <<- dbGetQuery(con2, "select * from t_localidade")

  obra.georref <- acompanhamento %>%
      left_join(obra, by = c("fk_obra" = "id")) %>%
      left_join(localidade, by = c("fk_localidade" = "id"))

  obra.filtrada <- obra.georref %>%
      filter(tipo_georeferenciamento != 0)

  obra.georref.corrigido <- obra.filtrada %>%
      rowwise() %>%
      mutate(
          valor_georeferenciamento = ifelse(tipo_georeferenciamento != 1 & tipo_georeferenciamento != 3,
                                            valor_georeferenciamento,
                                            centroide(valor_georeferenciamento))
      ) %>%
      filter(
          tipo_georeferenciamento != 2
      ) %>%
      separate(
          valor_georeferenciamento,
          c("lat", "lon"),
          sep = ",",
          remove = FALSE,
          convert = TRUE
      ) %>%
      filter(
          is.dentro.pb(lat,lon)
      )

  obra.georref.centroide.sumarizado <<- obra.georref.corrigido %>%
      group_by(fk_obra) %>%
      summarise(lat = mean(lat), lon = mean(lon)) %>%
      filter(!duplicated(fk_obra))
}

get.georreferencia.inputada <- function(obra, localidade, tipos.das.obras, municipios, obra.georref.centroide.sumarizado, ano.inicio) {
    obra %>%
        left_join(localidade, by = c("fk_localidade" = "id")) %>%
        left_join(evolucao, by = c("id" = "fk_obra")) %>%
        mutate(ano = lubridate::year(data_inicio_obra)) %>%
        filter(
            ano >= ano.inicio
        ) %>%
        left_join(tipos.das.obras, by = c("fk_tipo_obra" = "id")) %>%
        left_join(municipios, by = "codigo_ibge") %>%
        left_join(obra.georref.centroide.sumarizado %>% select(fk_obra,lat,lon), by = c("id" = "fk_obra")) %>%
        mutate(
            lat = ifelse(is.na(lat.y), lat.x, lat.y),
            lon = ifelse(is.na(lon.y), lon.x, lon.y),
            is.inputado = (is.na(lat.y) & is.na(lon.y))
        ) %>% select(-lat.x, -lat.y, -lon.x, -lon.y)
}

