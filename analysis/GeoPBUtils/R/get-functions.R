is.dentro.pb <- function(lat,lon) {
  lon.min <- -38.770132
  lat.min <- -8.316999
  lon.max <- -34.786624
  lat.max <- -5.995412
  return(lon.min < lon && lon < lon.max && lat.min < lat && lat < lat.max)
}

coord_divide <- function(value, parameter) {
  return(
    ifelse(value < parameter,
           coord_divide(value / 10, parameter),
           value)
  )
}

corrige.coords <- function(coord1, coord2){
  coord1 <- dplyr::if_else(coord1 > 0, coord1 * -1, coord1)
  coord2 <- dplyr::if_else(coord2 > 0, coord2 * -1, coord2)

  lat <- max(coord1, coord2)
  lon <- min(coord1, coord2)

  lat <- coord_divide(lat, -10)
  lon <- coord_divide(lon, -100)

  return(c(lat = lat, lon = lon))
}

format.geo <- function(mat) {
  mat.pb <- data.frame(mat) %>%
    rowwise() %>%
    mutate(
      lat = corrige.coords(X1, X2)["lat"],
      lon = corrige.coords(X1, X2)["lon"]
    ) %>%
    rowwise() %>%
    filter(is.dentro.pb(lat, lon))

  coord1 <- mean(mat.pb$X1)
  coord2 <- mean(mat.pb$X2)

  coords <- corrige.coords(coord1, coord2)

  lat <- coords[1]
  lon <- coords[2]

  paste(lat,lon, sep = ",")
}

centroide <- function(localizacao) {

  coords <- tryCatch(
    {
      parsed.mat <- matrix(jsonlite::fromJSON(localizacao), ncol = 2)

      return(format.geo(parsed.mat))
    },
    error=function(cond) {
      tryCatch(
        {
          lista.loc.format <- strsplit(localizacao, "\\],")[[1]]
          lista.loc.format <- str_replace_all(lista.loc.format, "\\[|\\]", "")
          lista.coord <- unlist(strsplit(lista.loc.format, ","))
          lista.coord <- trimws(lista.coord, which = "both")
          lista.coord.format <- lapply(lista.coord, function(d){
            as.numeric(unlist(strsplit(d, " "))[1])
          })
          parsed.matrix <- matrix(unlist(lista.coord.format), ncol = 2, byrow = TRUE)

          return(format.geo(parsed.matrix))
        },
        error = function(cond) {
          return(localizacao)
        }
      )
      return(localizacao)
    }
  )
  coords
}

paleta.de.cores <- function(paleta = "YlOrRd", dado) {
  colors <- colorNumeric(paleta, domain = c(min(dado, na.rm = T), max(dado, na.rm = T)))
}

adiciona.poligonos.e.legenda <- function(mapa, cores, valor.municipio, tooltip, janela, titulo) {
    addPolygons(mapa,
                opacity = 0.5,
                weight = 1,
                fillColor = cores(valor.municipio),
                color = "black",
                label = tooltip,
                popup = janela,
                fillOpacity = 1,
                layerId = ~GEOCODIG_M) %>%
    addLegend(position = "bottomright", pal = cores, values = valor.municipio,
              title = titulo,
              opacity = 1)
}

cria.mapa <- function(dado, valor.municipio, tooltip, janela, cores, titulo) {
  dado %>%
    leaflet() %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    adiciona.poligonos.e.legenda(cores, valor.municipio, tooltip, janela, titulo)
}
