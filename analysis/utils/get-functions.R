library(tidyverse)
library(RPostgreSQL)
library(rgdal)
library(rgeos)
library(sp)

#' @title is.dentro.pb
#' @description Retorna TRUE se uma coordenada está dentro da Paraíba FALSE caso contrario.
#' @param lat representa um valor de latitude.
#' @param lon representa um valor de longitude
#' @export
is.dentro.pb <- function(lat,lon) {
  lon.min <- -38.770132
  lat.min <- -8.316999
  lon.max <- -34.786624
  lat.max <- -5.995412
  return(lon.min < lon && lon < lon.max && lat.min < lat && lat < lat.max)
}

#' @title is.dentro.municipio
#' @description Retorna TRUE caso a coordenada dada esteja
#' dentro do município especificado, FALSE caso contrário.
#' @param lat representa um valor de latitude.
#' @param lon representa um valor de longitude
#' @param codigo_ibge código do IBGE que identifica o municipio.
#' @param mapa_paraiba ShapeFile do mapa da Paraíba.
#' @export
is.dentro.municipio <- function(lat, lon, codigo_ibge, mapa_paraiba) {
  mapa_municipio <- subset(mapa_paraiba, GEOCODIG_M == codigo_ibge)
  point <- data.frame(lat = lon, lon = lat)
  point_spatial <- SpatialPoints(point, proj4string = CRS(proj4string(mapa_paraiba)))
  return(rgeos::gContains(mapa_municipio, point_spatial))
}

#' @title coord_divide
#' @description Recebe um valor de latitude ou longitude e divide este por 10
#' até que o valor seja menor que o parâmetro especificado.
#' @param value Um valor de coordenada a ser diminuído.
#' @param parameter Valor que ira limitar a divisão da coordenada.
#' @export
coord_divide <- function(value, parameter) {
  return(
    ifelse(value < parameter,
           coord_divide(value / 10, parameter),
           value)
  )
}

#' @title corrige.coords
#' @description Recebe uma coordenada e as corrige caso seja
#' detectada alguma anomalia.
#' @param coord1 Um valor de latitude.
#' @param coord2 Um valor de longitude.
#' @export
corrige.coords <- function(coord1, coord2){
  coord1 <- dplyr::if_else(coord1 > 0, coord1 * -1, coord1)
  coord2 <- dplyr::if_else(coord2 > 0, coord2 * -1, coord2)

  lat <- max(coord1, coord2)
  lon <- min(coord1, coord2)

  lat <- coord_divide(lat, -10)
  lon <- coord_divide(lon, -100)

  return(c(lat = lat, lon = lon))
}

#' @title format.geo
#' @description Recebe um dataframe com dados de obras georreferenciadas
#' e corrige erros nas coordenadas caso necessário. Além disso verifica e
#' remove coordenadas que não representam pontos dentro da Paraíba.
#' @param mat dataframe com dados de obras georreferenciadas.
#' @export
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

#' @title centroide
#' @description Recebe, trata e processa uma string com uma ou várias localizações no formato de latitude e
#' longitude. Retorna um dataframe com as coordenadas processadas.
#' @param localizacao Uma string contendo uma ou mais localizações.
#' @export
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

#' @title get.mapa.paraiba
#' @description Retorna um ShapeFile do mapa da Paraíba já contendo dados sobre
#' as obras de cada município.
#' @param mapa_paraiba ShapeFile do mapa da Paraíba
#' @param dados.complementares Dataframe com dados sumarizados sobre as obras de cada município.
#' @param tipo.localidade Tipo de localidade selecionada, que pode ser municipio, microrregiao
#' ou mesorregiao.
#' @param localidade.selecionada Localidade selecionada de acordo com o tipo da localidade.
#' @export
get.mapa.paraiba <- function(mapa_paraiba, dados.complementares, tipo.localidade, localidade.selecionada) {
  mapa_paraiba_complementada <- mapa_paraiba

  mapa_paraiba_complementada@data <- mapa_paraiba_complementada@data %>%
    left_join(dados.complementares,
              by = c("GEOCODIG_M" = "codigo_ibge"))

  if (tipo.localidade == "microrregiao") {
    mapa_paraiba_complementada@data <- mapa_paraiba_complementada@data %>%
      mutate(
        cor.borda = if_else(Nome_Micro == localidade.selecionada, "blue", "black"),
        largura.borda = if_else(Nome_Micro == localidade.selecionada, 2, 1)
      )
  } else if (tipo.localidade == "mesorregiao") {
    mapa_paraiba_complementada@data <- mapa_paraiba_complementada@data %>%
      mutate(
        cor.borda = if_else(Nome_Meso == localidade.selecionada, "blue", "black"),
        largura.borda = if_else(Nome_Meso == localidade.selecionada, 2, 1)
      )
  } else {
    mapa_paraiba_complementada@data <- mapa_paraiba_complementada@data %>%
      mutate(
        cor.borda = if_else(is.na(cor.borda), "black", cor.borda),
        largura.borda = if_else(is.na(largura.borda), 1, largura.borda)
      )
  }

  mapa_paraiba_complementada
}

#' @title get.porc.municipios.georref
#' @description Analise um dataset com obras e retorna dados sumarizados
#' sobre a quantidade de obras georreferenciadas em cada município.
#' @param dado Dataset com as obras que se deseja analisar
#' @param localidade.desc Dataset que contém municípios, microrregiões e mesorregiões.
#' @param municipio.selecionado Município selecionado, parametro utilizado apenas pelo mapa,
#' o valor default é João Pessoa.
#' @param ano.inicial Ano inicial, apenas obras neste ano ou após ele serão analisadas.
#' @param ano.final Ano final, apenas obras neste ano ou antes dele serão analisadas.
#' @export
get.porc.municipios.georref <- function(dado, localidades.desc, municipio.selecionado = 'João Pessoa', ano.inicial = 0, ano.final = 3000) {
  dado %>%
    filter(ano >= ano.inicial,
           ano <= ano.final) %>%
    group_by(codigo_ibge, nome.x) %>%
    summarise(
      possui.georref.mas.tem.coordenadas.fora.municipio =
        sum(
          ifelse(!is.na(dentro_municipio) & dentro_municipio == FALSE, 1, 0)
        ),
      total.obras = n(),
      qtde.georref = sum(!is.inputado),
      porc.georref = (qtde.georref / total.obras) * 100
    ) %>%
    mutate(
      cor.borda = if_else(nome.x == municipio.selecionado, "blue", "black"),
      largura.borda = if_else(nome.x == municipio.selecionado, 5, 1)
    ) %>%
    left_join(localidades.desc, by = "codigo_ibge")
}

#' @title get.top.10.tipo.obra
#' @description Retorna um dataframe com o top 10 dos tipos de obras mais frequentes.
#' @param dado Dataframe com as obras que se deseja contar.
#' @export
get.top.10.tipo.obra <- function(dado) {
  dado %>%
    group_by(tipo_obra) %>%
    summarise(quantidade.tipo.obra = n()) %>%
    top_n(10, quantidade.tipo.obra) %>%
    arrange(tipo_obra) %>%
    pull(tipo_obra)
}

#' @title get.custos.efetivos
#' @description Dado um dataframe com dados de obras, seus respectivos tipos e valores, retorna
#' informações sumarizadas sobre o custo efetivo de cada tipo de obra.
#' @param dado Dataframe com dados de obras
#' @export
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

#' @title get.custo.efetivo.tipo.obra
#' @description Retorna um dataframe com informações sobre o custo efetivo
#' do tipo de obra especificado no intervalo de tempo especificado.
#' @param dado Dataframe com as obras a serem analisadas
#' @param municipio.selecionado Município selecionado no mapa
#' @param tipo.obra Tipo de obra a ser analisado. Valor default = PAVIMENTAÇÃO PARALEPÍPEDO.
#' @param ano.inicial Ano inicial do intervalo. Valor default = 0.
#' @param ano.final Ano final do intervalo. Valor default = 3000.
#' @export
get.custo.efetivo.tipo.obra <- function(dado, municipio.selecionado, tipo.obra = "PAVIMENTAÇÃO PARALEPÍPEDO", ano.inicial = 0, ano.final = 3000) {
  anos.tipo.obra <- dado %>% filter(tipo_obra == tipo.obra) %>% pull(ano)

  if (ano.inicial <= anos.tipo.obra %>% max() && ano.final >= anos.tipo.obra %>% min()) {
    dado <- dado %>%
      filter(
        ano >= ano.inicial,
        ano <= ano.final
        )
  }

  dado %>%
    filter(
      tipo_obra == tipo.obra
      ) %>%
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

#' @title get.popup.georref
#' @description Retorna uma string formatada com informações do município
#' para ser exibida no popup do mapa.
#' @param nome.munic Nome do município
#' @param total.obras Total de obras do município
#' @param qtde.georref Quantidade de obras georreferenciadas do município.
#' @param porc.georref Porcentagem de obras georreferenciadas do município.
#' @param qtde.coordenadas.fora.municipio Quantidade de obras do município
#' que possuem coordenadas fora do município.
#' @export
get.popup.georref <- function(nome.munic, total.obras, qtde.georref, porc.georref, qtde.coordenadas.fora.municipio) {
  paste0("Município: ",
         nome.munic,
         "</br>Total de obras: ",
         total.obras,
         "</br>Quantidade de obras georreferenciadas: ",
         qtde.georref,
         "</br>Obras georreferenciadas (%): ",
         round(porc.georref, 2), "%",
         "</br>Obras com coordenadas fora do município: ",
         qtde.coordenadas.fora.municipio)
}

#' @title paleta.de.cores
#' @description Retorna uma paleta de cores a ser utilizada no mapa.
#' @param paleta Nome da paleta a ser utilizada. Valor default = YlOrRd.
#' @param dado Dataframe
#' @param reverse Reverte a paleta de cores. Valor default = FALSE.
#' @export
paleta.de.cores <- function(paleta = "YlOrRd", dado, reverse = FALSE) {
  colors <- colorNumeric(paleta, domain = c(min(dado, na.rm = T), max(dado, na.rm = T)), reverse = reverse)
}

#' @title adiciona.poligonos.e.legenda
#' @description Adiciona polígonos e legenda ao mapa.
#' @param mapa ShapeFile do mapa da Paraíba.
#' @param cores Cores a serem utilizadas no mapa.
#' @param valor.municipio Valor do município.
#' @param tooltip String com o tooltip que será exibido no mapa.
#' @param janela Janela do popup.
#' @param titulo Titulo exibido no mapa.
#' @param tag_grupo Tag do grupo.
#' @param cod.localidades Código do município de acordo com o IBGE
#' @param localidade.selecionada Localidade selecionada no mapa
#' @param tipo.localidade Tipo da localidade selecionada, onde pode ser
#' municipio, microrregiao ou mesorregiao
#' @param localidades.desc Descrição das localidades, a qual contém
#' código do IBGE, nome, microrregião e mesorregião do município
#' @param cor.borda Cor da borda exibida no mapa.
#' @param largura.borda Largura da borda exibida no mapa.
#' @export
adiciona.poligonos.e.legenda <- function(mapa, cores, valor.municipio, tooltip, janela, titulo, tag_grupo, cod.localidades,
                                         localidade.selecionada, tipo.localidade, localidades.desc, cor.borda = "black", largura.borda = 1) {
  valores.legenda <- Filter(function(x) !is.na(x), valor.municipio) %>% unique()

  if(length(valores.legenda) == 1) {
    valores.legenda[2] <- valores.legenda[1] + 0.0000000001
  }
  valores.legenda[length(valores.legenda) + 1] <- NA

  bins <- ifelse(length(valores.legenda) <= 7, length(valores.legenda), 7)

  if (tipo.localidade != "municipio") {
    if (tipo.localidade == "microrregiao") {
      municipios.localidade <- localidades.desc %>% filter(microregiao == localidade.selecionada) %>% pull(codigo_ibge)
    } else {
      municipios.localidade <- localidades.desc %>% filter(mesoregiao == localidade.selecionada) %>% pull(codigo_ibge)
    }
    deve.ser.branco <- lapply(cod.localidades, function(l) !(l %in% municipios.localidade))
  } else {
    deve.ser.branco <- lapply(cod.localidades, function(l) FALSE)
  }

  addPolygons(mapa,
              opacity = 0.5,
              weight = largura.borda,
              fillColor = ifelse(deve.ser.branco, "white", cores(valor.municipio)),
              color = cor.borda,
              label = tooltip,
              popup = janela,
              fillOpacity = 1,
              group = tag_grupo) %>%
    addLegend(position = "bottomright", pal = cores, values = valores.legenda, bins = bins,
              title = titulo,
              opacity = 1)
}

#' @title cria.mapa
#' @description Cria um mapa leaflet com os parâmetros especificados.
#' @param dado Dataframe com dados das obras.
#' @param valor.municipio Valor do município.
#' @param tooltip Texto do tooltip exibido no mapa.
#' @param janela Janela do tooltip.
#' @param cores Cores a serem usadas no mapa.
#' @param titulo Titulo mostrado no mapa.
#' @param tag_grupo Tag do grupo.
#' #' @param cod.localidades Código do município de acordo com o IBGE
#' @param localidade.selecionada Localidade selecionada no mapa
#' @param tipo.localidade Tipo da localidade selecionada, onde pode ser
#' municipio, microrregiao ou mesorregiao
#' @param localidades.desc Descrição das localidades, a qual contém
#' código do IBGE, nome, microrregião e mesorregião do município
#' @param cor.borda Cor da borda.
#' @param largura.borda Largura da borda
#' @export
cria.mapa <- function(dado, valor.municipio, tooltip, janela, cores, titulo, tag_grupo, cod.localidades,
                      localidade.selecionada, tipo.localidade, localidades.desc, cor.borda = "black", largura.borda = 1) {
  dado %>%
    leaflet() %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    adiciona.poligonos.e.legenda(cores, valor.municipio, tooltip, janela, titulo, tag_grupo, cod.localidades,
                                 localidade.selecionada, tipo.localidade, localidades.desc, cor.borda, largura.borda)
}

#' @title dygraph.tipo.obra
#' @description Plota uma linha do tempo para que o usuário
#' seleciona o intervalo desejado na aplicação shiny.
#' @param dado Dataframe com dados das obras.
#' @param tipo.obra Tipo das obras analisadas.
#' @export
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

#' @title plot.ranking.georref
#' @description Plota um gráfico do ranking dos municípios com mais obras georreferenciadas.
#' @param dado Dataframe com dados das obras.
#' @param municipio Nome do município.
#' @param tipo.dado.representado Tipo do dado representado, o qual pode ser relativo ou absoluto.
#' @export
plot.ranking.georref <- function(dado, municipio, tipo.dado.representado) {
  municipio.selecionado <- dado %>% filter(nome.x == municipio)

  if (tipo.dado.representado == "relativo") {
    dado.var <- "porc.georref"
    legenda.eixo.y <- "Obras georreferenciadas (%)"
  } else {
    dado.var <- "qtde.georref"
    legenda.eixo.y <- "Obras georreferenciadas"
  }

  top.24.selecionado <- dado %>%
    arrange_(paste0("-", dado.var)) %>%
    head(24) %>%
    rbind(municipio.selecionado) %>%
    distinct() %>%
    mutate(class = ifelse(nome.x == municipio, "selecionado", "top 24"))

  plot <- top.24.selecionado %>%
    ggplot(aes_string(x = paste("reorder(nome.x, ", dado.var, ")"),
                      y = dado.var,
                      fill = dado.var)) +
    geom_bar(stat="identity") +
    guides(fill=FALSE, colour = FALSE) +
    labs(x = "Município",
         y = legenda.eixo.y) +
    scale_fill_distiller(palette = "YlOrRd") +
    coord_flip() +
    theme(legend.position="bottom")

  top.25 <- dado %>% arrange_(paste0("-", dado.var)) %>% head(25)

  if ((top.25 %>% filter(municipio == nome.x) %>% ungroup() %>% count()) == 0) {
    plot <- plot +
      labs(title = "Top 24 municípios que mais \ngeorreferenciam + selecionado") +
      facet_grid(class ~ ., scales = "free_y", space = "free_y")
  } else {
    plot <- plot +
      labs(title = "Top 25 municípios que mais \ngeorreferenciam")
  }

  if (nrow(municipio.selecionado) > 0) {
    plot <- plot +
      geom_text(
        data = filter(top.24.selecionado, municipio == nome.x),
        aes(label = "selecionado"),
        y = max(top.25 %>% pull(dado.var)) / 2
      )
  }
  plot +
    theme_bw()
}

#' @title plot.ranking.tipo.obra
#' @description Plota um gráfico do ranking dos tipos obras.
#' @param dado Dataframe com os dados das obras.
#' @param municipio Nome do município.
#' @export
plot.ranking.tipo.obra <- function(dado, municipio) {
  municipio.selecionado <- dado %>% filter(nome == municipio)

  top.24.selecionado <- dado %>%
    arrange(custo.efetivo) %>%
    head(24) %>%
    rbind(municipio.selecionado) %>%
    distinct() %>%
    mutate(class = ifelse(nome == municipio, "selecionado", "top 24"))

  plot <- top.24.selecionado %>%
    ggplot(aes(x = reorder(nome, -custo.efetivo),
               y = custo.efetivo,
               fill = custo.efetivo.log)) +
    geom_bar(stat="identity") +
    guides(fill=FALSE, colour = FALSE) +
    labs(x = "Município",
         y = "Custo efetivo por m2") +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) +
    coord_flip() +
    theme(legend.position="bottom")

  top.25 <- dado %>% arrange(custo.efetivo) %>% head(25)

  if ((top.25 %>% filter(municipio == nome) %>% ungroup() %>% count()) == 0) {
    plot <- plot +
      labs(title = "Top 24 municípios com menor \ncusto efetivo + selecionado") +
      facet_grid(class ~ ., scales = "free_y", space = "free_y")
  } else {
    plot <- plot +
      labs(title = "Top 25 municípios com menor \ncusto efetivo")
  }

  if (nrow(municipio.selecionado) > 0) {
    plot <- plot +
      geom_text(
        data = filter(top.24.selecionado, municipio == nome),
        aes(label = "selecionado"),
        y = max(top.25$custo.efetivo) / 2
      )
  }

  plot +
    theme_bw()
}

#' @title cidade.default
#' @description Obtém a cidade default.
#' @param dado Dataframe com os dados das obras.
#' @param nome Nome do município.
#' @export
cidade.default <- function(dado, nome) {
    dado %>%
        arrange_(nome) %>%
        head(1) %>%
        pull(nome)
}

#' @title add.borda
#' @description Adiciona colunas especificando parâmetros para as bordas
#' dos municipios exibidos no mapa.
#' @param dado Dataframe com dados dos municípios exibidos no mapa
#' @param municipio.selecionado Município selecionado no mapa
#' @param cor.destacada Cor da borda do município selecionado. Valor default = blue.
#' @param cor.default Cor da borda dos demais municípios. Valor default = black.
#' @param borda.destacada Largura da borda do município selecionado. Valor default = 5.
#' @param borda.default Largura da borda dos demais municípios. Valor default = 1.
#' @export
add.borda <- function(dado, municipio.selecionado, cor.destacada = "blue", cor.default = "black", borda.destacada = 5, borda.default = 1) {
    dado %>%
        mutate(
            cor.borda = if_else(nome == municipio.selecionado, cor.destacada, cor.default),
            largura.borda = if_else(nome == municipio.selecionado, borda.destacada, borda.default)
        )
}

