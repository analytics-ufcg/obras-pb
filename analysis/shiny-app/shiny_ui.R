# Interface do usuário
sidebar.width <- 240
ui <- dashboardPage(
    title = "Sagres obras",
    skin = "purple", 
    dashboardHeader( 
        title = span(
            img(src = "sagres-obras.png")
        ),
        titleWidth = sidebar.width
    ),
    dashboardSidebar( 
        sidebarMenu(
            menuItem("Obras georreferenciadas", tabName = "obras-georref", icon = icon("map-marker")),
            menuItem("Tipos de obras", icon = icon("building"), tabName = "tipos-obras")
        ),
        width = sidebar.width
    ),
    dashboardBody(
        tags$head(tags$style(HTML('.box-header {min-height: 35px;} .selectize-dropdown {z-index: 2000;}'))),
        tags$head(tags$link(rel="shortcut icon", href="tce-cropped.png")),
        tags$head(tags$link(rel="shortcut icon", href="sagres-obras.png")),
        tags$head(
            HTML("<script>
                 var socket_timeout_interval
                 var n = 0
                 $(document).on('shiny:connected', function(event) {
                 socket_timeout_interval = setInterval(function(){
                 Shiny.onInputChange('count', n++)
                 }, 15000)
                 });
                 $(document).on('shiny:disconnected', function(event) {
                 clearInterval(socket_timeout_interval)
                 });
                 </script>")
            ),
        tabItems(
            tabItem(
                tabName = "obras-georref",
                fluidRow(
                    height = "100vh",
                    column(width = 12,
                           box(width = NULL,
                               solidHeader = TRUE,
                               collapsible = FALSE,
                               h2("Painel I - Obras georreferenciadas"),
                               tags$p("Este painel fornece dados sumarizados e permite ter uma visão geral de quais gestões municipais mais georreferenciam suas obras. É possível ainda filtrar as obras por microrregião/mesorregião
                                      e pelo ano das obras, permitindo assim que sejam realizadas análises históricas sobre os municípios. O painel conta ainda com um ranking que filtra 
                                      as obras através dos filtros selecionados e mostra os 25 municípios que possuem mais obras georreferenciadas. Por fim, é possível também visualizar os dados de forma relativa ou absoluta.")
                               ),
                           box(width = NULL,
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               radioButtons("select_tipo_localidade_georref", "Tipo de localidade:",
                                            c("Município" = "municipio",
                                              "Microrregião" = "microrregiao",
                                              "Mesorregião" = "mesorregiao"),
                                            inline = TRUE
                               ),
                               radioButtons("select_tipo_representacao_georref", "Valor representado:",
                                            c("Relativo" = "relativo",
                                              "Absoluto" = "absoluto"),
                                            inline = TRUE
                               ),
                               selectInput("select_localidade_georref", label = h3("Selecione a localidade"),
                                           choices = c("Todos", localidades.georref$nome.x),
                                           selected = cidade.default(localidades.georref, "nome.x"))
                               
                           )
                               ),  
                    column(width = 12,
                           box(width = NULL,
                               collapsible = TRUE,
                               leafletOutput("mapa_georref", height = "50vh")
                           ),
                           box(width = NULL,
                               collapsible = TRUE,
                               dygraphOutput("dygraph_georref",  height = "25vh")
                           ),
                           box(width = "50vh",
                               collapsible = TRUE,
                               plotOutput("ranking_georref", height = "57vh")
                           )
                    )
                )
        ),
        tabItem(
            tabName = "tipos-obras",
            fluidRow(
                column(width = 12,
                       box(width = NULL,
                           solidHeader = TRUE,
                           collapsible = FALSE,
                           h2("Painel II - Custo efetivo das obras"),
                           tags$p("Este painel apresenta dados sobre o custo das obras por unidade de medida construída dos mais diversos tipos de obras realizadas no estado, 
                                  permitindo uma fácil análise e detecção de anomalias nos custos das obras. É possível filtrar as obras por microrregião/mesorregião
                                  e pelo ano das mesmas, permitindo assim que sejam realizadas análises históricas sobre os municípios. O painel conta ainda com um ranking que filtra 
                                  as obras através dos filtros selecionados e mostra os 25 municípios que possuem o melhor custo efetivo.
                                  Por fim, é possível também selecionar o tipo de obra que se deseja analisar.")
                           ),
                       box(width = NULL,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           radioButtons("select_tipo_localidade_tipo_obra", "Tipo de localidade:",
                                        c("Município" = "municipio",
                                          "Microrregião" = "microrregiao",
                                          "Mesorregião" = "mesorregiao"),
                                        inline = TRUE
                           ),
                           selectInput("select_tipo_obra", label = h3("Selecione o tipo da obra"),
                                       choices = get.top.10.tipo.obra(custo.efetivo.obras),
                                       selected = tipo.obra.selecionada),
                           selectInput("select_localidade_tipo_obra", label = h3("Selecione a localidade"),
                                       choices = localidades.custo.efetivo$nome, 
                                       selected = localidade.selecionada.tipo.obra)
                       ),
                       box(width = NULL,
                           collapsible = TRUE,
                           leafletOutput("mapa_tipo_obra", height = "50vh")
                       ),
                       box(width = NULL,
                           collapsible = TRUE,
                           dygraphOutput("dygraph_tipo_obra", height = "25vh")
                       )
                       ),
                column(width = 12,
                       box(width = NULL,
                           collapsible = TRUE,
                           plotOutput("ranking_tipo_obra", height = "51vh")
                           )
                       ),
                column(width = 12,
                       box(width = NULL,
                           collapsible = TRUE,
                           DT::dataTableOutput("obras_custo_efetivo")
                       )
                )
                )
            )
        )
    )
)