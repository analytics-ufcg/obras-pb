source("shiny_server_georref.R")
source("shiny_server_custo_efetivo.R")

# Lógica do sistema
server <- function(input, output, session) {
    output$keepAlive <- renderText({
        req(input$count)
        paste("keep alive ", input$count)
    })
    
    server_georref(input, output, session)
    server_custo_efetivo(input, output, session)
}