# one piece of an answer to this StackOverflow question
#  http://stackoverflow.com/questions/31814037/integrating-time-series-graphs-and-leaflet-maps-using-r-shiny

# for this we'll use Kyle Walker's rpubs example
#   http://rpubs.com/walkerke/leaflet_choropleth
# combined with data from Diego Valle's crime in Mexico project
#   https://github.com/diegovalle/mxmortalitydb

# we'll also build on the shiny example included in dygraphs
#  https://github.com/rstudio/leaflet/blob/master/inst/examples/shiny.R

library(shiny)
library(leaflet)
library(dygraphs)
library(dplyr)
library(rgdal)

# let's build this in advance so we don't download the
#    data every time
tmp <- tempdir()
url <- "http://personal.tcu.edu/kylewalker/data/mexico.zip"
file <- basename(url)
download.file(url, file)
unzip(file, exdir = tmp)
mexico <- {
    #delete our files since no longer need
    on.exit({unlink(tmp);unlink(file)})  
    readOGR(dsn = tmp, layer = "mexico", encoding = "UTF-8")
}

# now let's get our time series data from Diego Valle
crime_mexico <- jsonlite::fromJSON(
    "https://rawgit.com/diegovalle/crimenmexico.diegovalle.net/master/assets/json/states.json"
)

# instead of the gdp data, let's use mean homicide_rate
#   for our choropleth
mexico$homicide <- crime_mexico$hd %>%
    group_by( state_code ) %>%
    summarise( homicide = mean(rate) ) %>%
    ungroup() %>%
    select( homicide ) %>%
    unlist


pal <- colorBin(
    palette = RColorBrewer::brewer.pal(n=9,"YlGn")[-(1:2)]
    , domain = c(0,50)
    , bins =7
)

popup <- paste0("<strong>Estado: </strong>", 
                mexico$name, 
                "<br><strong>Homicide Rate: </strong>", 
                round(mexico$homicide,2)
)

leaf_mexico <- leaflet(data = mexico) %>%
    addTiles() %>%
    addPolygons(fillColor = ~pal(homicide), 
                fillOpacity = 0.8, 
                color = "#BDBDC3", 
                weight = 1,
                layerId = ~id,
                popup = popup
    )


ui <- fluidPage(
    leafletOutput("map1"),
    dygraphOutput("dygraph1",height = 200),
    textOutput("message", container = h3)
)

server <- function(input, output, session) {
    v <- reactiveValues(msg = "")
    
    output$map1 <- renderLeaflet({
        leaf_mexico
    })
    
    output$dygraph1 <- renderDygraph({
        # start dygraph with all the states
        crime_wide <- reshape(
            crime_mexico$hd[,c("date","rate","state_code"),drop=F],
            v.names="rate",
            idvar = "date",
            timevar="state_code",
            direction="wide"
        )
        colnames(crime_wide) <- c("date",as.character(mexico$state))
        rownames(crime_wide) <- as.Date(crime_wide$date)
        dygraph( crime_wide[,-1])  %>%
            dyLegend( show = "never" )
    })
    
    observeEvent(input$dygraph1_date_window, {
        if(!is.null(input$dygraph1_date_window)){
            # get the new mean based on the range selected by dygraph
            mexico$filtered_rate <- crime_mexico$hd %>%
                filter( 
                    as.Date(date) >= as.Date(input$dygraph1_date_window[[1]]),
                    as.Date(date) <= as.Date(input$dygraph1_date_window[[2]])  
                ) %>%
                group_by( state_code ) %>%
                summarise( homicide = mean(rate) ) %>%
                ungroup() %>%
                select( homicide ) %>%
                unlist
            
            # leaflet comes with this nice feature leafletProxy
            #  to avoid rebuilding the whole map
            #  let's use it
            leafletProxy( "map1", data = mexico  ) %>%
                removeShape( layerId = ~id ) %>%
                addPolygons( fillColor = ~pal( filtered_rate ), 
                             fillOpacity = 0.8, 
                             color = "#BDBDC3", 
                             weight = 1,
                             layerId = ~id,
                             popup = paste0("<strong>Estado: </strong>", 
                                            mexico$name, 
                                            "<br><strong>Homicide Rate: </strong>", 
                                            round(mexico$filtered_rate,2)
                             )
                )
        }
    })
    
    observeEvent(input$map1_shape_click, {
        v$msg <- paste("Clicked shape", input$map1_shape_click$id)
        #  on our click let's update the dygraph to only show
        #    the time series for the clicked
        state_crime_data <- subset(crime_mexico$hd,state_code == input$map1_shape_click$id)
        rownames(state_crime_data) <- as.Date(state_crime_data$date)
        output$dygraph1 <- renderDygraph({
            dygraph(
                xts::as.xts(state_crime_data[,"rate",drop=F]),
                ylab = paste0(
                    "homicide rate ",
                    as.character(mexico$state[input$map1_shape_click$id])
                )
            )
        })
    })
    
}

shinyApp(ui, server)
