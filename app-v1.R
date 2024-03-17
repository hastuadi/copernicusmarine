#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(terra)
library(data.table)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('rentang', h4('Date range'), start = '2015-01-01', end = '2015-02-28'),
      br(),
      h4('Area boundary'),
      
      textInput('bottom', 'Bottom', ''),
      textInput('left', 'Left', ''),
      textInput('top', 'Top', ''),
      textInput('right', 'Right', ''),
      actionButton('set', 'Set Boundary'),
      br(), br(), tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
      ),
      actionButton('download', 'Download')
    ),
    mainPanel(
      leafletOutput("mymap", height = '95vh')
    )
  )
)

fungsi.poli <- \(x, y, session.input) {
  poli.tmp <- ext(c(x, y)) %>% 
    as.polygons(crs = '+proj=longlat')
  
  updateTextInput(session.input, 'left', label = 'Left', value = x[1])
  updateTextInput(session.input, 'right', label = 'Right', value = x[2])
  updateTextInput(session.input, 'bottom', label = 'Bottom', value = y[1])
  updateTextInput(session.input, 'top', label = 'Top', value = y[2])
  
  leafletProxy('mymap') %>% 
    addPolygons(data = poli.tmp, color = 'red', fill = NA, layerId = 'wilayah')
  
  return(poli.tmp)
}

server <- function(input, output, session) {
  klik <- reactiveVal(FALSE)
  lons <- reactiveVal(); lats <- reactiveVal()
  poli <- reactiveVal()
  
  observeEvent(input$mymap_click, {
    mclick <- input$mymap_click
    if(mclick %>% is.null) return()
    if(!klik()) {
      lons({mclick$lng})
      lats({mclick$lat})
      klik({TRUE})
    } else {
      klik(FALSE)
      lons({c(lons(), mclick$lng) %>% sort})
      lats({c(lats(), mclick$lat) %>% sort})
      
      poli({fungsi.poli(lons(), lats(), session)})
  
    }
  })
  
  observeEvent(input$set, {
    lons({c(input$left %>% as.numeric, input$right %>% as.numeric) %>% sort})
    lats({c(input$bottom %>% as.numeric, input$top %>% as.numeric) %>% sort})
    if(
      all(!is.na(c(lons(), lats())))
    ) {
      poli({fungsi.poli(lons(), lats(), session)})
    }

  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(noWrap = TRUE))
  })
}

shinyApp(ui, server)