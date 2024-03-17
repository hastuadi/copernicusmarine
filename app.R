library(shiny)
library(shinyFiles)
library(tidyverse)
library(leaflet)
library(terra)
library(data.table)
library(CopernicusMarine)

minlon <- 75; maxlon <- 153; minlat <- -20; maxlat <- 10
steps <- .5
set.dir <- getwd() %>% paste0('/') 

ui <- tabsetPanel(
  tabPanel(
    title = h4('Download Data'),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 3, wellPanel(
            dateRangeInput('rentang', h4('Date range'), start = '2015-01-01', end = '2015-02-28')
          ),
          wellPanel(
            h4('Area boundary'),
            
            numericInput('top', 'Top', value = 10, min = minlat+0.0083, max = maxlat, step = steps),
            numericInput('right', 'Right', value = 153, min = minlon+.0083, max = maxlon, step = steps),
            numericInput('bottom', 'Bottom', value = -20, min = minlat, max = maxlat-0.0083, step = steps),
            numericInput('left', 'Left', value = 75, min = minlon, max = maxlon-0.0083, step = steps),
            
            actionButton('set', 'Set Boundary')
          ),
          wellPanel(
            h4('Copernicus Marine login'),
            textInput('username', 'User name', width = '75%'),
            passwordInput('passw', 'Password', width = '75%'),
            
            downloadButton('download')
          ),
          actionButton('tmp', 'tmp')
        ),
        mainPanel(
          leafletOutput("mymap", height = '90vh', width = '110%')
        )))))

fungsi.poli <- \(x, y, session.input) {
  poli.tmp <- ext(c(x, y)) %>% 
    as.polygons(crs = '+proj=longlat')
  
  updateNumericInput(session.input, 'left', label = 'Left', value = x[1], min = minlon, max = maxlon-0.0083, step = steps)
  updateNumericInput(session.input, 'right', label = 'Right', value = x[2], min = minlon+.0083, max = maxlon, step = steps)
  updateNumericInput(session.input, 'bottom', label = 'Bottom', value = y[1], min = minlat, max = maxlat-0.0083, step = steps)
  updateNumericInput(session.input, 'top', label = 'Top', value = y[2], min = minlat+0.0083, max = maxlat, step = steps)
  
  leafletProxy('mymap') %>% 
    removeMarkerCluster('wilayah') %>% 
    addPolygons(data = poli.tmp, color = 'red', fill = NA, layerId = 'wilayah')
  
  return(poli.tmp)
}

server <- function(input, output, session) {
  
  klik <- reactiveVal(FALSE)
  lons <- reactiveVal(); lats <- reactiveVal()
  poli <- reactiveVal()
  
  observeEvent(input$set, {
    if(input$top <= input$bottom) {
      nilai <- input$top
      updateNumericInput(session, 'top', label = 'Top', value = input$bottom, min = minlat+0.0083, max = maxlat, step = steps)
      updateNumericInput(session, 'bottom', label = 'Bottom', value = nilai, min = minlat, max = maxlat-0.0083, step = steps)
    }
    if(input$right <= input$left) {
      nilai <- input$right
      updateNumericInput(session, 'right', label = 'Right', value = input$left, min = minlon+.0083, max = maxlon, step = steps)
      updateNumericInput(session, 'left', label = 'Left', value = nilai, min = minlon, max = maxlon-0.0083, step = steps)
    }
    lons({c(input$left, input$right)})
    lats({c(input$bottom, input$top)})
    poli({fungsi.poli(lons(), lats(), session)})
  })
  
  observeEvent(input$mymap_click, {
    mclick <- input$mymap_click
    if(mclick %>% is.null) return()
    if(!klik()) {
      lons({mclick$lng})
      lats({mclick$lat})
      klik({TRUE})
      
      leafletProxy('mymap') %>%
        addCircles(mclick$lng, mclick$lat, layerId = 'wilayah', radius = 5000, color = 'red')
      
    } else {
      klik(FALSE)
      lons({c(lons(), mclick$lng) %>% sort})
      lats({c(lats(), mclick$lat) %>% sort})
      
      tmp <- lons()
      tmp[tmp < minlon] <- minlon; tmp[tmp > maxlon] <- maxlon
      lons({tmp})
      
      tmp <- lats()
      tmp[tmp < minlat] <- minlat; tmp[tmp > maxlat] <- maxlat
      lats({tmp})
      
      poli({fungsi.poli(lons(), lats(), session)})
      
    }
  })
  
  observeEvent(input$tmp, {
    nama.file <- paste0(set.dir, 'www/',
                        'CHL_cmems_obs-oc_glo_multi-4km_', paste0(lons(), collapse = '_'), '_',
                        paste0(lats(), collapse = '_'), '_', paste0(input$rentang, collapse = '_'), '.nc'
    )
    
    runstring <- paste0('python ', set.dir, 'download_dataset_CHL.py ') %>%
      paste0(
        paste0(input$rentang, collapse = ' '), 
        ' ', nama.file, ' ', paste0(lons(), collapse = ' '), ' ', 
        paste0(lats(), collapse = ' '),
        ' ', input$username ,' ', input$passw
      )
    print(runstring)
  })
  
  output$download <- downloadHandler(
    filename = paste0(
      set.dir, 'www/', 'CHL_cmems_obs-oc_glo_multi-4km_', paste0(lons(), collapse = '_'), '_',
      paste0(lats(), collapse = '_'), '_', paste0(input$rentang, collapse = '_'), '.nc'
    ) %>% basename,
    content = function(f.in) {
      runstring <- paste0('python ', set.dir, 'download_dataset_CHL.py ') %>%
        paste0(
          paste0(input$rentang, collapse = ' '), 
          ' ', f.in, ' ', paste0(lons(), collapse = ' '), ' ', 
          paste0(lats(), collapse = ' '),
          ' ', input$username ,' ', input$passw
        )
      
      invisible(system(runstring))
      contentType = 'application/nc'
    })
  
  output$mymap <- renderLeaflet({
    leaflet(
      options = leafletOptions(attributionControl = FALSE, attribution = 'BRIN')
    ) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      setView(113.9213, -0.7893, zoom = 5)
  })
}

shinyApp(ui, server)
