# global.R defines variables needed in global environment

## text message about ECHO facility search
## used by inputId 'ss_search_echo'
echo_url <-  'https://echo.epa.gov/facilities/facility-search' # used in server.R and in message below
echo_message <- shiny::HTML(paste0('To use the ECHO website to search for and specify a list of regulated facilities, 
                                    <br>1) go to ', '<a href=\"', echo_url, '\", target=\"_blank\">', echo_url,  '</a>', ' and <br>
                                    2) under Facility Characteristics Results View select data table, click Search, then <br>
                                    3) click Customize Columns, use checkboxes to include Latitude and Longitude, then <br>
                                    4) click Download Data, then <br>
                                    5) return to this app to upload that ECHO site list.<br>'))



## named vector of EPA programs to limit facilities to
## used by inputId 'ss_limit_fac1' and 'ss_limit_fac2'
epa_programs <- c(
  "TRIS" = "TRIS", "RCRAINFO" = "RCRAINFO",
  "AIRS/AFS" = "AIRS/AFS", "E-GGRT" = "E-GGRT",
  "NPDES" = "NPDES", "RCRAINFO" = "RCRAINFO", "RMP" = "RMP"
)

# Defaults for quantiles summary stats
# (probs of 0, 0.50, 1 are redundant since min, median, max are
# already separately shown)
## used by inputId 'an_list_pctiles'

#probs.default.choices <- c("0", "0.25", "0.50", "0.75", "0.80", 
#                           "0.90", "0.95", "0.99", "1.00")
probs.default.selected <- c(0.25, 0.75, 0.95)
probs.default.values <- c(0, 0.25, 0.5, 0.75, 0.8, 
                           0.9, 0.95, 0.99, 1)
probs.default.names <- formatC(probs.default.values, digits = 2, format='f', zero.print = '0')


# a default for cutoff in at/above threshold stat summarizing EJ US percentiles
## used by inputIds 'an_thresh_comp1' and 'an_thresh_comp2'
threshold.default <- c('comp1' = 95, 'comp2' = 95)  

# which fields to compare to thresholds 
# EJ US pctiles or EJ State pctiles
## used by inputIds 'an_fields_comp1' and 'an_fields_comp2'
threshgroup.default <- list(
  'comp1' = "EJ US pctiles",  'comp2' = "EJ State pctiles"
)

## function for making leaflet map of uploaded points
plot_facilities <- function(mypoints, rad = 4){
  
  ## map settings
  base_color      <- 'blue'
  cluster_color   <- 'red'
  highlight_color<- 'orange'
  circleweight <- 4
  
  # names(mypoints) <- gsub('lon','longitude', names(mypoints))
  # names(mypoints) <- gsub('lat','latitude', names(mypoints))
  
  names(mypoints) <- gsub('FacLat','latitude', names(mypoints))
  names(mypoints) <- gsub('FacLong','longitude', names(mypoints))
  
  
  if (length(mypoints) != 0) {
    isolate({ # do not redraw entire map and zoom out and reset location viewed just because radius changed?
      mymap <- leaflet(mypoints) %>% 
        addTiles()  %>%
        addCircles(
          #radius = input$radius * meters_per_mile,
          radius = rad,
          color = base_color, fillColor = base_color, 
          fill = TRUE, weight = circleweight, 
          #popup = popup_to_show()
        )
      #### %>% clearBounds() # clears the bound, so that the view will be automatically determined by the range of latitude/longitude data in the map layers if provided;
      mymap
    })
  } else {  # length(mypoints) == 0
    mymap <- leaflet() %>% 
      addTiles() %>% 
      setView(-110, 46, zoom = 3)
    mymap
  }
  ### Button to print map ####
  leaflet.extras2::addEasyprint(map = mymap, options = leaflet.extras2::easyprintOptions(exportOnly = TRUE, title='Save Map Snapshot'))
  
}

## code to generate quadtree dataset on app startup
localtree <- SearchTrees::createTree(
  EJAMblockdata::quaddata, treeType = "quad", dataType = "point"
)