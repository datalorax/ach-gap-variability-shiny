library(shiny)
library(tidyverse)
library(leaflet)
library(here)
library(feather)

gaps <- read_feather(here("data", "achievement-gaps-geocoded.feather"))

# compute weighted mean
gaps <- gaps %>% 
    group_by(state, district_id, school_id, ncessch, city, cnty, nmcnty, 
             lat, lon, content) %>% 
    summarize(v_hisp = weighted.mean(v_hisp, n, na.rm = TRUE),
              v_econ = weighted.mean(v_econ, n, na.rm = TRUE)) %>%
    ungroup()

map_start_loc <- gaps %>% 
    filter(city == "Oakland") %>% 
    summarize(mean_long = mean(lon, na.rm = TRUE),
              mean_lat = mean(lat, na.rm = TRUE))

pal <- colorNumeric(palette = "RdBu",
                    domain = seq(-1.5, 1.5, 0.01))

pal_rev <- colorNumeric(palette = "RdBu",
                        domain = seq(-1.5, 1.5, 0.01),
                        reverse = TRUE)

hisp_99 <- filter(gaps, v_hisp > -1.5, v_hisp < 0.5) %>% 
    drop_na(v_hisp) %>% 
    mutate(gap = v_hisp,
           label = paste(
               "NCES School ID: <a href =",
               "'https://nces.ed.gov/ccd/schoolsearch/school_list.asp?Search=1&InstName=&SchoolID=", 
               ncessch, "'>", ncessch, "</a>", "<br/>",
               "Estimated Gap:", round(v_hisp, 2)))

econ_99 <- filter(gaps, v_econ > -1.5, v_econ < 0.5) %>% 
    drop_na(v_econ) %>% 
    mutate(gap = v_econ,
           label = paste(
               "NCES School ID: <a href =",
               "'https://nces.ed.gov/ccd/schoolsearch/school_list.asp?Search=1&InstName=&SchoolID=", 
               ncessch, "'>", ncessch, "</a>", "<br/>",
               "Estimated Gap:", round(v_econ, 2)))

no_hisp <- gaps$ncessch[is.na(gaps$v_hisp)]
no_econ <- gaps$ncessch[is.na(gaps$v_econ)]

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel(title = "Geographical Variance in Achievement Gaps"),
    h5("Click on a point (school) to get a link to the corresponding NCES record."),
    div(class="outer",
        tags$style(type = "text/css", 
                   ".outer { position: fixed; 
                             top: 90px; 
                             left: 0; 
                             right: 0; 
                             bottom: 0; 
                             overflow: hidden; 
                             padding: 0
                            }",
                   "#controls { background-color: white;
                                padding: 0 10px 10px 10px;
                                cursor: move;
                                opacity: 0.5;
                                zoom: 1.0;
                                transition: opacity 500ms 1ms;
                              }",
                   "#controls:hover { opacity: 0.95;
                                      transition-delay: 0;
                                    }"),
        leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(id = "controls", 
                  bottom = 10, 
                  left = 10, 
                  draggable = FALSE, 
                  fixed = TRUE, 
                  class = "panel panel-default", 
                  
                  h2("Select Map Inputs"),
                  radioButtons("content",
                               "Select Content Area",
                               c("English/Language Arts" = "ELA",
                                 "Mathematics" = "Math"),
                               selected = "ELA",
                               width = ),
                  radioButtons("data",
                               "Select Achievement Gap",
                               c("Hispanic/White" = "hisp_99",
                                 "Economically Disadvantage/All Students" = "econ_99"),
                               selected = "hisp_99")
          )
    )
)

server <- function(input, output) {
    
    filtered_data <- reactive({
        filter(get(input$data), 
               content == input$content)
    })
        
    output$map <- renderLeaflet(
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            addLegend("bottomright",
                      values = seq(-0.5, 1.5, 0.25), # Restrict range
                      pal = pal_rev,                 # Use reversed color palette
                      labFormat = labelFormat(       #
                          transform = function(x) x*-1), # Multiply by -1
                      title = "Achievement Gap<br/>Effect Size",
                      opacity = 0.7) %>% 
            setView(map_start_loc$mean_long, map_start_loc$mean_lat, zoom = 10)
    )
    observe({
        d <- filtered_data()
        
        p <- leafletProxy("map") %>% 
            addCircleMarkers(data = d,
                             layerId = ~ncessch,
                             lng = ~lon,
                             lat = ~lat,
                             color = ~pal(gap),
                             stroke = FALSE,
                             radius = 5,
                             fillOpacity = 0.8,
                             label = ~lapply(label, htmltools::HTML),
                             popup = ~lapply(label, htmltools::HTML),
                             labelOptions = labelOptions(
                                 textOnly = FALSE,
                                 style = list(
                                     'background'='rgba(255,255,255,0.95)',
                                     'border-color' = 'rgba(95, 106, 106, .8)',
                                     'border-radius' = '2px',
                                     'border-style' = 'solid',
                                     'border-width' = '2px')))
        if(input$data == "hisp_99") {
            p <- p %>% 
                removeMarker(no_hisp)
                
        }
        if(input$data == "econ_99") {
            p <- p %>% 
                removeMarker(no_econ)
        }
      p
    })
}

shinyApp(ui = ui, server = server)
