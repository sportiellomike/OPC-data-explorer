library(shiny)
library(DT)  # for interactive tables
library(shinythemes)  # for the slate theme
library(ggplot2)
library(viridis)
library(usmap)
library(dplyr)
library(sf)

# Load data
data <- readRDS("finalmerge-07Apr2025.rds")
us_states<-us_map(
  regions = c("states")
)
us_counties<-us_map(
  regions = c("counties")
)

# UI definition
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
            body {
                background-color: white;
                color: black;
            }
            .selectize-input, .selectize-dropdown {
                color: black;
                background-color: white;
                border: 1px solid #ddd;
            }
            .selectize-dropdown-content {
                background: white;
            }
            .selectize-dropdown-content .active {
                background: #FF7731;
                color: white;
            }
            label {
                color: black !important;
            }
            .dataTables_wrapper {
                color: black;
            }
            .dataTables_filter input {
                color: black;
                background-color: white;
                border: 1px solid #ddd;
            }
            .dataTables_length select {
                color: black;
                background-color: white;
                border: 1px solid #ddd;
            }
            .well {
                background-color: #f8f9fa;
                border: 1px solid #ddd;
            }
            .nav-tabs > li.active > a {
                background-color: #FF7731 !important;
                color: white !important;
            }
            .nav-tabs > li > a {
                color: black !important;
            }
            .btn-default {
                background-color: #FF7731;
                color: white;
                border: none;
            }
            .btn-default:hover {
                background-color: #FF8C00;
                color: white;
            }
        "))
    ),
    titlePanel("OPHC Data Explorer"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("state", 
                      "Select State:",
                      choices = sort(unique(data$STATE_NAME)),
                      selected = "Alabama"),
            
            selectInput("county", 
                      "Select County:",
                      choices = NULL),  # will be updated based on state selection
            
            selectInput("year",
                      "Select Year:",
                      choices = sort(unique(data$Year)),
                      selected = max(data$Year)),
            
            selectInput("freqinjections",
                      "Select Injection Frequency:",
                      choices = sort(unique(data$freqinjections)),
                      selected = 0.01),
            
            selectInput("plotColumn",
                      "Select Column for Plot Color:",
                      choices = c("cumulativelivessaved", "deathsper100k",
                                "injectionsatophc", "pwid", 
                                "totalinjectionseverywhere", 
                              
                                'cumulativeHIVinfectionlowaverted',
                                'cumulativeHIVinfectionmedaverted',
                                'cumulativeHIVinfectionhighaverted',
                                'cumulativeHCVinfectionaverted',
                                'totalsavings',
                                'valueoflivessaved',
                                'infectiousdiscountedcost'
                                  
                                
                                
                                
                                
                               ),
                      selected = "cumulativelivessaved"),
            
            selectInput("transformation",
                      "Select Transformation:",
                      choices = c("No transformation", "log2(1+value)", "log10(1+value)"),
                      selected = "log2(1+value)"),
            
            textInput("legendLabel",
                     "Legend Label:",
                     value = "cumulativelivessaved"),
            
            numericInput("legendTextSize",
                      "Legend Text Size:",
                      value = 12,
                      min = 6,
                      max = 24),
            
            textInput("legendMin",
                     "Legend Minimum:",
                     value = ""),
            
            textInput("legendMax",
                     "Legend Maximum:",
                     value = "")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Data", 
                    DTOutput("filtered_data")
                ),
                tabPanel("Plot",
                    plotOutput("plot1"),
                    plotOutput("plot2")
                )
            )
        )
    )
)

# Server logic
server <- function(input, output, session) {
    # Update legend label when plot column changes
    observeEvent(input$plotColumn, {
        updateTextInput(session, "legendLabel", value = input$plotColumn)
    })
    
    # Update county choices based on selected state
    observeEvent(input$state, {
        counties <- sort(unique(data$COUNTYNAME[data$STATE_NAME == input$state]))
        updateSelectInput(session, "county",
                        choices = counties,
                        selected = NULL)
    })
    
    # Create filtered dataset
    filtered_data <- reactive({
        req(input$state, input$freqinjections, input$year)  # ensure state, frequency, and year are selected
        
        filtered <- data
        
        # Filter by state
        filtered <- filtered[filtered$STATE_NAME == input$state,]
        
        # Filter by county if selected
        if (!is.null(input$county)) {
            filtered <- filtered[filtered$COUNTYNAME == input$county,]
        }
        
        # Filter by injection frequency and year
        filtered <- filtered[filtered$freqinjections == input$freqinjections & filtered$Year == input$year,]
        
        filtered
    })
    
    # Render the filtered data table
    output$filtered_data <- renderDT({
        datatable(filtered_data(),
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().container()).css({'background-color': '#ffffff', 'color': '#000'});",
                        "}"
                    )
                  ),
                  rownames = FALSE,
                  style = "bootstrap4") %>%
        formatRound(columns = c("freqinjections", "deathsper100k", "Population", 
                              "injectionsatophc", "pwid", "injectionsperperson",
                              "totalinjectionseverywhere", "pop18plus"), digits = 0)
    })
    
    # Render blank plot
    output$plot1 <- renderPlot({
datasubsetted<-subset(data, data$freqinjections == input$freqinjections & data$Year == input$year)

  #       counties_with_merge<-us_counties |>
  # left_join(datasubsetted$fips, by = join_by(fips))
counties_with_merge<-merge(us_counties,
                           datasubsetted,
                            by = 'fips',
                            all.x = T)
counties_with_merge$plotValue <- counties_with_merge[[input$plotColumn]]
# Get x-axis limits of the bounding box for the state data
xlim_current <- st_bbox(us_states)$xlim
# Add 540ish km (or 10% of the US) to the bounds (thus shifting the window over)
xlim_expanded <- c(
  xlim_current[1] + (0.1 * diff(xlim_current)),
  xlim_current[2] + (0.1 * diff(xlim_current))
)

interior_state_borders <- st_intersection(us_states) |>
  filter(n.overlaps > 1) |>
  # Remove weird points that st_intersection() adds
  filter(!(st_geometry_type(geom) %in% c("POINT", "MULTIPOINT")))
ggplot() +
  # Add counties filled with unemployment levels
  geom_sf(
    data = counties_with_merge, aes(fill = plotValue), linewidth = 0
  ) +
  # Add interior state boundaries
  geom_sf(
    data = interior_state_borders, color = "white", linewidth = 0.25
  ) +
  scale_fill_viridis(option = "plasma", 
                     name = input$legendLabel,
                     limits = if (!is.null(input$legendMin) && !is.null(input$legendMax) && 
                                input$legendMin != "" && input$legendMax != "") {
                       c(as.numeric(input$legendMin), as.numeric(input$legendMax))
                     } else {
                       NULL
                     },
                     trans = switch(input$transformation,
                                  "log2(1+value)" = scales::trans_new(
                                    name = "log2_plus1",
                                    transform = function(x) log2(1 + x),
                                    inverse = function(x) 2^x - 1
                                  ),
                                  "log10(1+value)" = scales::trans_new(
                                    name = "log10_plus1",
                                    transform = function(x) log10(1 + x),
                                    inverse = function(x) 10^x - 1
                                  ),
                                  "No transformation" = "identity")) +
   coord_sf(crs = st_crs("ESRI:102003"), xlim = xlim_expanded) +
  theme(
    panel.background =  element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.86, 0.32),
    legend.direction = "vertical",
    legend.key.height = unit(0.45, "inches"),
    legend.text = element_text(size = input$legendTextSize),
    legend.title = element_text(size = input$legendTextSize)
  )







    })
    output$plot2 <- renderPlot({
      datasubsetted<-subset(data, data$freqinjections == input$freqinjections & data$Year == input$year)
      #       counties_with_merge<-us_counties |>
      # left_join(datasubsetted$fips, by = join_by(fips))
      counties_with_merge<-merge(us_counties,
                                 datasubsetted,
                                 by = 'fips',
                                 all.x = T)
      counties_with_merge$plotValue <- counties_with_merge[[input$plotColumn]]
      counties_with_merge<-subset(counties_with_merge,counties_with_merge$STATE_NAME==input$state)
      # Get x-axis limits of the bounding box for the state data
      xlim_current <- st_bbox(us_states)$xlim
      us_states<-subset(us_states,us_states$full == input$state)
      # Add 540ish km (or 10% of the US) to the bounds (thus shifting the window over)
      xlim_expanded <- c(
        xlim_current[1] + (0.1 * diff(xlim_current)),
        xlim_current[2] + (0.1 * diff(xlim_current))
      )
      
      
      ylim_current <- st_bbox(us_states)$ylim
      ylim_expanded <- c(
        ylim_current[1] + (0.2 * diff(ylim_current)),
        ylim_current[2] + (0.2 * diff(ylim_current))
      )
      
      
      
      
      interior_state_borders <- st_intersection(us_states) |>
        filter(n.overlaps > 1) |>
        # Remove weird points that st_intersection() adds
        filter(!(st_geometry_type(geom) %in% c("POINT", "MULTIPOINT")))
      ggplot() +
        # Add counties filled with unemployment levels
        geom_sf(
          data = counties_with_merge, aes(fill = plotValue), linewidth = 0
        ) +
        # Add interior state boundaries
        geom_sf(
          data = interior_state_borders, color = "white", linewidth = 0.25
        ) +
        scale_fill_viridis(option = "plasma", 
                          name = input$legendLabel,
                          limits = if (!is.null(input$legendMin) && !is.null(input$legendMax) && 
                                     input$legendMin != "" && input$legendMax != "") {
                            c(as.numeric(input$legendMin), as.numeric(input$legendMax))
                          } else {
                            NULL
                          },
                          trans = switch(input$transformation,
                                       "log2(1+value)" = scales::trans_new(
                                         name = "log2_plus1",
                                         transform = function(x) log2(1 + x),
                                         inverse = function(x) 2^x - 1
                                       ),
                                       "log10(1+value)" = scales::trans_new(
                                         name = "log10_plus1",
                                         transform = function(x) log10(1 + x),
                                         inverse = function(x) 10^x - 1
                                       ),
                                       "No transformation" = "identity")) +
        # coord_sf(crs = st_crs("ESRI:102003"), xlim = xlim_expanded,ylim = ylim_expanded) +
        theme(
          panel.background =  element_blank(),
          # legend.position = "inside",
          # legend.position.inside = c(0.86, 0.32),
          legend.direction = "vertical",
          legend.key.height = unit(0.45, "inches"),
          legend.text = element_text(size = input$legendTextSize),
          legend.title = element_text(size = input$legendTextSize)
        )
      
      
      
      
      
      
      
    })
    
    
    
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
