library(shiny)
library(DT)  # for interactive tables
library(shinythemes)  # for the slate theme
library(ggplot2)
library(viridis)
library(usmap)
library(dplyr)
library(sf)
library(writexl)  # for Excel file export

# Load data
data <- readRDS("finalmerge-09Apr2025.rds")
# data2 <- readRDS("finalmerge-09Apr2025.rds")

# us_states<-us_map(
#     regions = c("states")
# )
# us_counties<-us_map(
#     regions = c("counties")
# )

# saveRDS(us_states, "us_states.rds")
# saveRDS(us_counties, "us_counties.rds")
us_states<-readRDS("us_states.rds")
us_counties<-readRDS("us_counties.rds")

# Define column names and their labels
national_result_cols <- c(
    "cumulativelivessaved",
    "cumulativeHCVinfectionaverted",
    "cumulativeHIVinfectionlowaverted",
    "cumulativeHIVinfectionmedaverted",
    "cumulativeHIVinfectionhighaverted",
    "infectiousdiscountedcost",
    "valueoflivessaved",
    "cumulativedeaths"
)

result_cols <- c(
    "cumulativelivessaved",
    "cumulativeHIVinfectionmedaverted",
    "cumulativeHCVinfectionaverted",
    "totalsavings",
    
    "Provisional.Drug.Overdose.Deaths",
    "deathsper100k",
    "injectionsatophc",
    "pwid",
    
    "Population",
    "livessaved",
    
    "cumulativeHIVinfectionlow",
    "cumulativeHIVinfectionmed",
    "cumulativeHIVinfectionhigh",
    "cumulativeHCVinfection",
    
    "HIVavertedhighrisk",
    "HIVavertedmedrisk",
    "HIVavertedlowrisk",
    "newHCVrisk",
    "HCVavertedrisk",
    "cumulativeHIVinfectionlowaverted",
    "cumulativeHIVinfectionhighaverted",
    
    "HIVdiscountedcostsavingshigh",
    "HIVdiscountedcostsavingsmed",
    "HIVdiscountedcostsavingslow",
    
    "HCVdiscountedcost",
    "infectiousdiscountedcost",
    "valueoflivessaved"
)



# UI definition
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
            body {
                background-color: white;
                color: black;
            }
            .selectize-input, .selectize-dropdown, input[type='text'], input[type='number'] {
                color: black;
                background-color: white;
                border: 1px solid #ddd;
                padding: 10px;
                font-size: 16px;
                border-radius: 6px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.05);
                transition: all 0.3s ease;
            }
            .selectize-input:hover, input[type='text']:hover, input[type='number']:hover {
                border-color: #FF7731;
                box-shadow: 0 4px 8px rgba(0,0,0,0.1);
            }
            .selectize-input.focus, input[type='text']:focus, input[type='number']:focus {
                border-color: #FF7731;
                box-shadow: 0 0 0 2px rgba(255,119,49,0.2);
                outline: none;
            }
            .selectize-dropdown {
                margin-top: 4px;
                border-radius: 6px;
                box-shadow: 0 4px 8px rgba(0,0,0,0.1);
            }
            .selectize-dropdown-content {
                background: white;
                padding: 5px;
            }
            .selectize-dropdown-content .active {
                background: #FF7731;
                color: white;
                border-radius: 4px;
            }
            label {
                color: black !important;
                font-size: 16px;
                font-weight: 500;
                margin-bottom: 8px;
                display: block;
            }
            .dataTables_wrapper {
                color: black;
            }
            .dataTables_filter input {
                color: black;
                background-color: white;
                border: 1px solid #ddd;
                padding: 8px;
                border-radius: 6px;
            }
            .dataTables_length select {
                color: black;
                background-color: white;
                border: 1px solid #ddd;
                padding: 8px;
                border-radius: 6px;
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
                padding: 10px 20px;
                font-size: 16px;
                border-radius: 6px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                transition: all 0.3s ease;
                margin: 5px;
            }
            .btn-default:hover {
                background-color: #FF8C00;
                color: white;
                transform: translateY(-1px);
                box-shadow: 0 4px 8px rgba(0,0,0,0.2);
            }
            .btn-default:active {
                transform: translateY(1px);
                box-shadow: 0 1px 2px rgba(0,0,0,0.1);
            }
        "))
    ),
    titlePanel("OPHC Data Explorer"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("state", 
                      "Select State:",
                      choices = sort(unique(data$STATE_NAME)),
                      selected = "New York"),
            
            selectInput("county", 
                      "Select County:",
                      choices = NULL),  # will be updated based on state selection
            
            selectInput("year",
                      "Select Year:",
                      choices = sort(unique(data$Year)),
                      selected = max(data$Year)),
            
            selectInput("percentinjections",
                      "Select Injection Percentage:",
                      choices = sort(unique(data$percentinjections)),
                      selected = 1),
            
            selectInput("plotColumn",
                      "Select Column for Plot Color:",
                      choices = c("cumulativelivessaved", 
                                  "deathsper100k",
                                "injectionsatophc", 
                                "pwid", 
                                "totalinjectionseverywhere", 
                                # 'cumulativelivessavedper100k',
                                'Population',
                                'pop18plus',
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
                tabPanel("Plots",
                         h3("Plots for subset of data selected on the left", 
                            style = "text-align: center; margin-bottom: 30px; color: #000000;"),
                    plotOutput("plot2"),
                    plotOutput("plot1"),
                    h5('Right click the state plot and/or national plot once you have selected the subset of data on the left.'),
                       h5('Though the data for Connecticut and some Alaska counties are factored into the results, they may not be plotted given the differences in mapping boundary names and how the CDC reported the data.') ,
                       h5('Make sure to transform the axis to optimize coloring of the plot and make the legend legible with text size and color bar free text'),
                    h4("Legend Key", style = "text-align: center; margin-bottom: 10px; color: #000000;"),
                       h5("cumulativelivessaved: Deaths averted due to OPHC"), 
                          h5("deathsper100k: Deaths per 100,000 people"),
                             h5("injectionsatophc: Number of injections at OPHC"), 
                                h5("pwid: Number of people who inject drugs"), 
                                   h5("totalinjectionseverywhere: Number of injections in and out of the OPHC"), 
                       
                                      h5('cumulativeHIVinfectionlowaverted: Cumulative HIV infections averted assuming low HIV incidence model'),
                                         h5('cumulativeHIVinfectionmedaverted: Cumulative HIV infections averted assuming medium HIV incidence model'),
                                            h5('cumulativeHIVinfectionhighaverted: Cumulative HIV infections averted assuming high HIV incidence model'),
                                               h5('cumulativeHCVinfectionaverted: Cumulative Hepatitis C Virus infections averted'),
                                                  h5("totalsavings: Total monetary savings from OPHC including both discounted costs from infections that did not occur and the 'statistical value' of life"),
                                                     h5('valueoflivessaved: Statistical value of deaths averted'),
                                                        h5('infectiousdiscountedcost: Costs saved from infections that did not occur due to OPHC')
                ),
                tabPanel("Local Results",
                    h3("Results for subset of data selected on the left", 
                       style = "text-align: center; margin-bottom: 30px; color: #000000;"),
                    fluidRow(
                        style = "padding: 20px;",
                        lapply(result_cols, function(col) {
                            label <- switch(col,
                                "cumulativelivessaved" = "Cumulative Lives Saved",
                             "cumulativeHIVinfectionmedaverted" = "Cumulative HIV Infection Med Averted",
                              "cumulativeHCVinfectionaverted" = "Cumulative HCV Infection Averted",
                            "totalsavings" = "Total Cumulative Savings",

                                "Provisional.Drug.Overdose.Deaths" = "Provisional Drug Overdose Deaths",
                                "deathsper100k" = "Deaths per 100k",
                                "injectionsatophc" = "Injections at OPHC",
                                "pwid" = "PWID",

                                
                                "Population" = "Population",
                                "livessaved" = "Lives Saved",
                                
                                "cumulativeHIVinfectionlow" = "Cumulative HIV Infection (Low)",
                                "cumulativeHIVinfectionmed" = "Cumulative HIV Infection (Med)",
                                "cumulativeHIVinfectionhigh" = "Cumulative HIV Infection (High)",
                                "cumulativeHCVinfection" = "Cumulative HCV Infection",

                                "HIVavertedhighrisk" = "HIV Averted High Risk",
                                "HIVavertedmedrisk" = "HIV Averted Med Risk",
                                "HIVavertedlowrisk" = "HIV Averted Low Risk",
                                "newHCVrisk" = "New HCV Risk",
                                "HCVavertedrisk" = "HCV Averted Risk",
                                "cumulativeHIVinfectionlowaverted" = "Cumulative HIV Infection Low Averted",
                                "cumulativeHIVinfectionhighaverted" = "Cumulative HIV Infection High Averted",

                                "HIVdiscountedcostsavingshigh" = "HIV Discounted Cost Savings (High)",
                                "HIVdiscountedcostsavingsmed" = "HIV Discounted Cost Savings (Med)",
                                "HIVdiscountedcostsavingslow" = "HIV Discounted Cost Savings (Low)",

                                "HCVdiscountedcost" = "HCV Discounted Cost",
                                "infectiousdiscountedcost" = "Infectious Discounted Cost",
                                "valueoflivessaved" = "Value of Lives Saved",
                                "Provisional.Drug.Overdose.Deaths" = "Provisional Drug Overdose Deaths"
                                
                            )
                            column(
                                width = 4,
                                div(
                                    style = "background-color: #f8f9fa; padding: 15px; margin-bottom: 20px; border-radius: 5px; text-align: center;",
                                    h4(label, style = "margin-bottom: 15px; color: #FF7731;"),
                                    textOutput(paste0("result_", gsub("\\.", "_", col))),
                                    style = "font-size: 24px; font-weight: bold;"
                                )
                            )
                        })
                    )
                ),
                tabPanel("National Results",
                    h3("National results for selected year and injection percentage", 
                       style = "text-align: center; margin-bottom: 30px; color: #000000;"),
                    fluidRow(
                        style = "padding: 20px;",
                        lapply(national_result_cols, function(col) {
                            label <- switch(col,
                                "cumulativelivessaved" = "Cumulative Lives Saved",
                                "cumulativeHCVinfectionaverted" = "Cumulative HCV Infection Averted",
                                "cumulativeHIVinfectionlowaverted" = "Cumulative HIV Infection Low Averted",
                                "cumulativeHIVinfectionmedaverted" = "Cumulative HIV Infection Med Averted",
                                "cumulativeHIVinfectionhighaverted" = "Cumulative HIV Infection High Averted",
                                "infectiousdiscountedcost" = "Infectious Discounted Cost",
                                "valueoflivessaved" = "Statistical Value of Lives Saved",
                                "cumulativedeaths" = "Actual deaths January 2020-June 2024"
                            )
                            column(
                                width = 4,
                                div(
                                    style = "background-color: #f8f9fa; padding: 15px; margin-bottom: 20px; border-radius: 5px; text-align: center;",
                                    h4(label, style = "margin-bottom: 15px; color: #FF7731;"),
                                    textOutput(paste0("national_result_", gsub("\\.", "_", col))),
                                    style = "font-size: 24px; font-weight: bold;"
                                )
                            )
                        })
                    )
                ),
                tabPanel("Data",
                    fluidRow(
                        column(6,
                            checkboxInput("subset_state", "Subset by State", value = TRUE)
                        ),
                        column(6,
                            checkboxInput("subset_county", "Subset by County", value = TRUE)
                        )
                    ),
                    DTOutput("filtered_data"),
                    downloadButton("downloadData", "Download as Excel", 
                                 style = "background-color: #FF7731; color: white; border: none; padding: 10px 20px; font-size: 16px; border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-top: 20px;")
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
    
    # Create national dataset
    national_data <- reactive({
        req(input$percentinjections, input$year)  # ensure frequency and year are selected
        
        # Filter by injection frequency and year only
        filtered <- data[data$percentinjections == input$percentinjections & data$Year == input$year,]
        
        filtered
    })
    
    # Create filtered dataset
    filtered_data <- reactive({
        req(input$state, input$percentinjections, input$year)  # ensure state, frequency, and year are selected
        
        filtered <- data
        
        # Filter by state if enabled
        if (input$subset_state) {
            filtered <- filtered[filtered$STATE_NAME == input$state,]
            
            # Filter by county if enabled and state is filtered
            if (input$subset_county && !is.null(input$county)) {
                filtered <- filtered[filtered$COUNTYNAME == input$county,]
            }
        }
        
        # Filter by injection frequency and year
        filtered <- filtered[filtered$percentinjections == input$percentinjections & filtered$Year == input$year,]
        
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
        formatRound(columns = c(
          "Provisional.Drug.Overdose.Deaths",
          "injectionsatophc",
          "pwid",
          "deathsper100k",
          "Population",
          "livessaved",
          "cumulativelivessaved"  ,
          "cumulativeHIVinfectionlow" ,
          "cumulativeHIVinfectionmed"    ,
          "cumulativeHIVinfectionhigh",       
          "HIVavertedhighrisk"        ,
          "HIVavertedmedrisk"      ,
          "HIVavertedlowrisk"            ,    
          "cumulativeHIVinfectionlowaverted",
          "cumulativeHIVinfectionmedaverted",
          "cumulativeHIVinfectionhighaverted",
          "HIVdiscountedcostsavingshigh" ,
          "HIVdiscountedcostsavingsmed"  ,
          "HIVdiscountedcostsavingslow",      
          "newHCVrisk"        ,
          "cumulativeHCVinfection"    ,
          "HCVavertedrisk"              ,
          "cumulativeHCVinfectionaverted" ,
          "HCVdiscountedcost"         ,
          "infectiousdiscountedcost" ,        
          "valueoflivessaved"  ,
          "totalsavings"  
          
        ), digits = 0)
    })
    
    # Download handler for Excel export
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("ophc-data-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            write_xlsx(filtered_data(), file)
        }
    )
    
    # Render blank plot
    output$plot1 <- renderPlot({
datasubsetted<-subset(data, data$percentinjections == input$percentinjections & data$Year == input$year)

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
                                    transform = function(x) {log2(1 + x)},
                                    inverse = function(x) {2^x - 1}
                                  ),
                                  "log10(1+value)" = scales::trans_new(
                                    name = "log10_plus1",
                                    transform = function(x) {log10(1 + x)},
                                    inverse = function(x) {10^x - 1}
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
      datasubsetted<-subset(data, data$percentinjections == input$percentinjections & data$Year == input$year)
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
                                         transform = function(x) {log2(1 + x)},
                                         inverse = function(x) {2^x - 1}
                                       ),
                                       "log10(1+value)" = scales::trans_new(
                                         name = "log10_plus1",
                                         transform = function(x) {log10(1 + x)},
                                         inverse = function(x) {10^x - 1}
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
    
    # Render national result outputs
    for(col in national_result_cols) {
        local({
            col_name <- col
            output_id <- paste0("national_result_", gsub("\\.", "_", col_name))
            
            output[[output_id]] <- renderText({
                data <- national_data()
                if(nrow(data) > 0) {
                    value <- sum(data[[col_name]], na.rm = TRUE)
                    # Add dollar sign for cost-related metrics and format with commas
                    formatted_value <- format(round(value), big.mark = ",", scientific = FALSE)
                    if (grepl("cost|savings|value", col_name, ignore.case = TRUE)) {
                        paste0("$", formatted_value)
                    } else {
                        formatted_value
                    }
                } else {
                    "No data available"
                }
            })
        })
    }
    
    # Render individual result outputs
    result_cols <- c(
      "cumulativelivessaved",
      "cumulativeHIVinfectionmedaverted",
      "cumulativeHCVinfectionaverted",
      "totalsavings",

        "Provisional.Drug.Overdose.Deaths",
        "injectionsatophc",
        "pwid",
        "deathsper100k",
        "Population",
        "livessaved",
        
        "cumulativeHIVinfectionlow",
        "cumulativeHIVinfectionmed",
        "cumulativeHIVinfectionhigh",
        "HIVavertedhighrisk",
        "HIVavertedmedrisk",
        "HIVavertedlowrisk",
        "cumulativeHIVinfectionlowaverted",
        
        "cumulativeHIVinfectionhighaverted",
        "HIVdiscountedcostsavingshigh",
        "HIVdiscountedcostsavingsmed",
        "HIVdiscountedcostsavingslow",
        "newHCVrisk",
        "cumulativeHCVinfection",
        "HCVavertedrisk",
        
        "HCVdiscountedcost",
        "infectiousdiscountedcost",
        "valueoflivessaved"
        
    )
    
    for(col in result_cols) {
        local({
            col_name <- col
            output_id <- paste0("result_", gsub("\\.", "_", col_name))
            
            output[[output_id]] <- renderText({
                data <- filtered_data()
                if(nrow(data) > 0) {
                    value <- data[[col_name]][1]
                    # Add dollar sign for cost-related metrics and format with commas
                    formatted_value <- format(round(value), big.mark = ",", scientific = FALSE)
                    if (grepl("cost|savings|value", col_name, ignore.case = TRUE)) {
                        paste0("$", formatted_value)
                    } else {
                        formatted_value
                    }
                } else {
                    "No data available"
                }
            })
        })
    }
}

# Run the application
shinyApp(ui = ui, server = server)
