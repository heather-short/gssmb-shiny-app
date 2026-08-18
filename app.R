# GSSMB Shiny App - UI
# H. Short
# Wed Jul  8 08:46:08 2026 ------------------------------

# load required packages ------------------------------------------------------------------------------

library(shiny)
library(bslib)
library(DT)  
library(tidyverse)
library(shinythemes)
library(ggplot2)
library(thematic)
library(lubridate)

# load data ------------------------------------------------------------------------------

tagging = read_csv("data/released_fish_tagging_data.csv")
tagging_data = read_csv("data/tagging_data.csv")
release_wq = read_csv("data/release_water_quality_summary_2.csv")
fp_all = read_csv("data/cdec_all_fp_water_quality.csv")
rectangles_all = read_csv("data/release_rectangles.csv")
for_calfishtrack = read_csv("data/for_calfishtrack.csv")
tag_life_2026 = read_csv("data/tag_life_2026.csv")
release_data = read_csv("data/release_data.csv")

# begin user interface ------------------------------------------------------------------------------

ui <- page_navbar(
  
  # sets the color theme
  theme = bs_theme(version = 5, bootswatch = "yeti"),
  
  # app title
  title = "Georgiana Slough Salmonid Migratory Barrier Project", 
  navbar_options = navbar_options(underline = T),
  
  # make navbar dark
  tags$head(
    tags$style(HTML(".navbar {background-color: #343a40 !important;}
                           .navbar .navbar-brand,
                           .navbar .nav-link {color: #ffffff !important;}
                           .navbar .nav-link.active {color: #dcdcdc !important;}"))),
  
  # change color of table selection
  tags$head(
    tags$style(HTML("table.dataTable tbody tr.selected td,
                           table.dataTable tbody td.selected 
                           {border-top-color: white !important;
                            box-shadow: inset 0 0 0 9999px #53868B !important;}
                           table.dataTable tbody tr:active td 
                               {background-color: #53868B !important;}
                           :root {--dt-row-selected: transparent !important;}
                           table.dataTable tbody tr:hover, 
                           table.dataTable tbody tr:hover td 
                           {background-color: #D1EEEE !important;}"))),
  # change view to 90%
  tags$head(
    tags$style(HTML("body {zoom: 90%;}"))),
  
  # home page ------------------------------------------------------------------------------
  
  nav_panel(title = "Background", 
            
            fluidRow(
        
              column(5,
                     br(), br(),
                     h5("The Georgiana Slough Salmonid Migratory Barrier (GSSMB) is a 640-foot-long bio-acoustic fish fence (BAFF) 
                       that uses complementary lights, sounds, and a bubble curtain to deter out-migrating juvenile Chinook salmon 
                       away from the entrance of Georgiana Slough to remain in the mainstem Sacramento River as they make their way 
                       to the ocean. If entrained in Georgiana Slough, fish enter the complex and highly altered central and south Delta, 
                       increasing travel time and decreasing through-Delta survival following increased contact with sub-optimal habitat, 
                       predators, and agriculture and water-project pumps. Beginning in 2023, the California Department of Water Resources 
                       began the annual deployment and operation of the GSSMB from November to May. Additionally, using acoustic telemetry, 
                       seasonal routing and survival studies have been implemented to evaluate the efficiency of the Barrier on out-migrating 
                       juvenile Chinook salmon."),
                     br(),   
                     h5("Navigate through this Shiny App to explore the GSSMB Effectiveness Study data from water years 2024, 2025, and 2026."),
                     br(), br(),
                     div(style = "text-align: center;",
                     tags$img(src = "salmon2.jpg")),
                     br(),
                     h5("Figure 1: Juvenile Chinook salmon being measured before tagging surgery.", style = "text-align: center;")),
                     
              
              column(7, 
                     br(), br(),
                     div(style = "text-align: center;",
                         tags$img(src = "barrier.jpg", height = 700, width = 1000)),
                     br(),
                     h5("Figure 2: Aerial view of the Georgiana Slough Salmonid Migratory Barrier looking downstream at the Sacramento River/Georgiana Slough divergence. 
                        The Sacramento River continues (right) while Georgiana Slough branches off (left).", style = "text-align: center;"))),
            
  ), # end nav panel - background
  
  # tagging page ------------------------------------------------------------------------------
  
  nav_panel(title = "Tagging", 
            
            fluidRow(h2("Tagging Procedure"),
                     
                     # methods and selection box for water year       
                     column(6, 
                            br(),
                            h5("Methods: \n
                   To begin each surgical procedure, fish were placed in an anesthesia solution 
                   until they lost equilibrium and were no longer responsive to touch. The fish 
                   was removed from anesthesia and weighed to the nearest 0.1 g and fork length 
                   was measured to the nearest mm. Fish were then placed ventral side up on a foam 
                   block before an irrigation tube was placed in or near the mouth to allow for 
                   irrigation of the gills with a lower dose of the anesthetic solution. A small 
                   incision was made parallel to the mid-ventral line and an ss400 JSATS acoustic
                   transmitter was inserted into the coelomic cavity before the incision was closed 
                   with a single uninterrupted 2 x 2 x 2 surgical knot. The duration of each surgery 
                   was measured from the time the fish was removed from anesthesia to the time it was 
                   placed in recovery following surgery. Tagging methods were based on Liedke et al. (2012)."),
                            br(),
                            wellPanel(h4("Select a water year to explore the tagging data."),
                                      selectInput("wy", " ",
                                                  choices = sort(unique(tagging$water_year)),
                                                  selected = max(tagging$water_year)))), 
                     # tag picture 
                     column(6, 
                            br(), br(),
                            div(style = "text-align: center;",
                                tags$img(src = "tag.jpg")),
                            br(),
                            h5("Figure 1: JSATS ss400 acoustic transmitter", style = "text-align: center;"))),
            
            br(), 
            # add a line to separate from next row
            hr(style = "border-top: 2px solid #333; margin: 20px 0;"),
            
            ### morphometrics ------------------------------------------------------------------------------
            
            fluidRow(h2("Morphometrics"),
                     
                     # weight vs length plot
                     column(11,   
                            br(),
                            plotOutput("plot_wl"), 
                            br(),
                            h5("Figure 2: Fish weight (g) as a function of fork length (mm) 
                      by month of tagging. Each point represents an individual fish.")),
                     
                     # legend figure      
                     column(1, 
                            br(), br(),
                            tags$img(src = "legend2.png"), style = "text-align: left;")), # end row
            
            br(), 
            
            fluidRow(
              
              # weight table and histogram
              column(6, 
                     h3("Weight (g)", class = "text-center"),
                     br(),
                     DTOutput("table_weight"),
                     br(),
                     plotOutput("hist_weight"), 
                     br(),
                     h5("Figure 3: Distribution of Chinook salmon weight (g) by month of tagging.")), 
              
              # fork length table and histogram
              column(6, 
                     h3("Fork Length (mm)", class = "text-center"),
                     br(),
                     DTOutput("table_fl"),
                     br(),
                     plotOutput("hist_fl"),
                     br(),
                     h5("Figure 4: Distribution of Chinook salmon fork length (mm) by month of tagging."))), 
            
            br(), 
            # add a line to separate from next row
            hr(style = "border-top: 2px solid #333; margin: 20px 0;"),
            
            ### surgery times ------------------------------------------------------------------------------
            
            fluidRow(h2("Anesthetic, Surgery, and Recovery Times"),
                     
                     # anesthetic table and boxplot       
                     column(4, 
                            br(),
                            h3("Anesthetic", class = "text-center"),
                            DTOutput("anes_times"),
                            br(),
                            plotOutput("anesthetic_boxplot")),
                     
                     # surgery table and boxplot       
                     column(4, 
                            br(),
                            h3("Surgery", class = "text-center"),
                            DTOutput("surg_times"),
                            br(),
                            plotOutput("surgery_boxplot")),
                     
                     # recovery table and boxplot       
                     column(4, 
                            br(),
                            h3("Recovery", class = "text-center"),
                            DTOutput("rec_times"),
                            br(),
                            plotOutput("recovery_boxplot"))), 
            
            # row for figure caption
            fluidRow(br(), 
                     h5("Figure 5: Anesthetic, surgery, and recovery times for tagged Late-Fall and Fall Run juvenile Chinook salmon. The centerline of the boxplots 
                      represents the median, the box represents the interquartile range (IQR), the whiskers extend 1.5 times 
                      the IQR, and the black points represent values outside 1.5 times the IQR. 
                      The mean value for each boxplot is represented by a white point."), 
                     br(), br())
            
  ), # end nav panel - tagging
  
  # release page ------------------------------------------------------------------------------
  
  nav_panel(title = "Releases", 
            
            # heading
            fluidRow(h2("Release Procedure"),
                     
                     # methods and selection box for water year       
                     column(6, 
                            br(),
                            h5("Methods: Tagged fish were released over a two-week block, to cover a spring-neap tidal cycle, 
                      and at regular intervals throughout the diel cycle. Small subgroups of ten tagged fish were 
                      released every 3 hours on each study release date, resulting in eight subgroups daily 
                      (midnight, 3 a.m., 6 a.m., 9 a.m., noon, 3 p.m., 6 p.m., and 9 p.m.). 
                      Releases occurred over a 72-hour period during each week."),
                            br(),
                            wellPanel(h4("Select a water year to explore the tagging data."),
                                      selectInput("water_year", " ",
                                                  choices  = sort(unique(fp_all$water_year)),
                                                  selected = max(fp_all$water_year)))),
                     # release picture       
                     column(6,
                            div(style = "text-align: center;",
                                tags$img(src = "release2.jpg", height = 500, width = 800)),
                            br(),
                            h5("Figure 1: Fish being released into the Sacramento River.", style = "text-align: center;"))),
            
            br(), 
            # add a line to separate from next row
            hr(style = "border-top: 2px solid #333; margin: 20px 0;"),
            
            ### water quality ------------------------------------------------------------------------------
            
            fluidRow(h2("Water Quality during Fish Releases"),
                     
                     # selection box for release date and water quality metric       
                     column(3, 
                            br(), br(),
                            wellPanel(h4("Make selections to explore the release & water quality data."),
                                      selectInput("release_date", "Release Date:", choices = NULL),
                                      selectInput("value_type", "Water Qualtiy Metric:",
                                                  choices = c("Discharge (cfs)"      = "discharge",
                                                              "Velocity (ft/s)"      = "velocity",
                                                              "Temperature (°C)"     = "temperature",
                                                              "Turbidity (NTU)"      = "turbidity")))),
                     # water quality table       
                     column(9,   
                            br(),
                            DTOutput("release_wq_table"))),
            
            br(), 
            # water quality plot
            fluidRow(plotOutput("wq_plot"),
                     h5("Figure 1: CDEC water quality (discharge, velocity, temperature, or turbidity) at Freeport, CA. 
                      Shaded rectangles represent 72-hour release periods for each week of the study. 
                       The yellow point represents the selected release date.")),
  
), # end nav panel - releases

# tag life page ------------------------------------------------------------------------------

nav_panel(title = "Tag Life", 
          
          # heading
          fluidRow(h2("Tag Life Procedure"),
                   
                   # methods and selection box for water year       
                   column(6, 
                          br(),
                          h5("Methods: Tag life was evaluated to ensure that the transmitters used throughout study met the 
                             manufacturer’s stated run time. Tag life tags were activated at the same time as the rest of the 
                             tags for the tagging block. For every week of tagging, eight additional tags were activated and 
                             deployed in a tank with an acoustic receiver. The receiver was downloaded monthly and tags were 
                             left in the tank until their battery failed. Using tag detection data, the plot below was created 
                             confirm tag batteries ran to the manufacturer’s expected run time (at least 71 days)."),
                          br(),
                          wellPanel(h4("Select a water year to explore the tag life data."),
                                    selectInput("water_year", " ",
                                                choices  = sort(unique(fp_all$water_year)),
                                                selected = max(fp_all$water_year)))),
                   # release picture       
                   column(6,
                          div(style = "text-align: center;",
                              tags$img(src = "tags.jpg", height = 500, width = 800)),
                          br(),
                          h5("Figure 1: Box of numbered tag vials.", style = "text-align: center;"))),
          
          br(), 
          # add a line to separate from next row
          hr(style = "border-top: 2px solid #333; margin: 20px 0;"),
          
          ### tag life plot ------------------------------------------------------------------------------
          
          fluidRow(h2("Tag Life Data"),
                   
                   column(12,   
                          br(),
                          uiOutput("plotOrMessage"),
                          br(),
                          h5("Figure 2: Proportion of tags active plotted over time (days since tag activation). Colored lines represent individual tagging blocks 
                             (Block 1 = December, Block 2 = January, Block 3 = February, Block 4 = April) and unique batches of tags. 
                             The dashed line represents the 71 day tag life expectancy for JSATS model ss400 transmitters.")))
          
), # end nav panel - tag life

# download data ------------------------------------------------------------------------------

nav_panel(title = "Download Data", 
          
          fluidPage(
            
            fluidRow(h5("Make Water Year and Tagging Block selections below to generate reports for 
                        tagging data, release data, and for the CalFishTrack upload template. 
                        Select Download CSV once you have generated the appropriate report."),
                     br(), br(), 
          
              column(4,
                     wellPanel(h4("Tagging Report"),
                               selectInput("water_year", "Water Year:",
                                           choices  = sort(unique(tagging_data$water_year)),
                                           selected = max(tagging_data$water_year)),
                               selectInput("block", "Tagging Block:",
                                           choices  = sort(unique(tagging_data$block)),
                                           selected = min(tagging_data$block)),
                               actionButton("tagging_gen_report", "Generate Report", class = "btn-primary"),
                               br(), br(),
                               downloadButton("tag_download", "Download CSV"))),
              
              column(4,
                     wellPanel(h4("Release Report"),
                               selectInput("water_year", "Water Year:",
                                           choices  = sort(unique(release_data$water_year)),
                                           selected = max(release_data$water_year)),
                               selectInput("block", "Release Block:",
                                           choices  = sort(unique(release_data$block)),
                                           selected = min(release_data$block)),
                               actionButton("release_gen_report", "Generate Report", class = "btn-primary"),
                               br(), br(),
                               downloadButton("rel_download", "Download CSV"))),
              
              column(4,
                     wellPanel(h4("CalFishTrack Report"),
                               selectInput("water_year", "Water Year:",
                                           choices  = sort(unique(for_calfishtrack$water_year)),
                                           selected = max(for_calfishtrack$water_year)),
                               selectInput("block", "Tagging & Release Block:",
                                           choices  = sort(unique(for_calfishtrack$block)),
                                           selected = min(for_calfishtrack$block)),
                               actionButton("calfishtrack_gen_report", "Generate Report", class = "btn-primary"),
                               br(), br(),
                               downloadButton("calfishtrack_download", "Download CSV")))),
            
            fluidRow(
              
              column(12,
                     h3(textOutput("active_report_title")),
                     DTOutput("report_table")))),
          
) # end nav panel - download data
          
) # end page nav bar - end ui

# end ------------------------------------------------------------------------------

# server logic ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # weight and length plot ------------------------------------------------------------------------------
  
  output$plot_wl <- renderPlot({
    
    # require water year as an input
    req(input$wy)
    
    tagging %>% 
      
      # filter and mutate data
      filter(water_year == input$wy) %>% 
      mutate(water_year = factor(water_year), 
             date = ymd(date),
             run = factor(run),
             month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May"))) %>%
      
      # plot
      ggplot(aes(x = weight_gr, y = fork_length_mm, color = month)) + 
      facet_grid(~month) + 
      geom_jitter() + 
      scale_color_manual(values = c("Dec" = "#2F4F4F", "Jan" = "#53868B", "Feb" = "#6E8B3D",   
                                    "Apr" = "#A2CD5A", "May" = "#FFC125")) + 
      labs(x = "Weight (g)",
           y = "Fork Length (mm)") +
      theme(axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 12),
            axis.title.y = element_text(size = 16),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            plot.caption = element_text(size = 16, face = "italic", hjust = 0, margin = margin(t = 20)),
            title = element_blank(), 
            strip.text = element_text(size = 16))
    
  }, res = 96) # end render weight vs length plot
  
  # weight data table ------------------------------------------------------------------------------
  
  output$table_weight <- renderDT({
    
    # require water_year as an input  
    req(input$wy)
    
    df = tagging %>%
      select(water_year, month, run, weight_gr) %>%
      filter(water_year == input$wy) %>%
      mutate(month = factor(month, levels = c("Dec","Jan","Feb","Mar","Apr","May"))) %>%
      group_by(water_year, month, run) %>%
      summarize(n = n(),
                Minimum = round(min(weight_gr, na.rm = TRUE), 2),
                Maximum = round(max(weight_gr, na.rm = TRUE), 2),
                Mean = round(mean(weight_gr, na.rm = TRUE), 2),
                SD = round(sd(weight_gr, na.rm = TRUE), 2)) %>%
      ungroup() %>%
      rename("Water Year" = water_year,
             "Month" = month,
             "Run Type" = run)
    
    # creates data table with selectable row
    datatable(df,
              rownames = FALSE,
              selection = list(mode = "single"),   
              options = list(dom = "t",
                             server = FALSE, 
                             columnDefs = list(list(targets = 0, 
                                                    visible = FALSE, 
                                                    width = "0px"))))
    
  }) # end render data table
  
  # weight histogram ------------------------------------------------------------------------------
  
  output$hist_weight <- renderPlot({
    
    # require water_year as an input
    req(input$wy)
    
    # get the selected month from the clicked row (NULL if none selected)
    selected_month <- NULL
    row <- input$table_weight_rows_selected
    if (length(row) > 0) {
      df_lookup <- tagging %>%
        filter(water_year == input$wy) %>%
        mutate(month = factor(month, levels = c("Dec","Jan","Feb","Mar","Apr","May"))) %>%
        group_by(month) %>%
        summarize() %>%
        ungroup()
      
      # the summary table rows are in month order, so we can pull the month directly
      selected_month <- as.character(df_lookup$month[row])} # end row selection
    
    month_colors <- c("Dec" = "#2F4F4F", "Jan" = "#53868B", "Feb" = "#6E8B3D",   
                      "Apr" = "#A2CD5A", "May" = "#FFC125")
    
    # make the histogram  
    plot_data <- tagging %>%
      
      # require water_year as an input
      filter(water_year == input$wy) %>%
      mutate(month = factor(month, levels = c("Dec","Jan","Feb","Mar","Apr","May")),
             
             highlight = if (is.null(selected_month)) "all"
             else ifelse(month == selected_month, "selected", "other"))
    
    if (is.null(selected_month)) {
      # No row selected — normal plot, colored by month
      ggplot(plot_data, aes(x = weight_gr, fill = month)) +
        geom_histogram(alpha = 0.7, position = "identity") +
        scale_fill_manual(values = month_colors) +
        labs(x = "Weight (g)", 
             y = "Count") +
        theme(axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 12),
              legend.position = "none",
              plot.caption = element_text(size = 18, face = "italic", hjust = 0, margin = margin(t = 20)),
              title = element_blank())
      
    } # end if statement
    
    else {
      # row selected — highlight chosen month, grey out others
      ggplot(plot_data, aes(x = weight_gr,
                            fill = month,
                            alpha = highlight)) +
        geom_histogram(position = "identity") +
        scale_fill_manual(values = month_colors) +
        scale_alpha_manual(values = c(selected = 0.85, other = 0.15),
                           guide = "none") +
        labs(x = "Weight (g)", 
             y = "Count") +
        theme(axis.title.x = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.title.y = element_text(size = 16),
              axis.text.y = element_text(size = 12),
              legend.position = "none",
              title = element_blank())
      
    } # end else statement
    
  }) # end weight histogram
  
  # fork length data table ------------------------------------------------------------------------------
  
  output$table_fl <- renderDT({
    
    # require water_year as an input  
    req(input$wy)
    
    df = tagging %>%
      select(water_year, month, run, fork_length_mm) %>%
      filter(water_year == input$wy) %>%
      mutate(month = factor(month, levels = c("Dec","Jan","Feb","Mar","Apr","May"))) %>%
      group_by(water_year, month, run) %>%
      summarize("n" = n(), 
                "Minimum" = round(min(fork_length_mm, na.rm = T), 2),
                "Maximum" = round(max(fork_length_mm, na.rm = T), 2),
                "Mean" = round(mean(fork_length_mm, na.rm = T), 2),
                "SD" = round(sd(fork_length_mm, na.rm = T), 2)) %>%
      ungroup() %>%
      rename("Water Year" = water_year,
             "Month" = month,
             "Run Type" = run)
    
    # creates data table with selectable row
    datatable(df,
              rownames = FALSE,
              selection = list(mode = "single"),   
              options = list(dom = "t",
                             server = FALSE, 
                             columnDefs = list(list(targets = 0, 
                                                    visible = FALSE, 
                                                    width = "0px"))))
    
  }) # end render data table 
  
  # fork length histogram ------------------------------------------------------------------------------
  
  output$hist_fl <- renderPlot({
    
    # require water_year as an input
    req(input$wy)
    
    # get the selected month from the clicked row (NULL if none selected)
    selected_month <- NULL
    row <- input$table_fl_rows_selected
    if (length(row) > 0) {
      df_lookup <- tagging %>%
        filter(water_year == input$wy) %>%
        mutate(month = factor(month, levels = c("Dec","Jan","Feb","Mar","Apr","May"))) %>%
        group_by(month) %>%
        summarize() %>%
        ungroup()
      
      # The summary table rows are in month order, so we can pull the month directly
      selected_month <- as.character(df_lookup$month[row])} # end row selection
    
    month_colors <- c("Dec" = "#2F4F4F", "Jan" = "#53868B", "Feb" = "#6E8B3D",   
                      "Apr" = "#A2CD5A", "May" = "#FFC125")
    
    # make the histogram  
    df = tagging %>%
      
      # require water_year as an input
      filter(water_year == input$wy) %>%
      mutate(month = factor(month, levels = c("Dec","Jan","Feb","Mar","Apr","May")),
             
             highlight = if (is.null(selected_month)) "all"
             else ifelse(month == selected_month, "selected", "other"))
    
    if (is.null(selected_month)) {
      # no row selected — normal plot, colored by month
      ggplot(df, aes(x = fork_length_mm, fill = month)) +
        geom_histogram(alpha = 0.7, position = "identity") +
        scale_fill_manual(values = month_colors) +
        scale_y_continuous(position = "right") +
        labs(x = "Fork Length (mm)", 
             y = "Count") +
        theme(axis.title.x  = element_text(size = 16),
              axis.text.x   = element_text(size = 12),
              axis.title.y  = element_text(size = 16),
              axis.text.y   = element_text(size = 12),
              legend.position = "none",
              title = element_blank()
        )} # end if statement
    
    else {
      # row selected — highlight chosen month, grey out others
      ggplot(df, aes(x = fork_length_mm,
                            fill = month,
                            alpha = highlight)) +
        geom_histogram(position = "identity") +
        scale_fill_manual(values = month_colors) +
        scale_y_continuous(position = "right") +
        scale_alpha_manual(values = c(selected = 0.85, other = 0.15),
                           guide = "none") +
        labs(x = "Fork Length (mm)", 
             y = "Count") +
        theme(axis.title.x  = element_text(size = 16),
              axis.text.x   = element_text(size = 12),
              axis.title.y  = element_text(size = 16),
              axis.text.y   = element_text(size = 12),
              legend.position = "none",
              title = element_blank())
    } # end else statement
    
  }) # end length histogram
  
  # anesthetic table and boxplot ------------------------------------------------------------------------------
  
  # data table
  output$anes_times <- renderDT({
    
    # require water_year as an input  
    req(input$wy)
    
    df <- tagging %>% 
      filter(water_year == input$wy) %>% 
      mutate(month = factor(month, levels = c("Dec","Jan","Feb","Mar","Apr","May"))) %>% 
      group_by(water_year, month, run) %>% 
      summarize(n = n(),
                min_anes = round(min(anesthetic_time_sec, na.rm = TRUE)),
                max_anes = round(max(anesthetic_time_sec, na.rm = TRUE)),
                mean_anes = round(mean(anesthetic_time_sec, na.rm = TRUE)),
                sd_anes = round(sd(anesthetic_time_sec, na.rm = TRUE))) %>% 
      mutate(min_anes = sprintf("%02d:%02d", min_anes %/% 60, min_anes %% 60),
             max_anes = sprintf("%02d:%02d", max_anes %/% 60, max_anes %% 60),
             mean_anes = sprintf("%02d:%02d", mean_anes %/% 60, mean_anes %% 60),
             sd_anes = sprintf("%02d:%02d", sd_anes %/% 60, sd_anes %% 60)) %>% 
      ungroup() %>% 
      rename("Water Year" = water_year, 
             "Month" = month,
             "Run Type" = run, 
             "Min" = min_anes, 
             "Max" = max_anes, 
             "Mean" = mean_anes, 
             "SD" = sd_anes)
    
    # creates data table with selectable row
    datatable(df,
              rownames = FALSE,
              selection = list(mode = "single"),   
              options = list(dom = "t",
                             server = FALSE, 
                             columnDefs = list(list(targets = 0, 
                                                    visible = FALSE, 
                                                    width = "0px"))))
  }) # end render data table
  
  # boxplot
  
  output$anesthetic_boxplot <- renderPlot({req(input$wy)
    
    tagging %>%
      filter(water_year == input$wy) %>%
      ggplot(aes(x = run, y = anesthetic_time_sec, fill = run))+
      geom_boxplot()+
      labs(x = "Run Type",
           y = "Time in Anesthetic (mm:ss)") +
      scale_y_continuous(limits = c(50, 350),
                         breaks = seq(60, 300, by = 60),
                         labels = function(x) sprintf("%02d:%02d", x %/% 60, round(x %% 60))) +
      scale_x_discrete(labels = c("late-fall" = "Late Fall", "fall" = "Fall"), limits = c("late-fall", "fall")) +
      stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "white") +
      scale_fill_manual(values = c("late-fall" = "#53868B", "fall" = "#6E8B3D"))+
      theme(axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            legend.position = "none")
    
  }) # end render plot
  
  # surgery table and boxplot ------------------------------------------------------------------------------
  
  # data table
  output$surg_times <- renderDT({
    
    # require water_year as an input  
    req(input$wy)
    
    df <- tagging %>% 
      filter(water_year == input$wy) %>% 
      mutate(month = factor(month, levels = c("Dec","Jan","Feb","Mar","Apr","May"))) %>% 
      group_by(water_year, month, run) %>% 
      summarize(n = n(),
                min_surg = round(min(surgery_time_sec, na.rm = TRUE)),
                max_surg = round(max(surgery_time_sec, na.rm = TRUE)),
                mean_surg = round(mean(surgery_time_sec, na.rm = TRUE)),
                sd_surg = round(sd(surgery_time_sec, na.rm = TRUE))) %>% 
      mutate(min_surg = sprintf("%02d:%02d", min_surg %/% 60, min_surg %% 60),
             max_surg = sprintf("%02d:%02d", max_surg %/% 60, max_surg %% 60),
             mean_surg = sprintf("%02d:%02d", mean_surg %/% 60, mean_surg %% 60),
             sd_surg = sprintf("%02d:%02d", sd_surg %/% 60, sd_surg %% 60)) %>% 
      ungroup() %>% 
      rename("Water Year" = water_year, 
             "Month" = month,
             "Run Type" = run, 
             "Min" = min_surg, 
             "Max" = max_surg, 
             "Mean" = mean_surg, 
             "SD" = sd_surg)
    
    # creates data table with selectable row
    datatable(df,
              rownames = FALSE,
              selection = list(mode = "single"),   
              options = list(dom = "t",
                             server = FALSE, 
                             columnDefs = list(list(targets = 0, 
                                                    visible = FALSE, 
                                                    width = "0px"))))
  }) # end render data table
  
  # boxplot
  output$surgery_boxplot <- renderPlot({req(input$wy)
    
    tagging %>%
      filter(water_year == input$wy) %>%
      ggplot(aes(x = run, y = surgery_time_sec, fill = run))+
      geom_boxplot() +
      labs(x = "Run Type",
           y = "Time in Surgery (mm:ss)") +
      scale_y_continuous(limits = c(50, 350),
                         breaks = seq(60, 300, by = 60),
                         labels = function(x) sprintf("%02d:%02d", x %/% 60, round(x %% 60))) +
      scale_x_discrete(labels = c("late-fall" = "Late Fall", "fall" = "Fall"), limits = c("late-fall", "fall")) +
      stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "white") +
      scale_fill_manual(values = c("late-fall" = "#53868B", "fall" = "#6E8B3D"))+
      theme(axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            legend.position = "none")
    
  }) # end render boxplot
  
  # recovery table and boxplot ------------------------------------------------------------------------------
  
  # data table
  output$rec_times <- renderDT({
    
    # require water_year as an input  
    req(input$wy)
    
    df <- tagging %>% 
      filter(water_year == input$wy) %>% 
      mutate(month = factor(month, levels = c("Dec","Jan","Feb","Mar","Apr","May"))) %>% 
      group_by(water_year, month, run) %>% 
      summarize(n = n(),
                min_rec = round(min(recovery_time_sec, na.rm = TRUE)),
                max_rec = round(max(recovery_time_sec, na.rm = TRUE)),
                mean_rec = round(mean(recovery_time_sec, na.rm = TRUE)),
                sd_rec = round(sd(recovery_time_sec, na.rm = TRUE))) %>% 
      mutate(min_rec = sprintf("%02d:%02d", min_rec %/% 60, min_rec %% 60),
             max_rec = sprintf("%02d:%02d", max_rec %/% 60, max_rec %% 60),
             mean_rec = sprintf("%02d:%02d", mean_rec %/% 60, mean_rec %% 60),
             sd_rec = sprintf("%02d:%02d", sd_rec %/% 60, sd_rec %% 60)) %>% 
      ungroup() %>% 
      rename("Water Year" = water_year, 
             "Month" = month,
             "Run Type" = run, 
             "Min" = min_rec, 
             "Max" = max_rec, 
             "Mean" = mean_rec, 
             "SD" = sd_rec)
    
    # creates data table with selectable row
    datatable(df,
              rownames = FALSE,
              selection = list(mode = "single"),   
              options = list(dom = "t",
                             server = FALSE, 
                             columnDefs = list(list(targets = 0, 
                                                    visible = FALSE, 
                                                    width = "0px"))))
  }) # end render data table 
  
  # boxplot
  output$recovery_boxplot <- renderPlot({req(input$wy)
    
    tagging %>%
      filter(water_year == input$wy) %>%
      ggplot(aes(x = run, y = recovery_time_sec, fill = run))+
      geom_boxplot()+
      labs(x = "Run Type",
           y = "Time in Recovery (mm:ss)") +
      scale_y_continuous(limits = c(50, 350),
                         breaks = seq(60, 300, by = 60),
                         labels = function(x) sprintf("%02d:%02d", x %/% 60, round(x %% 60))) +
      scale_x_discrete(labels = c("late-fall" = "Late Fall", "fall" = "Fall"), limits = c("late-fall", "fall")) +
      stat_summary(fun = mean, geom = "point", shape = 16, size = 3, color = "white") +
      scale_fill_manual(values = c("late-fall" = "#53868B", "fall" = "#6E8B3D"))+
      theme(axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            legend.position = "none")
    
  }) # end render boxplot
  
  # release water quality data table ------------------------------------------------------------------------------
  
  # update release_date choices when water_year changes
  observe({
    
    req(input$water_year)
    
    dates <- release_wq %>%
      filter(water_year == input$water_year) %>%
      pull(release_date) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "release_date",
                      choices  = dates,
                      selected = dates[1])
  })
  
  # render the release_date selectInput
  output$release_date_ui <- renderUI({
    
    selectInput("release_date", "Release Date:", choices = NULL)
    
  })
  
  # filtered table data
  filtered_release_wq <- reactive({
    
    req(input$water_year, input$release_date)
    
    release_wq %>%
      filter(water_year == input$water_year,
             release_date == input$release_date) %>%
      mutate(rel_id = factor(rel_id, levels = c("RT-1", "RT-2", "RT-3", "RT-4", "RT-5", "RT-6", 
                                                "RT-7", "RT-8", "RT-9", "RT-10", "RT-11", "RT-12",
                                                "RT-13", "RT-14", "RT-15", "RT-16", "RT-17", "RT-18", 
                                                "RT-19", "RT-20", "RT-21", "RT-22", "RT-23", "RT-24", "point_rel"))) %>% 
      arrange(rel_id) %>%
      select(rel_id, release_time, lunar_phase, fish_released, temperature, turbidity, discharge, velocity) %>% 
      rename("Release ID"       = rel_id,
             "Time of Release" = release_time, 
             "Lunar Phase" = lunar_phase,
             "Fish Released" = fish_released,
             "Temperature (°C)" = temperature,
             "Turbidity (FNU)"  = turbidity,
             "Discharge (cfs)"  = discharge,
             "Velocity (ft/s)"  = velocity)
  })
  
  # Render DT
  output$release_wq_table <- renderDT({
    datatable(filtered_release_wq(),
              rownames  = FALSE,
              options = list(dom = "t",
                             server = FALSE))
    
  }) # end render data table
  
  # release water quality plot ------------------------------------------------------------------------------
  
  y_labels <- c(discharge   = "Discharge (cfs)",
                velocity    = "Velocity (ft/s)",
                temperature = "Temperature (°C)",
                turbidity   = "Turbidity (NTU)")
  
  fp_sub <- reactive({
    
    fp_all %>%
      filter(water_year == input$water_year,
             value_type == input$value_type) %>%
      mutate(datetime = ymd_hms(datetime))
    
  })
  
  rectangles_sub <- reactive({
    
    rectangles_all %>%
      filter(water_year == input$water_year) %>%
      mutate(start = ymd(start),
             end   = ymd(end))
    
  })
  
  output$wq_plot <- renderPlot({
    
    req(nrow(fp_sub()) > 0)
    
    p = ggplot(fp_sub(), aes(x = datetime, y = value)) +
      geom_rect(data = rectangles_sub(), inherit.aes = FALSE,
                aes(xmin = as.POSIXct(start),
                    xmax = as.POSIXct(end),
                    ymin = -Inf, ymax = Inf),
                fill = "darkgray", alpha = 0.40) +
      geom_line(color = "#2F4F4F", linewidth = 1.2) +
      scale_x_datetime(name = "Month",
                       date_breaks = "1 month",
                       date_labels = "%b") +
      labs(y = y_labels[input$value_type]) +
      theme_minimal(base_size = 14) +
      theme(axis.title = element_text(size = 16),
            axis.text  = element_text(size = 12))
    
    req(input$release_date)
    
    target_dt = as.POSIXct(input$release_date)
    
    closest_idx <- which.min(abs(difftime(fp_sub()$datetime, target_dt)))
    point_data  <- fp_sub()[closest_idx, ]
    
    p + geom_point(data = point_data,
                   aes(x = datetime, y = value),
                   shape = 21, color = "black", fill = "#FFC125", size = 4)
    
  }, res = 96) # end render plot
  
  # tag life plot ------------------------------------------------------------------------------
  
  output$plotOrMessage <- renderUI({
    if (input$water_year == 2026) {plotOutput("tag_life_plot")} 
    else {div(style = "color: #888; font-style: italic; font-size: 20px; padding: 20px; text-align: center;", 
              "Sorry, no data for this year.")}
  })
  
  
  # line plot
  output$tag_life_plot <- renderPlot({req(input$water_year)
    
    tag_life_2026 %>%
      filter(water_year == input$water_year) %>%
      ggplot(aes(x = days_since_activation, y = prop_active, color = factor(tagging_block)))+
      geom_line(linewidth = 2) + 
      labs(x = "Days Since Activation", 
           y = "Proportion of Tags Active", 
           color = "Tagging Block") + 
      scale_color_manual(values = c("1" = "#2F4F4F",
                                    "2" = "#53868B",
                                    "3" = "#6E8B3D",
                                    "4" = "#A2CD5A")) +
      geom_vline(xintercept = 71, linetype = "dashed") +
      theme(axis.title.x  = element_text(size = 16),
            axis.text.x   = element_text(size = 12),
            axis.title.y  = element_text(size = 16),
            axis.text.y   = element_text(size = 12))
    
  },res = 96) # end render plot
  
# reports
  active_report <- reactiveVal(NULL)
  
  # ---- Reactive data for each report type ----
  tagging_report <- reactive({
    
    req(input$water_year, input$block)
    
    tagging_data %>% 
      filter(water_year == input$water_year,
             block == input$block) %>% 
      
      mutate(anesthetic_time_mmss = as.character(anesthetic_time_mmss),
             surgery_time_mmss = as.character(surgery_time_mmss),
             recovery_time_mmss = as.character(recovery_time_mmss)) %>% 
      
      mutate(anesthetic_time_mmss = str_sub(anesthetic_time_mmss, end = -4),
             surgery_time_mmss = str_sub(surgery_time_mmss, end = -4)) %>% 
      
      select(water_year, block, tag_activation_date, species, run, date, surgeon, crew, vial_number, 
             tag_id_hex, weight_gr, fork_length_mm, anesthetic_time_mmss, surgery_time_mmss, recovery_time_mmss, comments)
  })
  
  release_report <- reactive({
    
    req(input$water_year, input$block)
    
    release_data %>%
      filter(water_year == input$water_year,
             block == input$block) %>% 
      
      mutate(release_datetime = parse_date_time(release_datetime, 
                                                orders = c("ymd HMS", "ymd HM", "ymd")),
             release_datetime = format(release_datetime, "%Y-%m-%d %H:%M:%S")) %>% 
      
      select(water_year, block, species, run, release_datetime, release_container_id, crew, morts, fish_released, comments)
    
  })
  
  calfishtrack_report <- reactive({
    
    req(input$water_year, input$block)
    
    for_calfishtrack %>% 
      filter(water_year == input$water_year,
             block == input$block) %>% 
      
      mutate(release_datetime = parse_date_time(release_datetime, 
                                                orders = c("ymd HMS", "ymd HM", "ymd")),
             release_datetime = format(release_datetime, "%Y-%m-%d %H:%M:%S")) %>% 
      
      rename(Release_Datetime_PST = release_datetime,
             TagID_Hex = tag_id_hex,
             Weight_gr = weight_gr, 
             Length_mm = fork_length_mm) %>% 
      
      mutate(Row = row_number(),
             StudyID = paste0("Georg_SL_Barrier_2026_", month),
             Release_riverkm = "162.64",
             Release_location = "Sacramento Marina",
             PRI_nominal = "5", 
             tag_battery_life_days = "71", 
             Release_Latitude = "38.560291", 
             Release_Longitude = "-121.517384") %>% 
      
      select(Row, StudyID, Release_Datetime_PST, TagID_Hex, Weight_gr, Length_mm, Release_riverkm,
             Release_location, PRI_nominal, tag_battery_life_days, Release_Latitude, Release_Longitude)
  })
  
  # ---- Set active report when a "Generate" button is clicked ----
  observeEvent(input$tagging_gen_report, {
    active_report("tagging_report")
  })
  
  observeEvent(input$release_gen_report, {
    active_report("release_report")
  })
  
  observeEvent(input$calfishtrack_gen_report, {
    active_report("calfishtrack_report")
  })
  
  # ---- Title above the table ----
  output$active_report_title <- renderText({
    req(active_report())
    switch(active_report(),
           tagging_report = "Tagging Report",
           release_report = "Release Report",
           calfishtrack_report   = "CalFishTrack Report"
    )
  })
  
  # ---- Shared table, switches based on active_report() ----
  output$report_table <- renderDT({
    req(active_report())
    
    df <- switch(active_report(),
                 tagging_report = tagging_report(),
                 release_report = release_report(),
                 calfishtrack_report   = calfishtrack_report()
    )
    
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- Download handlers (independent of which table is displayed) ----
  output$tag_download <- downloadHandler(
    filename = function() paste0("tagging_report.csv"),
    content = function(file) write.csv(tagging_report(), file, row.names = FALSE)
  )
  
  output$rel_download <- downloadHandler(
    filename = function() paste0("release_report.csv"),
    content = function(file) write.csv(release_report(), file, row.names = FALSE)
  )
  
  output$calfishtrack_download <- downloadHandler(
    filename = function() paste0("calfishtrack_report.csv"),
    content = function(file) write.csv(calfishtrack_report(), file, row.names = FALSE)
  )
} # end server

# run app-----------------------------------------------------------------------

shinyApp(ui = ui, server = server)