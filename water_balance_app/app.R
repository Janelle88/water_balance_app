library(shiny)
library(tidyverse)
library(shinythemes)
library(here)
library(janitor)
library(ggalt)



centroids <- read_csv(here::here("NCPN_centroids1.csv")) %>% 
    clean_names()

names_list = data.frame(
    var = centroids$park_name,
    short = centroids$park)

mylist <- as.list(names_list$short)
# Name it
names(mylist) <- names_list$var

# read in data

doy_avg_bc <- read_csv("doy_avg_bc.csv") %>% 
    select(!X1)

doy_avg_wc <- read_csv("doy_avg_wc.csv") %>% 
    select(!X1)

annual_values <- read_csv("annual_values.csv") %>% 
    select(!X1)


theme_set(theme_classic() + #has the L shape around the graph
              theme(panel.grid = element_blank(), #removes grid lines
                    plot.title = element_text(size = 25), #title of plot text size
                    legend.text = element_text(size = 15), #legend inside text size
                    legend.title = element_text(size = 18), #legend title text size
                    axis.title.y = element_text(size = 15), #changes y axis lable text size
                    axis.title.x = element_blank(),#x axis text size
                    axis.text.x = element_text(size = 15), #changes x-axis text size
                    axis.text.y = element_text(size = 15), #changes y-axis text size
                    legend.position = "top")) 

ui <- fluidPage(#open fluidPage
    
    navbarPage("Water Balance Predictions", theme = shinytheme("flatly"),
    
       tabPanel("About", fluid = TRUE, icon = icon("book-reader"), 
           
           mainPanel(
         
           h4("Importance of water balance in understanding climate change"),
           br(),
           p("Climate change will have impacts worldwide. Understanding how these changes will affect natural resources is important to better understand how they will respond. Most climate models use temperature and precipitation to show these changes, which can be hard to relate to changes in local habitats. Fortunately though, a dataset created by the National Parks Service's Mike Tercek, John Gross and David Thoma extrapolated out the data of these models into easier to understand water balance variables."),
           br(),
           img(src = "mccabe_and_markstrom.png", height = 422, width = 600)
           
           
       )# close mainPanel
       
       
       ),#close tabPanel About
       
       tabPanel("AET & Deficit", fluid = TRUE, icon = icon("chart-bar"), 
                
                sidebarLayout(
                    sidebarPanel("National Parks in the Northern Colorado Plateau",
                                 
                                 selectInput(inputId = "park_select_aet_d", 
                                             label = h3("Choose a Park:"), 
                                             choices = mylist),#close park_select
                                 
                                  radioButtons(inputId = "aet_d_radio", 
                                               label = h3("Select Scenario:"),
                                               choices = c("Hotter and wetter" = "annual_avg_bc", 
                                                           "Hotter and drier" = "annual_avg_wc")), #close radio buttons
                                 
     br(),
                                 
                                 img(src = "western_CONUS.png", height = 320, width = 330),                            
                    ), #close sidebarPanel
                    
                    
                    mainPanel(
                        
                        h4("Actual Evapotranspiration (AET) and Deficit"),
                        br(),
                        p("AET - the water plants use - and deficit - the water plants need - are one of the best measures of a habitats response to climate change than almost anything else. As can be seen in the graph below, habitats in the Western Continental United States fall neatly into these ranges."),
                           
                           br(),
                        
                        plotOutput(outputId = "aet_d_plot")
                        
                    )# close mainPanel
                    
                )#close sidebarPanel
                
       ),#close tabPanel aet_d
       
       tabPanel("Seasonality", fluid = TRUE, icon = icon("seedling"),

                sidebarLayout(
                    sidebarPanel("widgets",

                                 selectInput("park_select_seasonality", label = h3("Choose a Park:"),
                                             choices = mylist),#close park_select

                                 checkboxGroupInput("variable_select_seasonality", label = h3("Select water balance variables:"),
                                                    choices = unique(annual_values$variable),
                                                    selected = 1),#close variable_select


                                 radioButtons("radio", label = h3("Select Scenario:"),
                                              choices = list("Hotter and wetter" = "annual_avg_bc", "Hotter and drier" = "annual_avg_wc"),
                                              selected = 1)#close scenario_select


                    ), #close sidebarPanel


                    mainPanel(

                        "title"

                    )# close mainPanel

                )#close sidebarPanel

       ),#close tabPanel seasonality

       tabPanel("Time Series", fluid = TRUE, icon = icon("chart-line"),

                sidebarLayout(
                    sidebarPanel(

                        "widgets",

                        selectInput("park_select_time", label = h3("Choose a Park:"),
                                    choices = mylist,
                                    selected = 1),#close park_select

                        checkboxGroupInput("variable_select_time", label = h3("Select water balance variables:"),
                                           choices = unique(annual_values$variable),
                                           selected = 1),#close variable_select


                        radioButtons("radio", label = h3("Select Scenario:"),
                                     choices = list("Hotter and wetter" = "annual_avg_bc", "Hotter and drier" = "annual_avg_wc"),
                                     selected = 1)#close scenario_select

                    ), #close sidebarPanel


                    mainPanel(

                        "title"

                    )# close mainPanel

                )#close sidebarPanel

       ) #close tabPanel time series
       
    )# close navbarPage
)#close fluidPage

server <- function(input, output){
    
    # ------------------------
    # ------------------------
    # Deficit v. AET
    # ------------------------
    # ------------------------
    
    # max x limit for the graph - to change dynamically
    
   max_x_d_aet_reactive <-  reactive({
       max_x_d_aet_1 <- annual_values %>%
        filter(park %in% c(input$park_select_aet_d)) %>%
        filter(variable == "Deficit")

       max_x_d_aet_2 <- max(max_x_d_aet_1[,4], na.rm = TRUE)

       max_x_d_aet <- max_x_d_aet_2*1.15
       })#close max_x_d_aet_reactive

   # may y limit for graph - to change dynamically
   
   max_y_d_aet_reactive <-  reactive({
   max_y_d_aet_1 <- annual_values %>%
       filter(park == input$park_select_aet_d) %>%
       filter(variable == "AET")

   max_y_d_aet_2 <- max(max_y_d_aet_1[,4], na.rm = TRUE)

   max_y_d_aet <- max_y_d_aet_2*1.15
   }) # close max_y_d_aet_reactive
    
    
     aet_d_reactive <- reactive({
         require(input$park_select_aet_d, input$aet_d_radio)
         
         message('Selected: ', input$park_select_aet_d)
         message('Selected: ', input$aet_d_radio)
         
     annual_values %>% 
        filter(park %in% input$park_select_aet_d) %>% 
        filter(averages %in% c(input$aet_d_radio, "annual_avg")) %>% 
        pivot_wider(names_from = variable,
                    values_from = annual_value) %>% 
        clean_names()
    
    }) #close aet_d_reactive
    
    output$aet_d_plot <- renderPlot({
        message('Generating plot...')
    ggplot(aet_d_reactive()) +
        geom_point(aes(x = deficit, y = aet, color = decades),
                   size = 4,
                   alpha = 0.6) +
    geom_encircle(aes(x = deficit, y = aet, color = decades),
                  show.legend = FALSE,
                  size = 2) +
    labs(title = "Deficit v. AET",
         x = "Deficit",
         y = "AET") +
    scale_color_manual(breaks = c("1980-2019", "2020-2059", "2060-2099"),
                       values = c("#A3B86C", "#EBC944", "#1496BB")) +
    theme(legend.title = element_blank()) +
    ylim(NA, max_y_d_aet_reactive()) +
    xlim(NA, max_x_d_aet_reactive()) +
    theme(axis.title.x = element_text(size = 15))
    })#close renderPlot$aet_d
        
    
    # ----------------------
    #-----------------------
    #  seasonality
    # ----------------------
    # ----------------------
    
    # max_y_seasonality <- reactive({   
    #     
    #     max_y_1 <- doy_avg_bc %>%
    #         full_join(doy_avg_wc) %>%
    #         filter(park %in% input$park_select_seasonality) %>% 
    #         select(input$variable_select_seasonality)
    #     
    #     
    #     max_y_2 <- max(max_y_1[,1], na.rm = TRUE)
    #     
    #     max_y <- max_y_2*1.15
    #     
    # })
    #     
    #         
    #         ggplot(doy_avg_bc) + 
    #             geom_line(aes(x = as.Date.numeric(doy, origin = "1980-01-01"),
    #                           y = .data[[.y]],
    #                           color = decades),
    #                       size = 0.7,
    #                       alpha = 0.4) +
    #             stat_smooth(size = 1.5,
    #                         se = FALSE,
    #                         aes(x = as.Date.numeric(doy, origin = "1980-01-01"),#allows for month on xaxis
    #                             y = .data[[.y]],
    #                             # calling my variable, which I'll name later in the command
    #                             group = decades,
    #                             color = decades),
    #                         method = glm,
    #                         family = quasipoisson,
    #                         formula = y ~ ns(x, 16)) +
    #             theme(legend.title = element_blank())+
    #             #internet said to do this
    #             #quasipoisson is some type of statistical analysis type
    #             #forumula has something to do with the degrees of freedom
    #             #https://stackoverflow.com/questions/2777053/in-ggplot-restrict-y-to-be-0-in-loess
    #             # geom_vline(data = doy_avg_bc,
    #             #            xintercept = doy[.data[[.y]] == max(.data[[.y]])], color='red') +
    #             labs(color = "Year",
    #                  y = ifelse(.y == "agdd_daily", "Growing Degree Days (C)", "Water (mm)"), 
    #                  # R needs the "" here
    #                  # the ifelse goes after the command you want to change
    #                  #.y is useful because it prevents R from confusing it with something else
    #                  # purrr is much more useful than a for loop here
    #                  # if {} else{} and if_else() both didn't work, but I don't know why
    #                  title = case_when(.y ==  "soil_water_daily" ~ 
    #                                        wrap_sentence(paste("Soil Water Seasonality",
    #                                                            model_bc, model_bc_rcp_name), 30), #title based on variable
    #                                    .y == "runoff_daily" ~ 
    #                                        wrap_sentence(paste("Runoff Seasonality",
    #                                                            model_bc, model_bc_rcp_name), 30),
    #                                    .y == "rain_daily" ~  
    #                                        wrap_sentence(paste("Rain Seasonality",
    #                                                            model_bc, model_bc_rcp_name), 30),
    #                                    .y == "agdd_daily" ~ 
    #                                        wrap_sentence(paste("AGDD Seasonality",
    #                                                            model_bc,model_bc_rcp_name), 30),
    #                                    .y == "accumswe_daily"~ 
    #                                        wrap_sentence(paste(
    #                                            "Accumulated SWE Seasonality",
    #                                            model_bc,model_bc_rcp_name), 30),
    #                                    .y == "pet_daily" ~ 
    #                                        wrap_sentence(paste(
    #                                            "Potential Evapotranspiration Seasonality",
    #                                            model_bc,model_bc_rcp_name), 30),
    #                                    .y == "deficit_daily" ~ 
    #                                        wrap_sentence(paste("Deficit Seasonality",
    #                                                            model_bc, model_bc_rcp_name), 30),
    #                                    TRUE ~ 
    #                                        wrap_sentence(paste(
    #                                            "Actual Evapotranspiration Seasonality",
    #                                            model_bc, model_bc_rcp_name), 30))) + 
    #             scale_color_manual(breaks = c("1980-2019", "2020-2059", "2060-2099"),
    #                                values = c("#A3B86C", "#EBC944", "#1496BB")) +
    #             ylim(0, max_y) +
    #             scale_x_date(as.Date.numeric(doy_avg_bc$doy, origin = "1980-01-01"),
    #                          breaks = seq(as.Date("1980-01-15"), 
    #                                       as.Date("1980-12-15"),#plots dates midmonth
    #                                       by = "1 month"),
    #                          date_labels = "%b")#%b is abbreviated %B is full month name
            

            
        

        # model_name_reactive <- reactive({
        #     centroids %>%
        #     filter(park == input$park_select_aet_d) %>%
        #     ifelse(input$aet_d_radio == annual_avg_bc,
        #            pull(model_bc),
        #            pull(model_wc))
        #     })
        # 
        # model_rcp_reactive <- reactive({
        #     centroids %>%
        #     filter(park == input$park_select_aet_d) %>%
        #     ifelse(input$aet_d_radio == annual_avg_bc,
        #            pull(model_bc_rcp),
        #            pull(model_wc_rcp))
        # })
        
        
    
    
}#close server

# Combine into an app
shinyApp(ui = ui, server = server)