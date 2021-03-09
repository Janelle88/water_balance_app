library(shiny)
library(tidyverse)
library(shinythemes)
library(here)
library(janitor)
library(ggalt)
library(splines)
library(gridExtra)



centroids <- read_csv(here::here("NCPN_centroids1.csv")) %>% 
    clean_names()


# read in data

doy_avg_bc <- read_csv("doy_avg_bc.csv") %>% 
    select(!X1) %>% 
    mutate(scenario = "best_case")

doy_avg_wc <- read_csv("doy_avg_wc.csv") %>% 
    select(!X1) %>% 
    mutate(scenario = "worst_case")

doy_avg <- doy_avg_bc %>% 
    full_join(doy_avg_wc) %>% 
    mutate(doy = as.Date.numeric(doy, origin = "1980-01-01"))

annual_values <- read_csv("annual_values.csv") %>% 
    select(!X1)

wrap_sentence <- function(string, width) {
    words <- unlist(strsplit(string, " "))
    fullsentence <- ""
    checklen <- ""
    for(i in 1:length(words)) {
        checklen <- paste(checklen, words[i])
        if(nchar(checklen)>(width+1)) {
            fullsentence <- paste0(fullsentence, "\n")
            checklen <- ""
        }
        fullsentence <- paste(fullsentence, words[i])
    }
    fullsentence <- sub("^\\s", "", fullsentence)
    fullsentence <- gsub("\n ", "\n", fullsentence)
    return(fullsentence)
}
#wrap_sentence function taken from https://stackoverflow.com/a/27734975/14061596

names_list = data.frame(
    var_name = centroids$park_name,
    short_name = centroids$park)

mylist_park <- as.list(names_list$short_name)
# Name it
names(mylist_park) <- names_list$var_name

variable_list = data.frame(
    var_var = unique(annual_values$variable),
    short_var = c("soil_water_daily", "runoff_daily", "rain_daily", "accumswe_daily", "pet_daily", "deficit_daily", "aet_daily")
)

mylist_variable <- as.list(variable_list$short_var)

names(mylist_variable) <- variable_list$var_var


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
           img(src = "mccabe_and_markstrom.png", height = 422, width = 600),
           br(),
           
           p("For more information, check out this presenetation I gave in January 2021 to resource managers in National Parks in the Northern Colorado Plateau:"),
           br(),
           
           HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/PwqnAD5k6ao" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
           
       )# close mainPanel
       
       
       ),#close tabPanel About
       
       tabPanel("AET & Deficit", fluid = TRUE, icon = icon("chart-bar"), 
                
                sidebarLayout(
                    sidebarPanel("National Parks in the Northern Colorado Plateau",
                                 
                                 selectInput(inputId = "park_select_aet_d", 
                                             label = h3("Choose a Park:"), 
                                             choices = mylist_park),#close park_select
                                 
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
                        p("AET - the water plants use - and deficit - the water plants need - when graphed against each other is one of the best measures of a habitats response to climate change than almost anything else. As can be seen in the graph to the left, habitats in the Western Continental United States fall neatly into these ranges. Use the graph to the left to compare changes through time to a park you are familiar with. Does it remain in the same range for the habitat it has been, or does it change? Also, toggle betweeen the two scenarios for the future - hotter and wetter, or hotter and drier. How do the two climate futures compare to each other? How would management have to change in a drier climate future compared to a wetter climate future?"),
                          p("Each bubble in the reactive plot represents 40 years, with one observation for each year. 40 in the recent past in green, 40 in the near future in yellow, and 40 in the distant futre in blue."),
                           
                           br(),
                        
                        plotOutput(outputId = "aet_d_plot")
                        
                    )# close mainPanel
                    
                )#close sidebarPanel
                
       ),#close tabPanel aet_d
       
       tabPanel("Seasonality", fluid = TRUE, icon = icon("seedling"),

                sidebarLayout(
                    sidebarPanel("widgets",

                                 selectInput("park_select_seasonality", label = h3("Choose a Park:"),
                                             choices = mylist_park),#close park_select

                                 checkboxGroupInput("variable_select_seasonality", label = h3("Select water balance variables:"),
                                                    choices = mylist_variable, selected = "soil_water_daily"
                                                    ),#close variable_select


                                 radioButtons("seasonality_radio", label = h3("Select Scenario:"),
                                              choices = list("Hotter and wetter" = "best_case", "Hotter and drier" = "worst_case"))#close scenario_select
                                 
                            


                    ), #close sidebarPanel


                    mainPanel(

                        h4("Seasaonality Plots"), 
                        
                        p("Seasonality plots help us to understand how peaks and valleys will change through time. We can see how timing of events will change as a result of climate change. For water balance variables, this is important because many ecosystems are reliant on timing. One example of this in the Colorado Plateau Region is the monsoon season. If timing of this were to change, this could have large cascading impacts. Knowing what the future looks like in the hotter and drier or hotter and wetter scenarios allows managers to prepare for these possible changes. Check out all water balance variables at once or click on only the ones you are interested in. Also, toggle between the two climate futures to compare how each variable may look in the different scenarios."),
                        
                        plotOutput(outputId = "seasonality_plot")
                        
                        # if(num_plots == 3){plotOutput(outputId = "seasonality_plot",
                        #                width = "100%",
                        #                height = "400px"
                        # )},
                        # 
                        # if(num_plots == 4){
                        #     plotOutput(outputId = "seasonality_plot",
                        #                width = "100%",
                        #                height = "500px")
                        # }

                    )# close mainPanel

                )#close sidebarPanel

       ),#close tabPanel seasonality

       tabPanel("Time Series", fluid = TRUE, icon = icon("chart-line"),

                sidebarLayout(
                    sidebarPanel(

                        selectInput("park_select_time", label = h3("Choose a Park:"),
                                    choices = mylist_park),#close park_select

                        checkboxGroupInput("variable_select_time", label = h3("Select water balance variables:"),
                                           choices = unique(annual_values$variable), selected = "Soil Water"),#close variable_select


                        radioButtons("time_radio", label = h3("Select Scenario:"),
                                     choices = list("Hotter and wetter" = "annual_avg_bc", "Hotter and drier" = "annual_avg_wc"),
                                     selected = "annual_avg_bc")#close scenario_select

                    ), #close sidebarPanel


                    mainPanel(

                        h4("Annual values through time"),
                        
                        p("Understanding values through time helps us to see what the future will look like compared to the present. The years highlighted in the graph below show a historically 'wet' year in blue and a historically 'dry' year in red. These are offered for easy comparison, to be able to understand how the future may relate to years you are familiar with."),
                        plotOutput(outputId = "time_plot")

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

    
    seasonality_reactive <- reactive({
        doy_avg %>% 
            filter(scenario %in% input$seasonality_radio) %>% 
            filter(park %in% input$park_select_seasonality) 
    })
    
    num_plots <- reactive({length(input$variable_select_seasonality)})

     output$seasonality_plot <- renderPlot({

         if(!is.null(input$variable_select_seasonality)) {

         nplot<-length(input$variable_select_seasonality)

         p<-list()

         for (i in 1:nplot) {
             p[[i]]<- ggplot(data = seasonality_reactive(), aes_string(y = input$variable_select_seasonality[i])) +
                 geom_line(aes(x = doy,
                               color = decades),
                           size = 0.7,
                           alpha = 0.4) +
                 stat_smooth(size = 1.5,
                             se = FALSE,
                             aes(x = doy,
                                 group = decades,
                                 color = decades),
                             method = glm,
                             family = quasipoisson,
                             formula = y ~ ns(x, 16)) +
                 theme(legend.title = element_blank())+
                 #internet said to do this
                 #quasipoisson is some type of statistical analysis type
                 #forumula has something to do with the degrees of freedom
                 #https://stackoverflow.com/questions/2777053/in-ggplot-restrict-y-to-be-0-in-loess
                 # geom_vline(data = doy_avg_bc,
                 #            xintercept = doy[.data[[.y]] == max(.data[[.y]])], color='red') +
                 labs(color = "Year",
                      y = ifelse(input$variable_select_seasonality[i] == "agdd_daily", "Growing Degree Days (C)", "Water (mm)"),
                 # R needs the "" here
                 # the ifelse goes after the command you want to change
                 # .y is useful because it prevents R from confusing it with something else
                 # purrr is much more useful than a for loop here
                 # if {} else{} and if_else() both didn't work, but I don't know why
                 title = case_when(input$variable_select_seasonality[i] ==  "soil_water_daily" ~ "Soil Water Seasonality",
                                   input$variable_select_seasonality[i] == "runoff_daily" ~ "Runoff Seasonality",
                                   input$variable_select_seasonality[i] == "rain_daily" ~ "Rain Seasonality",
                                   input$variable_select_seasonality[i] == "agdd_daily" ~ "AGDD Seasonality",
                                   input$variable_select_seasonality[i] == "accumswe_daily"~ "Accumulated SWE Seasonality",

                                   input$variable_select_seasonality[i] == "pet_daily" ~
                                       wrap_sentence("Potential Evapotranspiration Seasonality", 30),
                                   input$variable_select_seasonality[i] == "deficit_daily" ~ "Deficit Seasonality",
                                   TRUE ~ wrap_sentence("Actual Evapotranspiration easonality", 30))) +
                 scale_color_manual(breaks = c("1980-2019", "2020-2059", "2060-2099"),
                                    values = c("#A3B86C", "#EBC944", "#1496BB")) +
                 #ylim(0, max_y_seasonality()) +
                 scale_x_date(doy_avg$doy,
                              breaks = seq(as.Date("1980-01-15"),
                                           as.Date("1980-12-15"),#plots dates midmonth
                                           by = "1 month"),
                              date_labels = "%b")#%b is abbreviated %B is full month name
             
            
             
         }
         
         do.call(grid.arrange, p)
         }
         
         
    })
           
     # ---------------------
     # ---------------------
     # time series plot
     # ---------------------
     # ---------------------

     time_reactive <- reactive({
         
         annual_values %>%
             filter(averages %in% c(input$time_radio, "annual_avg")) %>%
             filter(park %in% input$park_select_time)
         
     })# close time reactive
     
     point_low <- reactive({annual_values %>%
             filter(averages %in% c(input$time_radio, "annual_avg")) %>%
             filter(park %in% input$park_select_time) %>% 
             filter(year == 2012)})
     
     point_high<- reactive({annual_values %>%
             filter(averages %in% c(input$time_radio, "annual_avg")) %>%
             filter(park %in% input$park_select_time) %>% 
             filter(year == 2016)})

    
     output$time_plot <- renderPlot({

         if(!is.null(input$variable_select_time)) {

             nplot_time<-length(input$variable_select_time)

             p_time<-list()

             for(i in 1:nplot_time) {
                 
                 
                 p_time[[i]] <- ggplot(data = time_reactive() %>% filter(variable %in% annual_values$variable[i]))+
                         geom_line(aes(x = year,
                                       y = annual_value,
                                       color = averages),
                                   size = .75) +
                         labs(title = wrap_sentence(paste("Annual",
                                                          input$variable_select_time[i]),30),
                              y = ifelse(input$variable_select_time[i] == "agdd_daily",
                                         "Growing Degree Days (C)",
                                         "Water (mm)")) +
                     geom_hline(data = point_low()%>% 
                                    filter(variable %in% annual_values$variable[i]), # draws a line at a dry year
                                yintercept = point_low()%>% 
                                    filter(variable %in% annual_values$variable[i]) %>%
                                    pull(annual_value),
                                linetype = "dashed",
                                color = "#DA621E") +
                     geom_hline(data = point_high() %>% 
                                      filter(variable %in% annual_values$variable[i]), # draws a line at a wet year
                                yintercept = point_high() %>% 
                                    filter(variable %in% annual_values$variable[i]) %>%
                                    pull(annual_value),
                                linetype = "dashed",
                                color = "#1287A8") +
                     geom_point(data = point_low() %>% 
                                    filter(variable %in% annual_values$variable[i]), # highlights a dry year point
                                aes(x = year, y = annual_value),
                                color = "#DA621E",
                                alpha =  0.25,
                                size = 4) +
                     geom_point(data = point_high()%>% 
                                    filter(variable %in% annual_values$variable[i]), # highlights a dry year point
                                aes(x = year, y = annual_value),
                                color = "#1287A8",
                                alpha =  0.25,
                                size = 4) +
                     geom_text(data = point_low()%>% 
                                   filter(variable %in% annual_values$variable[i]), # labels the line
                               aes(x = 1973, 
                                   y = annual_value,
                                   label = year, 
                                   vjust = -0.35), # -0.x moves the text up above the line
                               color = "#DA621E") +
                     geom_text(data = point_high()%>% 
                                   filter(variable %in% annual_values$variable[i]), # labels the line
                               aes(x = 1973, 
                                   y = annual_value,
                                   label = year, 
                                   vjust = -0.35), # -0.x moves the text up above the line
                               color = "#1287A8") +
                         scale_color_manual(name = "", values = c("gray60","#EBC944"), labels = (c("Historical", "Predicted")))

             }# close plot loop
             do.call(grid.arrange,p_time)
         }# close if is.null
         }) #close timeplot
        
        
    
    
}#close server

# Combine into an app
shinyApp(ui = ui, server = server)