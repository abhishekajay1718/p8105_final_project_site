#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(rsconnect)
library(flexdashboard)
library(plotly)
library(tidyverse)
library(lubridate)
library(patchwork)
library(xml2)
library(rvest)
library(viridis)
library(grid)
library(rvest)
library(shiny)

load("./data/finaldat.RData")

finaldat = finaldat %>%
  mutate(season = 
           ifelse(incident_month %in% 9:11, "Fall",
                  ifelse(incident_month %in% c(12,1,2), "Winter",
                         ifelse(incident_month %in% 3:5, "Spring", "Summer"))), 
         hour_of_day = 
           ifelse(hour %in% 6:11, "morning",
                  ifelse(hour %in% 12:17, "afternoon",
                         ifelse(hour %in% 18:23, "night","dawn"))), 
         over_8min = ifelse(response_time > 8, "8min+", "8min-"),
         prcp_ctg = 
           if_else(prcp == 0, "no_prcp",
                   if_else((prcp > 0 & prcp <= 25), "low", "high")),
         snow_ctg = 
           if_else(snow == 0, "no_snow",
                   if_else((snow > 0 & snow <= 50), "low", "high"))) %>%
  
  mutate(season = fct_relevel(season, c("Spring", "Summer", "Fall", "Winter")))



finaldat_8min =
  finaldat %>% 
  mutate(over_8min = ifelse(response_time > 8, "8min+", "8min-"), over_8min = as.factor(over_8min))

zip_coor = read.csv("./data/US Zip Codes from 2013 Government Data")

zip_coor =
  zip_coor %>% 
  janitor::clean_names() %>% 
  rename(zip_code = zip, long = lng)

finaldat_8min_zip_coor = merge(finaldat_8min, zip_coor) %>%
  separate(borough_desc, into = c("remove", "borough"), sep = "-") %>%
  select(-c(remove))

boros = 
  finaldat_8min_zip_coor %>% 
  distinct(borough) %>%
  pull()

season = finaldat_8min_zip_coor %>% distinct(season) %>% pull()

# Define UI for application that draws a histogram
ui = fluidPage(
  # selectInput widget
  selectInput("borough_choice", label = h3("Select boro"), choices = boros, selected = "Manhattan"),
  # radioButtons widget
  radioButtons("season_choice", label = h3("Choose season"),
               choices = season, 
               selected = "Winter")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output = 
    renderPlotly({ 
    finaldat_8min_zip_coor %>%
      filter(borough == input$borough_choice, season == input$season_choice) %>%
      group_by(zip_code) %>%
      plot_ly(x = ~lat, y = ~long, type = "scatter", mode = "markers",
              alpha = 0.5, 
              color = ~over_8min)})
  
  renderPlotly({
    finaldat_8min_zip_coor %>%
      filter(borough == input$borough_choice, season == input$season_choice) %>%
      group_by(zip_code) %>%
      mutate(response_time = as.numeric(response_time)) %>%
      mutate(mean_res_time = mean(response_time)) %>% 
      mutate(text_label = str_c("Hour:", hour, " o'clock")) %>% 
      plot_ly(x = ~hour, y = ~mean_res_time, type = "bar", 
              alpha = 0.5, 
              text = ~text_label)})
  
  renderPlotly({
    finaldat_8min_zip_coor %>%
      filter(borough == input$borough_choice, season == input$season_choice) %>%
      group_by(zip_code) %>%
      mutate(response_time = as.numeric(response_time)) %>%
      mutate(mean_res_time = mean(response_time)) %>% 
      plot_ly(x = ~lat, y = ~long, type = "scatter", mode = "markers",
              alpha = 0.5, 
              color = ~mean_res_time, size = ~mean_res_time)})
}

# Run the application 
shinyApp(ui = ui, server = server)

rsconnect::deployApp('./shiny_dashboard/')
