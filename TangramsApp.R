#Loading Libraries
library(shiny)
library(shinythemes)
library(stringr)
library(readr)
library(ggplot2)
library(dplyr)

#Tangrams Data
data <-read_csv("https://www.stat2games.sites.grinnell.edu/data/tangrams/getdata.php")

#Date Column
data <- data %>% mutate(Date = str_sub(Date, 1, 10))

#Converting to Columns to Factor/Character
data$Win <- as.factor(data$Win)
data$RegTime <- as.factor(data$RegTime)
data$DisplayTime <- as.factor(data$DisplayTime)
data$HintOn <- as.factor(data$HintOn)
data$NumHints <- as.factor(data$NumHints)

#For UI Inputs
all_groups <- sort(unique(data$GroupID))
all_players <- sort(unique(data$PlayerID))
all_puzzles <- sort(unique(data$Puzzle))


##UI

ui <- fluidPage(
  
  titlePanel("Tangrams Hypothesis Tests"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "groupID",
                  label = "Group ID:", 
                  choices =  c("all", all_groups),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      selectInput(inputId = "playerID",
                  label = "Remove Player ID:",
                  choices = all_players,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      selectInput(inputId = "puzzle",
                  label = "Puzzle:",
                  choices = all_puzzles,
                  selected = all_puzzles[1],
                  multiple = FALSE),
      
      selectInput(inputId = "xvar",
                  label = "X Variable:",
                  choices = c("NumClicks", "RegTime", "DisplayTime", "HintOn", "NumHints", "HintTime"),
                  selected = "NumClicks",
                  multiple = FALSE),

      selectInput(inputId = "yvar",
                  label = "Y Variable:",
                  choices = c("Time"),
                  selected = "Time",
                  multiple = FALSE)),
    
     
      #Color By?
      #Facet By?
      #Different Options for Plots
      #Different Options for Statistical Tests
    
    
      mainPanel()
    
    )
  )


##Server
server <- function(input, output,session) {}











#Running Shiny App
shinyApp(ui = ui, server = server)

      
      
