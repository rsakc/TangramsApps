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


#Paired Puzzle Data
dataP <- data

  #Arranging Data
  dataP <- dataP %>% arrange(GroupID, PlayerID, Puzzle, Game)

  #Creating Identifier Column
  dataP$Identifier <- NA
  
  counter <- 1
  dataP$Identifier[1] <- 1
  
  for(i in 2:nrow(dataP)){
    
    if(dataP$GroupID[i] == dataP$GroupID[i-1] &
       dataP$PlayerID[i] == dataP$PlayerID[i-1]){
      
      dataP$Identifier[i] <- counter
      
    } else{
      counter <- counter + 1
      
      dataP$Identifier[i] <- counter
    }
  }
  
 #Remove players with only one row of data
 #Removing players with only one type of puzzle
  Index <- numeric()
  
  for(i in unique(dataP$Identifier)){
    
    temp <- dataP %>% filter(Identifier == i)
    
    if(nrow(temp) < 2){
      Index <- append(Index, i)
    }
    
    if(length(unique(temp$Puzzle)) == 1){
      Index <- append(Index, i)
    }
    
  }
  
  #Removing marked players
  #At this point, all players left have at least 2 rows, and at least 2 different puzzles
  dataP <- dataP %>% filter(!(Identifier %in% Index))
  

  
  #Keeping exactly two puzzles per player
  dataP$Index <- 0
 
   for(i in 2:nrow(dataP)){
      
     #Set up for the very first person only
      if(i == 2){
        counter <- 1
        puzzle <- dataP$Puzzle[1]
      }
  
      #If the players are the same and counter is 2
      if(counter == 2 & dataP$Identifier[i - 1] == dataP$Identifier[i]){
        dataP$Index[i] <- 1
      }
      
      #If the players are the same and puzzle matches the very first puzzle for that player
      if(dataP$Identifier[i - 1] == dataP$Identifier[i] &
         puzzle == dataP$Puzzle[i]){
        
        dataP$Index[i] <- 1
      }
      
     #If the players are the same but the puzzle does not match the very first puzzle for that player
      if(dataP$Identifier[i - 1] == dataP$Identifier[i] & 
         puzzle != dataP$Puzzle[i]){
        
        counter <- 2
      }
    
     #If the players are not the same
      if(dataP$Identifier[i - 1] != dataP$Identifier[i]){
        counter <-  1
        puzzle <- dataP$Puzzle[i]
      }
   }
  

  #Keeping only the rows that were not marked
    dataP <- dataP %>% filter(Index == 0)

 
  
  
#For UI Inputs
all_groups <- sort(unique(data$GroupID))
all_players <- sort(unique(data$PlayerID))



##UI
ui <- fluidPage(
  
  titlePanel("Tangrams Hypothesis Tests"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "groupID",
                  label = "Group ID:", 
                  choices =  all_groups,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("donkey","Donkey")),
      
      uiOutput(outputId = "player_out"),
              
      selectInput(inputId = "xvar",
                  label = "X Variable:",
                  choices = c("Puzzle", "Var1", "Var2", "Var3", "PlayerID"),
                  selected = "Puzzle",
                  multiple = FALSE),

      selectInput(inputId = "yvar",
                  label = "Y Variable:",
                  choices = c("Time", "NumClicks"),
                  selected = "Time",
                  multiple = FALSE),
    
      selectInput(inputId = "color",
                label = "Color by:",
                choices = c("Puzzle", "Var1", "Var2", "Var3", "RegTime", "DisplayTime", "HintOn", "NumHints", "HintTime"),
                selected = "HintOn",
                multiple = FALSE),
    
    selectInput(inputId = "facets",
                label = "Facet by:",
                choices = c("None", "Puzzle", "Var1", "Var2", "Var3", "RegTime", "DisplayTime", "HintOn", "NumHints"),
                selected = "None",
                multiple = FALSE),
    
    checkboxInput(inputId = "bplot",
                  label = "Add Boxplot",
                  value = FALSE),
    
    selectInput(inputId = "tests",
                label = "Statistical Tests:",
                choices = c("None", "Two Sample T-Test", "Paried T-Test", "ANOVA", "Block Design"),
                selected = "None",
                multiple = FALSE),
    
    checkboxGroupInput(inputId = "filter",
                       label = "Filter Data:",
                       choices = c("Paired Data", "Win Data"))),
                       
    
  
      #All data/clean data/good data (racer code)
  
      #Two sample t-test (exactly 2 levels)
      #Paired t-test (Exactly 2 levels)
      #ANOVA
      #Block Design
      
    
      mainPanel(
        plotOutput("plot")
      )
    
    )
  )


##Server
server <- function(input, output,session){
  
  
  ##Reactive Data
  plotDataR <- reactive({
    
    dataR <- data %>% filter(GroupID %in% input$groupID, !(PlayerID %in% input$playerID))
    
    return(dataR)
  })
  
  
  ##Dynamic Remove Player Input
  output$player_out <- renderUI({
    
    input_data <- data %>% filter(GroupID %in% input$groupID)
    players <- sort(unique(input_data$PlayerID))
    
    selectInput(inputId = "playerID",
                label = "Remove Player ID:",
                choices =  players,
                multiple = TRUE,
                selectize = TRUE)
    
  })
  
  
  
  ##Visualization
  output$plot <- renderPlot({
    
    #Require
    req(input$groupID)
    
    #Reactive Data
    plotData <- plotDataR()
    
    
    #General Plot
    
    #Boxplot checkbox not selected
    if(input$bplot == FALSE){
    myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color = input$color)) +
      geom_point(position=position_dodge(width = 0.75), size = 2) +
      labs(title = paste("Plot of",input$yvar, "by",input$xvar, "and Colored by", input$color)) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
            axis.title = element_text(size = 20), 
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 18), 
            legend.text = element_text(size = 16), 
            axis.text.y = element_text(size = 14)) 
    

    #Boxplot checkbox is selected
    } else{
      myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color = input$color)) +
        geom_boxplot() +
        geom_point(position=position_dodge(width = 0.75), size = 2) +
        labs(title = paste("Plot of",input$yvar, "by",input$xvar, "and Colored by", input$color)) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_text(size = 18), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14)) 
    }
    
    
    #Facet Option is selected
    if(input$facets != "None"){
      myplot <- myplot +
        facet_wrap(as.formula(paste("~", input$facets))) +
        labs(title = paste("Plot of",input$yvar, "by", input$xvar, "and Colored by", input$color, "and Faceted by", input$facets)) +
        theme(strip.text = element_text(size = 16)) 
    }
    
    return(myplot)
  })
  
  
  
  
  
#Closes Server
}


#Running Shiny App
shinyApp(ui = ui, server = server)