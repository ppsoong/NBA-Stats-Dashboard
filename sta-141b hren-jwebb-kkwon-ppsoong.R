
library(tidyverse)
library(httr)
library(rvest)
library(devtools)
library(jsonlite)
library(tidyverse)
library(plotly)
library(ggplot2)
library(shiny)
library(plotrix)
library(shinythemes)




nba <-read_html("https://www.basketball-reference.com/leagues/NBA_2020_per_game.html") 
nba_table <- html_table(nba)
player_stats <- data.frame(nba_table)
player_stats = player_stats[complete.cases(player_stats),]
player_stats[,-c(2,3,5)] = lapply(player_stats[,-c(2,3,5)], as.numeric)
players = player_stats %>% select(Player) %>% unique() 
teams = player_stats %>% select(Tm) %>% unique()

#by_team <- player_stats %>% group_by(Tm) %>% arrange(Tm) %>% arrange(Player)#added this jw

ui <- fluidPage(theme=shinytheme("slate"),
  
  #App Title
  titlePanel(h1("BASKETBALL REFERENCE API",align = "center")),
  navbarPage("Menu",
  tabPanel("Visualizations",
  
  #Sidebar layout with input and output definitions
  sidebarLayout(position = "left",
    
    #Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Slider for the number of bins
      
      selectInput(inputId = "team",
                  label = h3("select a team: "),
                  choice = teams,
                  selected = "NOP"),
      
      selectInput(inputId = "xaxis",
                  label = h3("select statistic 1: "),
                  choice = colnames(player_stats[,-c(2,3,5)]),
                  selected = "X2P."),
                                                               
  
      selectInput(inputId = "yaxis",
                   label = h3("select statistic 2: "),
                    choice = colnames(player_stats[,-c(2,3,5)]),
                    selected = "FG."),
      
      DT::dataTableOutput("dtab")
      ),
      
      
    
    
    #Main panel for displaying outputs
    mainPanel("Main",
      tabsetPanel(
        # put it back here 
        tabPanel("Scatterplot", plotlyOutput("plot3")),
        
        tabPanel("Barplots",
                 plotOutput("plot1", width = "100%"), 
                 plotOutput("plot2", width = "100%")),
        
      
        tabPanel("Pie Chart",
                 selectInput(inputId = "players",
                             label = h3("select a player: "),
                             choice = players  %>% separate(Player, c('first_name', 'last_name'), sep = ' ') %>% arrange(last_name) %>% unite(Player, first_name:last_name, sep=' ') ,                                                  # %>% filter(Tm == input$team),
                             selected = "Lonzo Ball"),
                 
                 column(8,plotOutput("their_stats",height = 900, width = "130%"))),
        
        
        
        tabPanel("Data: Average Stats per Game",tableOutput('data')))
        
        
        ) ))
  
))
 

#----------------------------------------------------------------------------------------------------------------------------------------------------------

server <- function(input, output,session) {
 
  picked_team <- reactive({
    player_stats %>% filter(Tm == input$team) %>% separate(Player, c('first_name', 'last_name'), sep = ' ') %>% arrange(last_name) %>% unite(Player, first_name:last_name, sep=' ') %>% select(Player,Tm,input$xaxis,input$yaxis)
  })
  
  picked_player <- reactive({
    player_stats  %>%     separate(Player, c('first_name', 'last_name'), sep = ' ') %>% arrange(last_name) %>% unite(Player, first_name:last_name, sep=' ')  %>%             filter(Player == input$players) %>% 
      select(AST,STL,BLK,TRB,PTS) 
  })
  
  player_team <- reactive({
    player_stats %>% separate(Player, c('first_name', 'last_name'), sep = ' ') %>% arrange(last_name) %>% unite(Player, first_name:last_name, sep=' ')   %>% filter(Tm == input$team) %>% 
      rename(X = input$xaxis,Y = input$yaxis)
  })
  
  

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
 #Plots and Tables
  
  
  
  #plot that goes with x axis input, purple barplot
  output$plot1 = renderPlot({
    par(mar = c(5, 15, 5, 5))
    par(bg = "#2a3439")
    
    barplot(sort(picked_team()[,3]),names.arg = picked_team()[,1], col = "purple", las =2, horiz=TRUE, col.axis="white", las = 1, main = input$xaxis, col.main="white")})
  
  
  
  #plot that goes with yaxis, blue barplot
  output$plot2 = renderPlot({
    par(mar = c(5, 15, 5, 5))
    par(bg = "#2a3439")
    barplot(sort(picked_team()[,4]),names.arg = picked_team()$Player, col = "blue", las =2, horiz= TRUE, col.axis="white", las =1, main = input$yaxis, col.main = "white")})
  
  
  
  #scatter plot
  output$plot3 <- renderPlotly({
    
      
      plot_ly(data = player_team(), x=~X, y=~Y, color = ~Player)  %>% #added layout jw
        
      layout(paper_bgcolor = "#2a3439", plot_bgcolor="#2a3439", 
               title = "Per Game Stats Plots",
               xaxis = list(title = input$xaxis, color = '#ffffff'), yaxis = list(title = input$yaxis, color = '#ffffff'),
               titlefont = list(color ='#ffffff' ),
               legend = (list(font = list(color='#ffffff')))
               )
    })  
  
  
  #pie chart
  output$their_stats <- renderPlot({
    
      par(bg = "#2a3439")
      pie(as.numeric(picked_player()[1,]), colnames(picked_player()), col.lab = "white", col.axis = '#ffffff', col= c("#74ee15","#ffe700", "#f000ff","#001eff", "#4deeea"),
          main = "The 5 Stat Breakdown: Rebounds, Points, Assists Steals, Blocks \n Average per Game", col.main = "white")#, "#FF8B8B"))
    })
  
  
  
  #data table on left bar
  output$dtab <- DT::renderDataTable({
    picked_team() %>% select(Player,Tm,input$xaxis,input$yaxis) 
  })
  
  
  #entire data tables
  output$data <- renderTable({player_stats})
  
 

}


shinyApp(ui = ui, server = server)

