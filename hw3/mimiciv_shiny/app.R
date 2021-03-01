
library(shiny)
library(tidyverse)
library(ggplot2)

# Load data ----
#df <- readRDS("./hw3/mimiciv_shiny/data/icu_cohort.rds")

ui <- fluidPage(
  titlePanel("Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Visualize single continuous variables by boxplot"),
      
      selectInput("by_var", 
                  label = "Choose the continuous variable of interest",
                  choices = c("Bicarbonate", "Calcium", "Chloride"),
                  selected = "Calcium"),
      
      selectInput ("con_var",
                   label = "Show the boxplot by subgroups",
                   choices = c("Admission Type", "First Careunit", 
                               "Anchor Year Group", "Last Careunit"),
                   selected = "Admission Type")
    ),
    
    mainPanel(plotOutput("plot1"),
              plotOutput("plot2"))
  ),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Visualize single Discrete variables by barcharts"),
      
      selectInput ("con_var",
                   label = "Show the boxplot by subgroups",
                   choices = c("Admission Type", "First Careunit", 
                               "Anchor Year Group", "Last Careunit"),
                   selected = "Admission Type"),
     ),
    
    mainPanel(plotOutput("plot3"))
   ),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Visualize the relationship between two variables"),
      
      selectInput("by_var", 
                  label = "Choose the first continuous variable of interest",
                  choices = c("Bicarbonate", "Calcium", "Chloride"),
                  selected = "Calcium"),
      
      selectInput("stat_var", 
                  label = "Choose the second variable of interest",
                  choices = c("Bicarbonate", "Calcium", "Chloride"),
                  selected = "Chloride"),
      
      selectInput ("con_var",
                   label = "Choose the catagorical variable of interest",
                   choices = c("Admission Type", "First Careunit", 
                               "Anchor Year Group", "Last Careunit"),
                   selected = "Admission Type")
    ),
    
    mainPanel(
              plotOutput("plot4"),
              plotOutput("plot5"))
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    by_var <- switch(input$by_var, 
                     "Bicarbonate" = df$bicarbonate,
                     "Calcium" = df$calcium, 
                     "Chloride" = df$chloride)
    
    ggplot(data=df,aes(x = by_var))+
      geom_boxplot()+
      theme_bw()
  })
  
  output$plot2 <- renderPlot({
    by_var <- switch(input$by_var, 
                     "Bicarbonate" = df$bicarbonate,
                     "Calcium" = df$calcium, 
                     "Chloride" = df$chloride)
    
    con_var <- switch(input$con_var, 
                      "Admission Type" = df$admission_type,
                      "First Careunit" = df$first_careunit, 
                      "Last Careunit" = df$last_careunit,
                      "Anchor Year Group" = df$anchor_year_group)
    
    ggplot(data=df,aes(y=by_var,x=con_var, color=con_var))+
      geom_boxplot()+
      theme_bw()+
      coord_flip()
  })
  
  output$plot3 <- renderPlot({
    
    con_var <- switch(input$con_var, 
                      "Admission Type" = df$admission_type,
                      "First Careunit" = df$first_careunit, 
                      "Last Careunit" = df$last_careunit,
                      "Anchor Year Group" = df$anchor_year_group)
    
    ggplot(data = df) + 
      geom_bar(mapping = aes(x = con_var, fill = con_var))
  })
  
  output$plot4 <- renderPlot({
    by_var <- switch(input$by_var, 
                       "Bicarbonate" = df$bicarbonate,
                       "Calcium" = df$calcium, 
                       "Chloride" = df$chloride)
    
    stat_var <- switch(input$stat_var, 
                       "Bicarbonate" = df$bicarbonate,
                       "Calcium" = df$calcium, 
                       "Chloride" = df$chloride)

    ggplot(data=df, aes(x=stat_var,y=by_var))+
      geom_count()+
      theme_bw()
  })
  
  output$plot5 <- renderPlot({
    by_var <- switch(input$by_var, 
                     "Bicarbonate" = df$bicarbonate,
                     "Calcium" = df$calcium, 
                     "Chloride" = df$chloride)
    
    con_var <- switch(input$con_var, 
                      "Admission Type" = df$admission_type,
                      "First Careunit" = df$first_careunit, 
                      "Last Careunit" = df$last_careunit,
                      "Anchor Year Group" = df$anchor_year_group)
    
    ggplot(data=df,aes(x=by_var,color=con_var))+
      geom_density(bw=10,size=1)+
      theme_bw()
  })
}

shinyApp(ui = ui, server = server)




















  