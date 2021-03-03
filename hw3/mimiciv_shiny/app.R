
library(shiny)
library(tidyverse)
library(ggplot2)


# Load data ----
df <- readRDS("/home/mia.chen1998/biostat-203b-2021-winter/hw3/mimiciv_shiny/data/icu_cohort.rds")

ui <- fluidPage(
  
  tabsetPanel(
  
    tabPanel("Demographic Data & Registered Information",
        sidebarLayout(
            sidebarPanel(
                helpText("Select the categorical variable of interest"),
             
                selectInput ("demo_var",
                      label = "Show the distribution of our sample
                          by the chosen demographic catagory",
                      choices = c( "gender", "anchor_age", "anchor_year",
                                       "insurance", "language",
                                       "marital_status", "ethnicity"),
                      selected = "ethnicity"),
             
                selectInput ("reg_var",
                      label = "Registered Information",
                      choices = c( "first_careunit", "last_careunit",
                                       "admission_type","admission_location",
                                       "age_at_adm", "admittime", "dischtime",
                                       "intime", "outtime", "deathtime", 
                                       "discharge_location",
                                       "edregtime", "edouttime"),
                      selected = "first_careunit"),
             ),
            mainPanel(plotOutput("plot2"))
       )
  ),
  
    tabPanel("Data Explorer for Clinical Measurement",
        sidebarLayout(
              sidebarPanel(
               # Choose the type of plots you want
              radioButtons("type", "Plot type:",
                            c("Histogram" = "Hist",
                              "Boxplot" = "Box"
                              )),
               
               # Choose the numerical input
              selectInput("by_var",
                        label = "Lab Value",
                        choices = c( "bicarbonate", "chloride", "creatinine", 
                                        "glucose", "magnesium", "potassium", 
                                        "sodium", "hematocrit", "wbc", 
                                        "lactate", "heart_rate", "calcium",
                                        "non_invasive_blood_pressure_systolic",
                                        "non_invasive_blood_pressure_mean",
                                        "respiratory_rate",
                                        "temperature_fahrenheit",
                                        "arterial_blood_pressure_systolic",
                                        "arterial_blood_pressure_mean"),
                        selected = "bicarbonate")),
             # Output 
          mainPanel(
               # Output: Tabset w/ plot, summary, and table ----
               tabsetPanel(type = "tabs",
                           tabPanel("Plot", plotOutput("plot")),
                           tabPanel("Summary", verbatimTextOutput("summary")),
                           tabPanel("Table", tableOutput("table"))
               )
             )
           )
         )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$plot2 <- renderPlot({

    demo_var <- switch(input$demo_var, 
                       "gender" = df$gender, 
                       "anchor_age" = df$anchor_age, 
                       "anchor_year" = df$anchor_year,
                       "insurance" = df$insurance, 
                       "language" = df$language,
                       "marital_status" = df$language, 
                       "ethnicity" = df$ethnicity)
    
    reg_var <- switch(input$reg_var,
                      "first_careunit" = df$first_careunit,
                      "last_careunit" = df$last_careunit,
                      "admission_type" = df$admission_type,
                      "admission_location" = df$admission_location,
                      "age_at_adm" = df$age_at_adm, 
                      "admittime" = df$admittime, 
                      "dischtime" = df$dischtime,
                      "intime" = df$intime, 
                      "outtime" = df$outtime, 
                      "deathtime" = df$deathtime, 
                      "discharge_location" = df$discharge_location,
                      "edregtime" = df$edregtime, 
                      "edouttime" = df$edouttime)
    
    ggplot(data = df) + 
      geom_bar(mapping = aes(x = demo_var, fill = reg_var)) +
      scale_x_discrete(guide = guide_axis(n.dodge=3))
  })
  
  output$plot <- renderPlot({
    
    by_var <- switch(input$by_var, 
                     "bicarbonate" = df$bicarbonate, 
                     "calcium" = df$calcium,
                     "chloride" = df$chloride, 
                     "creatinine" = df$creatinine,
                     "glucose" = df$glucose, 
                     "magnesium" = df$magnesium, 
                     "potassium" = df$potassium, 
                     "sodium" = df$sodium, 
                     "hematocrit" = df$hematocrit, 
                     "wbc" = df$wbc,
                     "lactate" = df$lactate, 
                     "heart_rate" = df$heart_rate,
                     "non_invasive_blood_pressure_systolic" = 
                       df$non_invasive_blood_pressure_systolic,
                     "non_invasive_blood_pressure_mean" = 
                       df$arterial_blood_pressure_mean,
                     "respiratory_rate" = df$respiratory_rate,
                     "temperature_fahrenheit" = df$temperature_fahrenheit,
                     "arterial_blood_pressure_systolic" = 
                       df$arterial_blood_pressure_systolic,
                     "arterial_blood_pressure_mean" = 
                       df$arterial_blood_pressure_mean)
    
    if (input$type == "Box")
    {ggplot(data = df,aes(x = by_var)) +
        geom_boxplot()+
        theme_bw()
    }
    
    else if (input$type == "Hist")
    {ggplot(data = df,mapping = aes(x = by_var)) +
        geom_bar(stat = 'count', color = "lightblue") +
        geom_text(stat = 'count', aes(label=..count..)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({ head( df, n = 10 )},  
                            spacing = 'xs')  
}

shinyApp(ui = ui, server = server)




















