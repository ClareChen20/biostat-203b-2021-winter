
library(shiny)
library(tidyverse)
library(ggplot2)


# Load data ----
df <- readRDS("/home/mia.chen1998/biostat-203b-2021-winter/hw3/mimiciv_shiny/data/icu_cohort.rds")

# The ui function ----
ui <- fluidPage(
  
  tabsetPanel(
    
    # First Panel: Introduction and Summary of data ----
    tabPanel("User's Guide",
             titlePanel("Explore the icu_cohort.rds Dataset"),
               mainPanel(htmlOutput("text"),
                         verbatimTextOutput("sum1")
               )
             ),
    
    # Second Panel: Demographic Data & Registered Information
    tabPanel("Demographic & Registration",
             titlePanel("Demographic Data & Registration Information"),
        sidebarLayout(
            sidebarPanel(
                helpText("Select the categorical variable of interest"),
                
                selectInput ("demo_var",
                      label = "Show the distribution of our sample
                          by the chosen demographic catagory",
                      choices = c("Gender", "Anchor age", "Anchor year",
                                       "Insurance", "Language",
                                       "Marital status", "Ethnicity"),
                      selected = "Ethnicity"),
             
                selectInput ("reg_var",
                      label = "Registered Information",
                      choices = c("First careunit", "Last careunit",
                                       "Admission type","Admission location",
                                       "Age at adm", "Admittime", "Dischtime",
                                       "Intime", "Outtime", "Deathtime", 
                                       "Discharge location",
                                       "Edregtime", "Edouttime"),
                      selected = "Discharge location"),
             ),
            mainPanel(htmlOutput("text1"),
              plotOutput("plot2"))
       )
  ),
    
    # Third Panel: Clinical variable with interests ----
    tabPanel("Clinical Measurement",
        titlePanel("Data Explorer for Clinical Measurement"),
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
                        choices = c( "Bicarbonate", "Chloride", "Creatinine", 
                                        "Glucose", "Magnesium", "Potassium", 
                                        "Sodium", "Hematocrit", "Wbc", 
                                        "Lactate", "Heart rate", "Calcium",
                                        "Non-invasive blood pressure systolic",
                                        "Non-invasive blood pressure mean",
                                        "Respiratory rate",
                                        "Temperature fahrenheit",
                                        "Arterial blood pressure systolic",
                                        "Arterial blood pressure mean"),
                        selected = "Bicarbonate")),
      
               mainPanel(
                 # Create sub-tabs within this tab for different functions ----
                 tabsetPanel(type = "tabs",
                           tabPanel("Plot", 
                                    mainPanel(
                                      htmlOutput("text4"),
                                      plotOutput("plot"))
                                    ),
                           tabPanel("Missing Values", 
                                    mainPanel(
                                      htmlOutput("text2"),
                                      verbatimTextOutput("summary")
                           )),
                           tabPanel("Death in 30 Days", 
                                    mainPanel(
                                      htmlOutput("text3"),
                                      plotOutput("plot3")
                           ))
                           )
               )
             )
           )
         )
)


# Define server logic ----
server <- function(input, output) {
  
  output$text <- renderUI({
    str1 <- paste("This app is designed to interactively visualize MIMIC-IV database collected by MIT, 
    which includes real hospital stays for patients admitted to a tertiary academic medical center in Boston, MA, USA. ")
    
    str2 <- paste("       ")
    
    str3 <- paste("All data used by this application are available through [MIMIC IV](https://mimic-iv.mit.edu/docs/datasets/).") 
    
    str4 <- paste("       ")
    
    str5 <- paste("The icu_cohort.rds is an edited version of a combined database, which included all the information for the first ICU stay of each unique adult patient.")
    
    str6 <- paste("       ")
    
    str7 <- paste("Also, an indicator of whether the patient was dead within 30 days has been created and appended into the dataset.")
    
    str8 <- paste("       ")
    
    HTML(paste(str1, str2, str3, str4, str5, str6, str7, str8, sep = '<br/>'))
    
  })
  
  output$sum1 <- renderPrint({
    dataset <- df
    summary(dataset)
  })
    
  output$plot2 <- renderPlot({

    demo_var <- switch(input$demo_var, 
                       "Gender" = df$gender, 
                       "Anchor age" = df$anchor_age, 
                       "Anchor year" = df$anchor_year,
                       "Insurance" = df$insurance, 
                       "Language" = df$language,
                       "Marital status" = df$language, 
                       "Ethnicity" = df$ethnicity)
    
    reg_var <- switch(input$reg_var,
                      "First careunit" = df$first_careunit,
                      "Last careunit" = df$last_careunit,
                      "Admission type" = df$admission_type,
                      "Admission location" = df$admission_location,
                      "Age at adm" = df$age_at_adm, 
                      "Admittime" = df$admittime, 
                      "Dischtime" = df$dischtime,
                      "Intime" = df$intime, 
                      "Outtime" = df$outtime, 
                      "Deathtime" = df$deathtime, 
                      "Discharge location" = df$discharge_location,
                      "Edregtime" = df$edregtime, 
                      "Edouttime" = df$edouttime)
    
    ggplot(data = df) + 
      geom_bar(mapping = aes(x = demo_var, fill = reg_var)) +
      scale_x_discrete(guide = guide_axis(n.dodge=3)) +
      xlab(input$demo_var) +
      ylab("Counts")
  })
  
  output$text1 <- renderUI({
    demo_var <- switch(input$demo_var, 
                       "Gender" = df$gender, 
                       "Anchor age" = df$anchor_age, 
                       "Anchor year" = df$anchor_year,
                       "Insurance" = df$insurance, 
                       "Language" = df$language,
                       "Marital status" = df$language, 
                       "Ethnicity" = df$ethnicity)
    
    reg_var <- switch(input$reg_var,
                      "First careunit" = df$first_careunit,
                      "Last careunit" = df$last_careunit,
                      "Admission type" = df$admission_type,
                      "Admission location" = df$admission_location,
                      "Age at adm" = df$age_at_adm, 
                      "Admittime" = df$admittime, 
                      "Dischtime" = df$dischtime,
                      "Intime" = df$intime, 
                      "Outtime" = df$outtime, 
                      "Deathtime" = df$deathtime, 
                      "Discharge location" = df$discharge_location,
                      "Edregtime" = df$edregtime, 
                      "Edouttime" = df$edouttime)
    
    str1 <- as.character(sum(is.na(demo_var)))
    str2 <- paste("Number of missing values of chosen demographic variable: ")
    str3 <- as.character(sum(is.na(reg_var)))
    str4 <- paste("Number of missing values of chosen registered variable: ")
    str5 <- paste("            ")
    HTML(paste(str2, str1, str5, str4, str3, str5, sep = '<br/>'))
  })
  
  output$plot <- renderPlot({
    
    by_var <- switch(input$by_var, 
                     "Bicarbonate" = df$bicarbonate, 
                     "Calcium" = df$calcium,
                     "Chloride" = df$chloride, 
                     "Creatinine" = df$creatinine,
                     "Glucose" = df$glucose, 
                     "Magnesium" = df$magnesium, 
                     "Potassium" = df$potassium, 
                     "Sodium" = df$sodium, 
                     "Hematocrit" = df$hematocrit, 
                     "Wbc" = df$wbc,
                     "Lactate" = df$lactate, 
                     "Heart rate" = df$heart_rate,
                     "Non-invasive blood pressure systolic" = 
                       df$non_invasive_blood_pressure_systolic,
                     "Non-invasive blood pressure mean" = 
                       df$arterial_blood_pressure_mean,
                     "Respiratory rate" = df$respiratory_rate,
                     "Temperature fahrenheit" = df$temperature_fahrenheit,
                     "Arterial blood pressure systolic" = 
                       df$arterial_blood_pressure_systolic,
                     "Arterial blood pressure mean" = 
                       df$arterial_blood_pressure_mean)
    
    if (input$type == "Box")
    {ggplot(data = df,aes(x = by_var)) +
        geom_boxplot(color = "lightblue")+
        xlab(input$by_var)
    }
    
    else if (input$type == "Hist")
    { hist(by_var, main="Histogram of the chosen variable",
           xlab = input$by_var, ylab = "Counts",
           breaks = 15, col = 'lightblue', border = 'white',
           labels = TRUE
      )
    }
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Generate an HTML table view of the data
  output$text2 <- renderUI({
    by_var <- switch(input$by_var, 
                     "Bicarbonate" = df$bicarbonate, 
                     "Calcium" = df$calcium,
                     "Chloride" = df$chloride, 
                     "Creatinine" = df$creatinine,
                     "Glucose" = df$glucose, 
                     "Magnesium" = df$magnesium, 
                     "Potassium" = df$potassium, 
                     "Sodium" = df$sodium, 
                     "Hematocrit" = df$hematocrit, 
                     "Wbc" = df$wbc,
                     "Lactate" = df$lactate, 
                     "Heart rate" = df$heart_rate,
                     "Non-invasive blood pressure systolic" = 
                       df$non_invasive_blood_pressure_systolic,
                     "Non-invasive blood pressure mean" = 
                       df$arterial_blood_pressure_mean,
                     "Respiratory rate" = df$respiratory_rate,
                     "Temperature fahrenheit" = df$temperature_fahrenheit,
                     "Arterial blood pressure systolic" = 
                       df$arterial_blood_pressure_systolic,
                     "Arterial blood pressure mean" = 
                       df$arterial_blood_pressure_mean)
    
    str1 <- as.character(sum(is.na(by_var)))
    str2 <- paste("Number of missing values of chosen variable: ")
    HTML(paste(str2, str1, sep = '<br/>'))
  })  
  
  output$text3 <- renderText(
    {paste("The replationship between the chosen variable and the 30-day death indicator: ")})
  
  output$plot3 <- renderPlot({
    
    by_var <- switch(input$by_var, 
                     "Bicarbonate" = df$bicarbonate, 
                     "Calcium" = df$calcium,
                     "Chloride" = df$chloride, 
                     "Creatinine" = df$creatinine,
                     "Glucose" = df$glucose, 
                     "Magnesium" = df$magnesium, 
                     "Potassium" = df$potassium, 
                     "Sodium" = df$sodium, 
                     "Hematocrit" = df$hematocrit, 
                     "Wbc" = df$wbc,
                     "Lactate" = df$lactate, 
                     "Heart rate" = df$heart_rate,
                     "Non-invasive blood pressure systolic" = 
                       df$non_invasive_blood_pressure_systolic,
                     "Non-invasive blood pressure mean" = 
                       df$arterial_blood_pressure_mean,
                     "Respiratory rate" = df$respiratory_rate,
                     "Temperature fahrenheit" = df$temperature_fahrenheit,
                     "Arterial blood pressure systolic" = 
                       df$arterial_blood_pressure_systolic,
                     "Arterial blood pressure mean" = 
                       df$arterial_blood_pressure_mean)
    
    if (input$type == "Box")
    {ggplot(df, aes(x = df$deathin30, y = by_var, group = df$deathin30)) + 
        geom_boxplot(aes(fill = df$deathin30)) +
          xlab("Whether the patient died in 30 days") +
          ylab(input$by_var)
    }
    
    else if (input$type == "Hist")
    { ggplot(df, aes(x = by_var)) +
        geom_histogram(aes(color = deathin30, fill = deathin30), 
                       position = "identity", bins = 15, alpha = 0.3) +
        ylim(0, 150) +
        scale_color_manual(values = c("#00AFBB", "#E7B800")) +
        scale_fill_manual(values = c("#00AFBB", "#E7B800")) 
      }
    
  })
  
  output$text4 <- renderText(
    {paste("Essentially, we are interested in any possible correlation between the clinical measurement
           and the 30-day death indicator. So in section, apart from exploring each variable, I also
           created the histograms and bloxplots for each variable within each subgroup that did/did not 
           die in 30 days.")})
  
}

# Call the application ----
shinyApp(ui = ui, server = server)




















