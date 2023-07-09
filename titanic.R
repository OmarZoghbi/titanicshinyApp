library(shiny)
library(ggplot2)

# Daten aus der CSV-Datei laden
data <- read.csv('/Users/User/Documents/App-1/titanic_data.csv') 

# UI
ui <- fluidPage(
  
  # App Titel
  titlePanel("Survival Chance of Passengers in Titanic"),
  
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Diagrammtyp",
                  label = "Diagrammtyp:",
                  choices = c("Balkendiagramm",
                              "Streudiagramm",
                              "Kreisdiagramm"
                  )),
      
      
      radioButtons(inputId = "Survived",
                   label = "Survived:",
                   choices = c(
                     "Yes" = "1",
                     "No" = "0"
                     
                   )),
      checkboxGroupInput(inputId = "Pclass",
                         label = "Pclass:",
                         choices = c(
                           "1" = "1",
                           "2" = "2",
                           "3" = "3"
                         )),
      
      conditionalPanel(
        condition = "input.Diagrammtyp != 'Kreisdiagramm'",
        
        checkboxGroupInput(inputId = "sex",
                           label = "sex:",
                           choices = c(
                             "Female" = "female",
                             "Male" = "male"
                           )),
        
      ),
      
      sliderInput("Age",
                  "Age:",
                  min = 1,
                  max = 100,
                  value = 100
      ), 
      verbatimTextOutput("text")),
    
    
    
    
    # Main Panel
    mainPanel(
      plotOutput(outputId = "SurvivedPlot"),
      dataTableOutput("titanic_table"),
      
    )
  )
)

# Server
server <- function(input, output, session) {
  output$titanic_table <- renderDataTable({
    data
  })
  
  # Nutze "data" in der Shiny App
  output$SurvivedPlot <- renderPlot({
    filteredData <- subset(data,
                           Survived == input$Survived &
                             (Sex %in% input$sex | is.null(input$sex) | length(input$sex) == 0) &
                             Age <= input$Age &
                             (Pclass %in% input$Pclass | is.null(input$Pclass) | length(input$Pclass) == 0))
    
    if (input$Diagrammtyp == "Balkendiagramm") {
      if (length(input$sex) == 0 || length(input$sex) > 1) {
        # Balkendiagramm für beide Geschlechter erstellen
        barplot(table(filteredData$Age), col = c("pink", "steelblue"),
                xlab = "Age", ylab = "Frequency",
                main = "Histogram of Age")
      } else if (input$sex == "male" && length(input$sex) == 1) {
        # Balkendiagramm nur für männliches Geschlecht erstellen
        maleData <- filteredData[filteredData$Sex == "male", ]
        barplot(table(maleData$Age), col = "steelblue",
                xlab = "Age", ylab = "Frequency",
                main = "Histogram of Age (Male)")
      } else if (input$sex == "female" && length(input$sex) == 1) {
        # Balkendiagramm nur für weibliches Geschlecht erstellen
        femaleData <- filteredData[filteredData$Sex == "female", ]
        barplot(table(femaleData$Age), col = "pink",
                xlab = "Age", ylab = "Frequency",
                main = "Histogram of Age (Female)")
      }
    } 
    else if (input$Diagrammtyp == "Streudiagramm") {
      # Zähle die Anzahl der Passagiere pro Alter
      ageCounts <- table(filteredData$Age)
      passengerCounts <- sapply(names(ageCounts), function(age) sum(filteredData$Age == age))
      
      # Erzeuge eine Datenrahmen mit Alterswerten und Passagieranzahlen
      scatterData <- data.frame(Age = as.numeric(names(ageCounts)),
                                PassengerCount = passengerCounts)
      
      plot(scatterData$Age, scatterData$PassengerCount,
           xlab = "Age", ylab = "Number of Passengers",
           main = "Scatter Plot of Age and Number of Passengers",
           pch = 16)
    }
    
    else if (input$Diagrammtyp == "Kreisdiagramm") {
      # Auswahl des Geschlechts zurücksetzen
      updateCheckboxGroupInput(session, "sex", selected = character(0))
      
      sexCounts <- table(filteredData$Sex)
      pie(sexCounts, labels = c("Female", "Male" ),
          col = c("pink", "lightblue"),
          main = "Pie Chart of Sex")
    }
  })
  output$text<- renderText({
    "Quellen : 
    https://towardsdatascience.com/end-to-end-shiny-app-tutorial-using-nyc-mortality-data-d29ad99506b9 " 
  })
}

# Run the application
shinyApp(ui = ui, server = server)