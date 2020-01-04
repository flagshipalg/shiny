library(survival)
library(survminer)
require("survival")
library(shiny)

cancer = read.csv("d:/yd/data/lung cancer.csv")
cancer1 = mutate(cancer, MATH_group=ifelse(cancer$MATH>50, "high","low"))
vars <- names(cancer1)


ui <- pageWithSidebar(
  headerPanel("Survival Curve"),
  sidebarPanel(
    selectInput('categrey', 'calsses', vars, selected=vars[21])
  ),
  mainPanel(
    plotOutput('plot1')
  )
  
)


server <- function(input, output){
  data <- reactive({
    cancer1[, input$categrey]
  })
  fit <- reactive({
    survfit(Surv(cancer$TMB.Score, OS_Status)~data(), data=cancer1)
  }
  )
  
  output$plot1 <- renderPlot({
    #plot(fit())
    ggsurvplot(fit(), data = cancer1)
    
    })

  }


shinyApp(ui=ui, server=server)


