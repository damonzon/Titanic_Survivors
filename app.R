# Kaggle Titanic Survived plots
# Jan 21, 2018
# https://rstudio.github.io/shiny/tutorial/#more-widgets

library(ggplot2)
library(shiny)
data <- read.csv("tidydata.csv")
train <- data[1:891,]
factor_vars <- c("Survived","Pclass","SibSp","Parch","FamSize")
train[factor_vars] <- lapply(train[factor_vars], function(x) as.factor(x))

ui <- shinyUI(fluidPage(
    # headerPanel ("Survival in the Kaggle Titanic Training Data"),
    titlePanel(title=div(img(src="RStudio-Ball.png",width="40px",height="40px"),"Survival in the Kaggle Titanic Training Data")),
    sidebarPanel(
        selectInput("variable", "Choose a Predictor Category:", 
                choices = c("Sex","Pclass","SibSp","Parch","Title","Embarked","FamSize",
                "FamSizeGroups", "TicketGroups","Miss_Cabin","DeckGroups","Miss_Age")),
        h4(helpText("The counter below keeps track of the number of times the app has been accessed.")),
        # br(),
        h4(textOutput("counter"))
       
    ),
    
    mainPanel(
            tabsetPanel(type = "tab"),
            tabPanel("",tags$img(src = "Titanic.jpg", width = "400px",height = "280px")),
            helpText("The 'unsinkable' ship hit an iceberg and sank on April 15, 1912."),
            tabPanel("Plot",plotOutput("plot"))
        
            )
))

server <- shinyServer(function(input, output) {
    
    #surv <- reactive({
    #    train$Survived
    # })
    
   
    output$plot <- renderPlot({
        p <- ggplot(train) + 
            geom_bar(aes_string(x = input$variable, fill = train$Survived), 
                     position = "dodge", color = "black") +
            # geom_bar(aes(x = get(input$variable), fill = train$Survived),  
             #        position = "dodge", color = "black") +
            scale_fill_brewer(type='qual', palette=2) +
            guides(fill=FALSE)+
            ggtitle(paste0("\n\nSurvival by ",input$variable, 
                  " (n = 891)\nLegend: Orange = Survived, Green = Died"))
        print(p)
    })
    
    output$counter <- renderText({
        if(!file.exists("counter.Rdata"))
            counter <- 0
        else
            load(file = "counter.Rdata")
        # counter <- 0
        counter <- counter + 1
        save(counter,file = "counter.Rdata")
        paste0("Hits: ",counter)
    }) 
  
    
    
    
    
})

shinyApp(ui = ui, server = server)

