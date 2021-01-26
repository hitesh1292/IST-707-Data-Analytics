#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd( "C:\\Users\\hites\\Desktop\\HW_01\\")

library(shiny)
library(arules) 

library(arulesViz)
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("Association Rule Mining Employee Attrition"),
    
    sidebarLayout(
        
        sidebarPanel(
            
            sliderInput(inputId =  "min_support",
                        
                        "Support",
                        
                        min = 0,
                        
                        max = 1,
                        
                        value = 0.03,step = 0.01),
            
            sliderInput("min_confidence",
                        
                        "Confidence",
                        
                        min = 0,
                        
                        max = 1,
                        
                        value = 0.4,step = 0.1),
            
            sliderInput("minlen_value",
                        
                        "Minimum Length of the Rule",
                        
                        min = 3,
                        
                        max = 10,
                        
                        value = 3,step = 1),		  
            
            sliderInput("num_top_rules",
                        
                        "Number of Top Rules to show Attrition",
                        
                        min = 4,
                        
                        max = 15,
                        
                        value = 5, step = 1),
            
            selectInput(inputId = "sort_by_param",
                        
                        "Sorting Association Rules in descending order by Lift or Confidence or Support as the parameter",
                        
                        choices = c('lift','confidence','support'),
                        
                        selected = 'lift'
                        
            )
            
        ),
        
        mainPanel(
            
            tabsetPanel(id = 'rules',
                        
                        tabPanel('Attrition = Yes',value = 'table',verbatimTextOutput("rulesTableYes"),plotOutput("graphPlotYes", width='100%', height='100%')),
                        
                        tabPanel('Attrition = No',value = 'table',verbatimTextOutput("rulesTableNo"),plotOutput('graphPlotNo',width = '100%',height = '100%'))
                        
                        
                        
                        
                        
            )
            
        )
        
    )
    
)


# Define server logic required to draw a histogram
server <- function(input,output) {
    
    
    
    output$rulesTableYes <- renderPrint({
        
        
        
        attr_yes <- apriori(emp_attr_trans, parameter = list(support = as.numeric(input$min_support), confidence = as.numeric(input$min_confidence), minlen = as.numeric(input$minlen_value)),
                            
                            appearance = list(default = "lhs", rhs=("Attrition=Yes")),control=list(verbose = FALSE))
        
        
        
        inspect(head(sort (attr_yes, by=input$sort_by_param, decreasing=TRUE),input$num_top_rules))
        
        
        
    })
    
    
    
    output$rulesTableNo <- renderPrint({
        
        
        
        attr_no <- apriori(emp_attr_trans, parameter = list(support = as.numeric(input$min_support), confidence = as.numeric(input$min_confidence),minlen = as.numeric(input$minlen_value)),
                           
                           appearance = list(default = "lhs", rhs=("Attrition=No")),control=list(verbose = FALSE))
        
        inspect(head(sort (attr_no, by=input$sort_by_param, decreasing=TRUE),input$num_top_rules))
        
        
        
    })
    
    
    
    output$graphPlotYes <- renderPlot({
        
        attr_yes <- apriori(emp_attr_trans, parameter = list(support = as.numeric(input$min_support), confidence = as.numeric(input$min_confidence),minlen = as.numeric(input$minlen_value)),
                            
                            appearance = list(default = "lhs", rhs=("Attrition=Yes")))
        
        plot(head(attr_yes,n = input$num_top_rules,by=input$sort_by_param), method="grouped")
        
    }, height=1000, width=1000)
    
    
    
    output$graphPlotNo <- renderPlot({
        
        attr_no <- apriori(emp_attr_trans, parameter = list(support = as.numeric(input$min_support), confidence = as.numeric(input$min_confidence),minlen = as.numeric(input$minlen_value)),
                           
                           appearance = list(default = "lhs", rhs=("Attrition=No")))
        
        
        
        plot(head(attr_no,n = input$num_top_rules,by=input$sort_by_param), method="grouped")
        
    }, height=1000, width=1000)
    
    
    
}

arm_df <- read.csv("df_attrition_shiny.csv")


emp_attr_trans <- as(arm_df,"transactions")

# Run the application 
shinyApp(ui = ui, server = server)
