#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(reshape2)

ui <- fluidPage(
  titlePanel("Investment"),
  
  sidebarLayout(
    fluidRow(
      column(3,
      sliderInput("initialamount",
                  "Initial Amount:",
                  min = 1,
                  max = 100000,
                  value = 1000,
                  step = 500,
                  pre = "$"),
      sliderInput("contribution",
                  "Annual Contribution:",
                  min = 0,
                  max = 50000,
                  value = 2000,
                  step = 500,
                  pre = "$")),
      column(3,
      sliderInput("returnrate",
                  "Return Rate (in %):",
                  min = 0,
                  max = 20,
                  value = 5,
                  step = 1),
      sliderInput("growthrate",
                  "Growth Rate:",
                  min = 0,
                  max = 20,
                  value = 2,
                  step = 1)),
      column(3,
      sliderInput("years",
                  "Years:",
                  min = 0,
                  max = 50,
                  value = 10),
      selectInput("facet",
                  "Facet?",
                  c("No",
                    "Yes")))),
    
    mainPanel(
      h3("Timelines"),
      plotOutput("graph"),
      h3("Balances"),
      verbatimTextOutput("balance")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  alex <- reactive({
    amount <- input$initialamount
    rate <- input$returnrate/100
    year_alex <- input$years
    contrib <- input$contribution
    growth <- input$growthrate/100
    
    future_value <- function(amount,rate,years){
      fv <- amount*(1+rate)^years
      return(fv)
    }
    
    annuity <- function(contrib,rate,years){
      fva <- contrib*(((1+rate)^years-1)/rate)
      return(fva)
    }
    
    growing_annuity <- function(contrib,rate,growth,years){
      fvga <- contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth))
      return(fvga)
    }
    no_contrib <- 0
    fixed_contrib <- 0
    growing_contrib <- 0
    #no contribution
    for (years_m1 in 1:(year_alex+1)){
      no_contrib[years_m1] <- future_value(amount,rate,years = (years_m1-1))
    }
    
    #fixed contribution
    for (years_m2 in 1:(year_alex+1)){
      fixed_contrib[years_m2] <- future_value(amount,rate,years = (years_m2-1))+annuity(contrib,rate,years = (years_m2-1))
      }
    
    #growing contribution
    for (years_m3 in 1:(year_alex+1)){
      growing_contrib[years_m3] <- future_value(amount,rate,years = (years_m3-1))+growing_annuity(contrib,rate,growth,years = (years_m3-1))
      }
    year <- seq(0,year_alex,by=1)
    alex <- data.frame(year,no_contrib,fixed_contrib,growing_contrib)
    
    })
  
   
  
  output$graph <- renderPlot({
    Balances <- alex()
    Balanceslong <- melt(Balances,measure.vars =c("no_contrib","fixed_contrib","growing_contrib"),id.vars="year")
 
    if(input$facet == "No"){
      ggplot(data=Balanceslong,aes(x=year,y=value))+
        geom_line(aes(color =variable))+
        geom_point(aes(color =variable))+
        ggtitle("Three modes of investing")+
        xlab("year")+
        ylab("value")} else if (input$facet == "Yes"){
          ggplot(data=Balanceslong,aes(x=year,y=value))+
            geom_line(aes(color =variable))+
            geom_point(aes(color =variable))+
            ggtitle("Three modes of investing")+
            xlab("year")+
            ylab("value")+facet_wrap(~variable)+geom_area(aes(fill=variable),alpha = 1/2)
      }})
    
  
  output$balance <- renderPrint({
    amount <- input$initialamount
    rate <- input$returnrate/100
    year_alex <- input$years
    contrib <- input$contribution
    growth <- input$growthrate/100
    
    future_value <- function(amount,rate,years){
      fv <- amount*(1+rate)^years
      return(fv)
    }
    
    annuity <- function(contrib,rate,years){
      fva <- contrib*(((1+rate)^years-1)/rate)
      return(fva)
    }
    
    growing_annuity <- function(contrib,rate,growth,years){
      fvga <- contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth))
      return(fvga)
    }
    no_contrib <- 0
    fixed_contrib <- 0
    growing_contrib <- 0
    #no contribution
    for (years_m1 in 1:(year_alex+1)){
      no_contrib[years_m1] <- future_value(amount,rate,years = (years_m1-1))
    }
    
    #fixed contribution
    for (years_m2 in 1:(year_alex+1)){
      fixed_contrib[years_m2] <- future_value(amount,rate,years = (years_m2-1))+annuity(contrib,rate,years = (years_m2-1))
    }
    
    #growing contribution
    for (years_m3 in 1:(year_alex+1)){
      growing_contrib[years_m3] <- future_value(amount,rate,years = (years_m3-1))+growing_annuity(contrib,rate,growth,years = (years_m3-1))
    }
    year <- seq(0,year_alex,by=1)
    data.frame(year,no_contrib,fixed_contrib,growing_contrib)
})  
}


# Run the application 
shinyApp(ui = ui, server = server)
