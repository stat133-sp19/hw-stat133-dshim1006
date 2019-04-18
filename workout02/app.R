#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

#1 FV
#' @title future value
#' @description calculates the future value
#' @param a numeric integer for present value amount
#' @param r numeric annual rate of return 
#' @param y numeric time in years
#' @return computed future value
future_value <- function(a, r, y) {
  return(a * ((1 + r) ^ y))
}

#2 FVA
#' @title annuity
#' @description calculates the annuity
#' @param c numeric contribution (how much you deposit at the end of the year)
#' @param r numeric annual rate of return 
#' @param y numeric time in years
#' @return computed annuity
annuity <- function(c, r, y) {
  return( c * (((1 + r)^y - 1) / r))
}

#3 FVGA
#' @title future value of growing annuity
#' @description calculates the future value of growing annuity
#' @param c numeric first contribution
#' @param r numeric annual rate of return 
#' @param g numeric growth rate
#' @param y numeric time in years
#' @return computed future value of growing annuity
growing_annuity <- function(c, r, g, y) {
  return(c *(((1 + r) ^y - (1 + g)^y) / (r - g)))
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("warmup02-Jennifer-Shim"),
   
   # Sidebar with a slider input for number of bins 
   fluidPage(
     fluidRow(
       column(4, sliderInput("init_a",
                             "Initial Amount",
                             min = 0,
                             max = 100000,
                             value = 1000,
                             step = 500,
                             pre = "$"),
              sliderInput("annual_cont",
                          "Annual Contribution",
                          min = 0,
                          max = 50000,
                          value = 2000,
                          step = 500,
                          pre = "$")),
       column(4, sliderInput("r_rate",
                             "Return Rate (in %)",
                             min = 0,
                             max = 20,
                             value = 5,
                             step = 0.1),
              sliderInput("g_rate",
                          "Growth Rate (in %)",
                          min = 0,
                          max = 20,
                          value = 2,
                          step = 0.1)),
       column(4, sliderInput("year",
                             "Years",
                             min = 0,
                             max = 50,
                             value = 20,
                             step = 1),
              selectInput("facet", 
                          "Facet?",
                          choice = c("No", "Yes")))
     )
   ),
   mainPanel(
     hr(),
     h4("Timelines"),
     width = 12, 
     plotOutput("distPlot"),
     h4("Balances"),
     verbatimTextOutput("summary")
   )
      # Show a plot of the generated distribution
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   df1 <- reactive({
     
     # Modalities
     fv <- c()
     fva <- c()
     fvga <- c()
     for (i in 0:input$year) {
       fv0 <- future_value(input$init_a,input$r_rate * 0.01,  i)
       fva0 <- fv0 + annuity( input$annual_cont, input$r_rate * 0.01,  i)
       fvga0 <- fv0 + growing_annuity(input$annual_cont, input$r_rate * 0.01, input$g_rate * 0.01, i)
       fv <- append(fv, fv0)
       fva <- append(fva, fva0)
       fvga <- append(fvga, fvga0)
     }
     modalities <- data.frame(
       year = 0:input$year,
       no_contrib = fv,
       fixed_contrib = fva,
       growing_contrib = fvga
     )
     return(modalities)
     })
   
   output$distPlot <- renderPlot ({
     label <- c("no_contrib", "fixed_contrib", "growing_contrib")
     x <- factor(1:3, labels = label)
     year = df1()$year
     
     df2 <- data.frame(
       year = rep(year, 3),
       balance = c(df1()$no_contrib, df1()$fixed_contrib, df1()$growing_contrib),
       variable = rep(x, each = length(year)),
       levels = label
     )
     
     if (input$facet == "No") {
       ggplot(data = df2, aes(x = year, y = balance, group = variable)) +
         geom_point(aes(color = variable)) + geom_path(aes(color = variable)) +
         xlab("Year") +
         ylab("Value") +
         ggtitle("Three modes of investing")
     } else {
       ggplot(data = df2, aes(x = year, y = balance, group = variable)) +
         geom_point(aes(color = variable)) + geom_path(aes(color = variable)) + geom_area(aes(fill = variable), alpha = 0.4) +
         xlab("Year") +
         ylab("Value") +
         ggtitle("Three modes of investing") +
         facet_grid(~ variable) + 
         theme_bw()
     }
    })
   output$summary <- renderPrint({
     df1()
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

