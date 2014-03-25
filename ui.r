library(shiny)
library(ggplot2)

shinyUI(pageWithSidebar(
  headerPanel("Black-Scholes Greeks"),
  
  sidebarPanel(
    numericInput("time","Time of interest (t):",0 ,min=0),
    numericInput("maturity","Time to maturity (T):",1,min=0),
    numericInput("strike","Strike price of option:", 50,min=0),
    numericInput("r", "Cont. risk-free rate (in %):", 4),
    numericInput("dividend", "Cont. dividend yield (in %):", 0),
    numericInput("volatility", "Volatility:", 1,min=0)
    ),
  

  
  mainPanel(
    tabsetPanel(
      tabPanel("Option price", plotOutput("price")),
      tabPanel("Option delta", plotOutput("delta")),
      tabPanel("Option gamma", plotOutput("gamma")),
      tabPanel("Option vega", plotOutput("vega")),
      tabPanel("Option rho", plotOutput("rho"))
      )
    )
  
  )
  )
