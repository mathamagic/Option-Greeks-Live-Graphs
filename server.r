library(shiny)
library(ggplot2)
library(gridExtra)

shinyServer(function(input,output){
  #calculate binary options
  
    CoNCall<-function(stock){
      dt<-input$maturity-input$time
      r<-input$r/100
      div<-input$dividend/100
      
      d1<-(log(stock/input$strike)+(r-div+0.5*input$volatility^2)*dt)/(input$volatility*dt^0.5)
    d2<-d1-input$volatility*dt^0.5
    return(exp(-div*dt)*pnorm(d2))
  }

CoNPut<-function(stock){
  dt<-input$maturity-input$time
  r<-input$r/100
  div<-input$dividend/100
  
  d1<-(log(stock/input$strike)+(r-div+0.5*input$volatility^2)*dt)/(input$volatility*dt^0.5)
  d2<-d1-input$volatility*dt^0.5
  return(exp(-r*dt)*pnorm(-d2))
}


AoNCall<-function(stock){
  dt<-input$maturity-input$time
  r<-input$r/100
  div<-input$dividend/100
  
  d1<-(log(stock/input$strike)+(r-div+0.5*input$volatility^2)*dt)/(input$volatility*dt^0.5)
  d2<-d1-input$volatility*dt^0.5
  return(exp(-div*dt)*pnorm(d1))
}

# 
AoNPut<-function(stock){
  dt<-input$maturity-input$time
  r<-input$r/100
  div<-input$dividend/100
    
  d1<-(log(stock/input$strike)+(r-div+0.5*input$volatility^2)*dt)/(input$volatility*dt^0.5)
  d2<-d1-input$volatility*dt^0.5
  return(exp(-div*dt)*pnorm(-d1))
}


callprice<-function(stock){
  return(stock*AoNCall(stock)-input$strike*CoNCall(stock))
}

putprice<-function(stock){
  return(-1*stock*AoNPut(stock)+input$strike*CoNPut(stock))
}

greekgamma<-function(stock){
  dt<-input$maturity-input$time
  r<-input$r/100
  div<-input$dividend/100
  
  d1<-(log(stock/input$strike)+(r-div+0.5*input$volatility^2)*dt)/(input$volatility*dt^0.5)
  return((stock*input$volatility*dt^0.5)^(-1)*exp(-div*dt)*dnorm(d1))
}

greekvega<-function(stock){
  dt<-input$maturity-input$time
  r<-input$r/100
  div<-input$dividend/100
  
  d1<-(log(stock/input$strike)+(r-div+0.5*input$volatility^2)*dt)/(input$volatility*dt^0.5)
  return((stock*dt^0.5)*exp(-div*dt)*dnorm(d1))
}

callrho<-function(stock){
  dt<-input$maturity-input$time
  r<-input$r/100
  div<-input$dividend/100
  
  d1<-(log(stock/input$strike)+(r-div+0.5*input$volatility^2)*dt)/(input$volatility*dt^0.5)
  d2<-d1-input$volatility*dt^0.5
  return(input$strike*dt*exp(-r*dt)*pnorm(d2))
}

putrho<-function(stock){
  dt<-input$maturity-input$time
  r<-input$r/100
  div<-input$dividend/100
  
  d1<-(log(stock/input$strike)+(r-div+0.5*input$volatility^2)*dt)/(input$volatility*dt^0.5)
  d2<-d1-input$volatility*dt^0.5
  return(input$strike*dt*exp(-r*dt)*pnorm(-1*d2))
}


output$price <-renderPlot({
  p<-ggplot(data.frame(x = c(1, 150)), aes(x))+ylab("call premium")+ xlab("stock price") + stat_function(fun = callprice, size=1)
  q<-ggplot(data.frame(x = c(1, 150)), aes(x))+ylab("put premium")+ xlab("stock price") + stat_function(fun = putprice, size=1)
  grid.arrange(p,q,ncol=2)
  
})

output$delta <-renderPlot({
  p<-ggplot(data.frame(x = c(1, 150)), aes(x))+ylab("call delta")+ xlab("stock price") + stat_function(fun = AoNCall, size=1)
  q<-ggplot(data.frame(x = c(1, 150)), aes(x))+ylab("put delta")+ xlab("stock price") + stat_function(fun = AoNPut, size=1)
  grid.arrange(p,q,ncol=2)
})

output$gamma <-renderPlot({
  p<-ggplot(data.frame(x = c(1, 150)), aes(x))+ylab("call and put gamma")+ xlab("stock price") + stat_function(fun = greekgamma, size=1)
  print(p)
})

output$vega <-renderPlot({
  p<-ggplot(data.frame(x = c(1, 150)), aes(x))+ylab("call and put vega")+ xlab("stock price") + stat_function(fun = greekvega, size=1)
  print(p)
})

output$rho <-renderPlot({
  p<-ggplot(data.frame(x = c(1, 150)), aes(x))+ylab("call rho")+ xlab("stock price") + stat_function(fun = callrho, size=1)
  q<-ggplot(data.frame(x = c(1, 150)), aes(x))+ylab("put rho")+ xlab("stock price") + stat_function(fun = putrho, size=1)
  grid.arrange(p,q,ncol=2)
})

})
