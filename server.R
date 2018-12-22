# server.R

library(shiny)
library(ggplot2)
library(XML)
library(MASS)

setwd("c:/R")

#w <- read.xlsx("cell4.xlsx",1,header=F)
#w <- matrix(unlist(w), ncol = ncol(w))

#z <- read.xlsx("cell4.xlsx",2,header=F)
#z <- matrix(unlist(z), ncol = ncol(z))

shinyServer(
     
  function(input, output) {
   
    ww <- reactive({
      inFile <- input$file
      if (is.null(inFile)) return(NULL)
      w <- read.xlsx(inFile$datapath,1,header=F)
      w <- matrix(unlist(w), ncol = ncol(w))
      w
    })
     
     zz <- reactive({
       inFile <- input$file
       if (is.null(inFile)) return(NULL)
       z <- read.xlsx(inFile$datapath,2,header=F)
       z <- matrix(unlist(z), ncol = ncol(z))
     })
     
     output$plot1 <- renderPlot({
      
      w0 <- ww()[ , c(as.numeric(input$colum)) ]
      w0 <- matrix(w0, ncol = length(input$colum))
      w1 <- stdize(w0)%*%ginv(cor(w0))%*%t(stdize(w0))/ncol(w0)
      w2 <- diag(nrow(w1))*w1 
      w3 <- apply(w2, 2, sum)
      plot(w3,type="o",xlim=c(1,35),ylim=c(0,input$height),xlab="case number", 
           ylab=" Maharanobis Distance", col="blue")
      
      zz <- zz()[, c(as.numeric(input$colum)) ]
      zz <- matrix(zz, ncol = length(input$colum))
      z0 <- predict(stdize(w0),zz)
      z1 <- z0%*%ginv(cor(w0))%*%t(z0)/ncol(z0)
      z2 <- diag(nrow(z1))*z1
      z3 <- apply(z2, 2, sum)
      points(z3,pch="+",col="red",type="o",lty=2)
      
      })
     
     output$view1 <- renderTable({
       w0 <- ww()[ ,as.numeric(input$colum) ]
       w0 <- matrix(w0, ncol = length(input$colum))
       w1 <- stdize(w0)%*%ginv(cor(w0))%*%t(stdize(w0))/ncol(w0)
       w2 <- diag(nrow(w1))*w1 
       w3 <- apply(w2, 1, sum)
       t(w3) 
       })
    
     output$view2 <- renderTable({
         
         w0 <- ww()[ , as.numeric(input$colum) ]
         w0 <- matrix(w0, ncol = length(input$colum))
         
         zz <- zz()[ , as.numeric(input$colum) ]
         zz <- matrix(zz, ncol = length(input$colum))
         
         z0 <- predict(stdize(w0),zz)
         z1 <- z0%*%ginv(cor(w0))%*%t(z0)/ncol(z0)
     
         z2 <- diag(nrow(z1))*z1
         z3 <- apply(z2, 2, sum)
         t(z3)
     })
     
    
     output$text1 <-renderText({
       
       w0 <- ww()[ , c(as.numeric(input$colum)) ]
       w0 <- matrix(w0, ncol = length(input$colum))
       w1 <- stdize(w0)%*%ginv(cor(w0))%*%t(stdize(w0))/ncol(w0)
       w2 <- diag(nrow(w1))*w1 
       w3 <- apply(w2, 2, sum)
       
       zz <- zz()[, c(as.numeric(input$colum)) ]
       zz <- matrix(zz, ncol = length(input$colum))
       z0 <- predict(stdize(w0),zz)
       z1 <- z0%*%ginv(cor(w0))%*%t(z0)/ncol(z0)
       z2 <- diag(nrow(z1))*z1
       z3 <- apply(z2, 2, sum)
       
       paste("Failed to distinguish FO Case:", length(z3[max(w3)>= z3]) , "EA" )
   
       })
   
     
     output$data1 <- renderTable({
       data1 <- ww()
       colnames( data1 )=c(LETTERS[1:ncol( data1 )] )
       data1
       })
     
     output$data2 <- renderTable({
       data2 <- zz()
       colnames(data2)=c(LETTERS[1:ncol(data2)])
       data2
       })
    
  })