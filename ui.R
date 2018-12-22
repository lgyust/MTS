# ui.R

library(shiny)
library(pls)
library(xlsx)
library(MASS)

setwd("c:/R")


shinyUI(pageWithSidebar(
   headerPanel(h3(strong("Mahalanobis Taguchi System"))),
    
     sidebarPanel( 
          
          fileInput("file", label = h5("Excel File Input (Sheet1:Normal Group, Sheet2:Abnormal Group)")),
         
          sliderInput("height",
                      "Plot  Height (Y-axis limitS):",
                      min =5,
                      max =50,
                      value = 30),
        
          checkboxGroupInput("colum", "Colum Select:", 
                     choices = c("A"=1,"B"=2,"C"=3,"D"=4,"E"=5,"F"=6,"G"=7,"H"=8,
                                 "I"=9,"J"=10,"K"=11,"L"=12,"M"=13,"N"=14,
                                 "O"=15,"P"=16,"Q"=17,"R"=18,"S"=19,"T"=20,
                                 "U"=21,"V"=22,"W"=23,"X"=24,"Y"=25,"Z"=26), selected = c(1:5)),
           br()
          
       #    img(src = "LG.jpg", height = 230, width = 160)
        
          ),
     
     
    mainPanel(     
      tabsetPanel(
        
        tabPanel("Maharanobis Distance", plotOutput("plot1"),
                 strong(p("Normal phone's MD")),
                 tableOutput("view1"),
                 br(),
                 strong(p("Foreign Object MD")),
                 tableOutput("view2"),
                 br(),
                 verbatimTextOutput("text1")
                 ), 
    
  
        tabPanel("Nornal Group Data", tableOutput('data1') ),
     
        tabPanel("Abnormal Group Data", tableOutput("data2") ) 
        
       ))
       
    ))
 
  

