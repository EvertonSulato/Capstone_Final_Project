library(shiny)
library(shinyWidgets)
library(shinythemes)
library(stringr)


#Load the n-gram databases 
bigram<-readRDS("bigram.RData")
trigram<-readRDS("trigram.RData")
quadgram<-readRDS("quadgram.RData")

ui <- navbarPage("𝐖𝐨𝐫𝐝𝐐𝐮𝐞𝐬𝐭", 
           tabPanel("𝘊𝘰𝘮𝘦 𝘵𝘰 𝘵𝘩𝘪𝘴 𝘢𝘥𝘷𝘦𝘯𝘵𝘶𝘳𝘦 𝘵𝘰 𝘴𝘦𝘦𝘬 𝘵𝘩𝘦 𝘪𝘥𝘦𝘢𝘭 𝘸𝘰𝘳𝘥𝘴!!!",
                    div(style="width:2400px; padding-left:25px;",), fluidPage(
                      theme = shinytheme("united"),
                      tags$head(tags$style(HTML(".navbar-brand {font-size: 40px;}"))),
                      tags$head(tags$style('body {color:white;font:18pt "Arial"}')),
                      tags$head(tags$style('h1 {color:white; font:18pt "Arial"}')),
                      tags$head(tags$style('h2 {color:white; font:14pt "Arial"}')),
                      tags$head(tags$style('h3 {color:white; font:24pt "Arial"}')),
                      tags$head(tags$style(HTML('<style type="text/css"> .row-fluid { width: 50%; }.well { background-color: #5F9EA0;}</style>'))),
                      setBackgroundImage(
                        src = "https://raw.githubusercontent.com/EvertonSulato/Capstone_Final_Project/master/Background_Image.jpg"),
                      headerPanel(
                         h1("This project presents an algorithm model for forecasting next word in the Shiny application, using the SwiftKey database, made available by Coursera during the Data Science Capstone course.")),
                      sidebarLayout(
                        sidebarPanel(
                          width = 5,
                          h1("𝗧𝘆𝗽𝗲 𝘆𝗼𝘂𝗿 𝘁𝗲𝘅𝘁 𝗶𝗻 𝘁𝗵𝗲 𝗯𝗼𝘅 𝘁𝗼 𝗼𝗯𝘁𝗮𝗶𝗻 𝘁𝗵𝗲 𝗽𝗿𝗲𝗱𝗶𝗰𝘁𝗲𝗱 𝘄𝗼𝗿𝗱"),
                          textInput("txt",label=NULL),
                          h1("𝗪𝗼𝗿𝗱 𝗣𝗿𝗲𝗱𝗶𝗰𝘁𝗶𝗼𝗻"),
                          verbatimTextOutput("prediction", placeholder = T)),
                        mainPanel(
                          img(src = "https://github.com/EvertonSulato/Capstone_Final_Project/blob/master/Image_2.png?raw=true", style = "opacity:0.5;width: 50%;"),
                          width = 4,
                          h3(strong("Thanks")),
                          h2("I hope you enjoyed 𝐖𝐨𝐫𝐝𝐐𝐮𝐞𝐬𝐭 App!"),
                          h2("For more detais and the ui and server files used to build this app by Shiny package, visit my Github repository."),
                          hr(),
                          a("GitHub",href="https://github.com/EvertonSulato/Capstone_Final_Project"),
                          hr(),
                          h2("Everton Sulato")))
                        )))
        

PNW <- function(sentence) {
  
  inputtxt <- gsub('[[:punct:]]|[[:digit:]]', "", sentence) ## Remove numbers and punctuations
  inputtxt <- unlist(strsplit(inputtxt, "\\s+")) # split the input string by white spaces
  
  req(inputtxt)
  
  if (length(inputtxt)>= 3) {
  
    outputtxt <- paste(tail(inputtxt, 3), collapse = " ")
    predictedtxt<- quadgram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=quadgram$word, ignore.case = T)]
    guess_word <- word(predictedtxt[1],-1)
    
    if(length(predictedtxt)==0){
      
      outputtxt <- paste(tail(inputtxt, 2), collapse = " ")
      predictedtxt<- trigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=trigram$word, ignore.case = T)]
      guess_word <- word(predictedtxt[1],-1)
      
      if(length(predictedtxt)==0){
        
        outputtxt <- paste(tail(inputtxt, 1), collapse = " ")
        predictedtxt<- bigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=bigram$word, ignore.case = T)]
        guess_word <- word(predictedtxt[1],-1)
        
        if(length(predictedtxt)==0){
          guess_word <-"the"
        }
      }
    }
  } else if (length(inputtxt)==2) {
    
    outputtxt <- paste(tail(inputtxt, 2), collapse = " ")
    predictedtxt<- trigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=trigram$word, ignore.case = T)]
    guess_word <- word(predictedtxt[1],-1)
    
    if(length(predictedtxt)==0){
      
      outputtxt <- paste(tail(inputtxt, 1), collapse = " ")
      predictedtxt<- bigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=bigram$word, ignore.case = T)]
      guess_word <- word(predictedtxt[1],-1)
      
      if(length(predictedtxt)==0){
        guess_word <-"the"
      }
    }
  } else if (length(inputtxt)==1) {
    
    outputtxt <- paste(tail(inputtxt, 1), collapse = " ")
    predictedtxt<- bigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=bigram$word, ignore.case = T)]
    guess_word <- word(predictedtxt[1],-1)
    
    if(length(predictedtxt)==0){
      guess_word <-"the"
    }
  } 
  paste(guess_word)
}

server<-shinyServer(
  
  function(input, output) {
    output$txt <- renderPrint({input$txt})  
    output$prediction <- reactive(paste(input$txt,pred()))
    pred<- reactive({
      PNW(sentence=input$txt) 
    })
  }
)

# Run the application 
shinyApp(ui = ui, server = server)
