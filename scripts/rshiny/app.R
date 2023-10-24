library(shiny)
library(shinythemes)
ui <- shinyUI(fluidPage(theme=shinytheme("united"),
                  
                  headerPanel("PubMed Search"),
                  sidebarLayout(
                    sidebarPanel(
                      helpText("Type a word below and search PubMed to find documents that contain that word in the text.
                                 You can even type multiple words. You can search authors, topics, any acronym, etc."),
                      textInput("text", label = h3("Keyord(s)"), value = "carson chow"),
                      helpText("You can specify the start and end dates of your search, use the format YYYY/MM/DD"),
                      textInput("date1", label = h3("From"),value="1990/01/01"),
                      textInput("date2", label = h3("To"),  value = "2015/11/07"),
                      helpText("Now select the output you'd like to see. 
                                 You can see a barplot of articles per year, a wordcloud of the abstract texts, or a table of the top six authors"),
                      actionButton("goButton","PLOT"),
                      actionButton("wordButton","WORDS"),
                      actionButton("authButton","AUTHORS")
                    ),
                    
                    
                    mainPanel(
                      plotOutput("distPlot"),
                      plotOutput("wordPlot"),
                      tableOutput("authList")
                    )
                  )))

# Define server logic required to draw a histogram
library(shiny)
library(SnowballC)
library(qdap)
library(ggplot2)
library(RISmed)
library(wordcloud)

server <- shinyServer(function(input, output) {
  word1<-eventReactive(input$goButton, {input$text})
  
  
  output$distPlot <- renderPlot({
    
    d1<-input$date1
    d2<-input$date2
    
    res <- EUtilsSummary(word1(), type="esearch", db="pubmed", datetype='pdat', mindate=d1, maxdate=d2, retmax=500)
    date()
    fetch <- EUtilsGet(res, type="efetch", db="pubmed")
    count<-table(YearPubmed(fetch))
    count<-as.data.frame(count)
    names(count)<-c("Year", "Counts")
    num <- data.frame(Year=count$Year, Counts=cumsum(count$Counts)) 
    num$g <- "g"
    names(num) <- c("Year", "Counts", "g")
    library(ggplot2)
    q <- qplot(x=Year, y=Counts, data=count, geom="bar", stat="identity")
    q <- q + geom_line(aes(x=Year, y=Counts, group=g), data=num) +
      ggtitle(paste("PubMed articles containing '", word1(), "' ", "= ", max(num$Counts), sep="")) +
      ylab("Number of articles") +
      xlab(paste("Year n Query date: ", Sys.time(), sep="")) +
      labs(colour="") +
      theme_bw()
    q 
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
