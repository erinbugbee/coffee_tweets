source("check_packages.R")
check_packages(c("shiny", "shinythemes", "ggplot2", "devtools", "twitteR", "base64enc", "httr", "DT", "rmarkdown", "wordcloud2"))
install_github("PHP-2560/final-project-coffee/coffee")
library(coffee)

ui <- navbarPage(
  theme = shinytheme("cosmo"),
  collapsible = FALSE,
  fluid = TRUE,
  "Sentiment Analysis of Coffee Tweets",
  id ="main", 
  tabPanel("About", includeMarkdown("Markdown.Rmd")
  ),
  tabPanel("Table",
           sidebarLayout(
             sidebarPanel(
               conditionalPanel(
                 'input.dataset == "coffee"',
                 helpText('Tweets including "coffee"')
               ),
               conditionalPanel(
                 'input.dataset == "starbucks"',
                 helpText('Tweets including "starbucks"')
               ),
               conditionalPanel(
                 'input.dataset == "dunkin"',
                 helpText('Tweets including "dunkin"')
               ),
               conditionalPanel(
                 'input.dataset == "shiru"',
                 helpText('Tweets including "shiru"')
               )
             ),
             mainPanel(
               #Tabs for data tables of original tweets
               tabsetPanel(
                 id = 'dataset',
                 tabPanel("coffee", DT::dataTableOutput("table1")),
                 tabPanel("starbucks", DT::dataTableOutput("table2")),
                 tabPanel("dunkin", DT::dataTableOutput("table3")),
                 tabPanel("shiru", DT::dataTableOutput("table4"))
               )
             )
           )
  ),
  tabPanel("Graphs",  tabsetPanel(
    tabPanel("Word Cloud",
             fluidPage(
               titlePanel("Word Cloud"),
               sidebarLayout(
                 sidebarPanel(
                   helpText('You will begin by selecting one of the words listed below. Our app will find the top 500 tweets capturing that specific word. The Word Cloud displays the most frequent words occurring within those specific tweets. You can also choose how many words you would like shown in the word cloud.'),
                   hr(),
                   selectInput("selection", "Choose a Word:", choices = c("coffee","starbucks", "dunkin", "shiru")),
                   # Sidebar with a slider and selection inputs
                   sliderInput("num",
                               "Number of Words:",
                               min = 0,  max = 100,  value = 50)
                 ),
                 #Show Word Cloud
                 mainPanel(
                   wordcloud2Output("plot1")
                 )
               )
             )
    ),
    tabPanel("Bar Graph",
             fluidPage(
               #Application title
               titlePanel("Bar Graph"),
               sidebarLayout(
                 #Sidebar with a slide and selection inputs
                 sidebarPanel(
                   helpText('You will begin by selecting one of the words listed below. Our app will find the top 500 tweets capturing that specific word. The Bar Graph displays the number of words that are positive and negative for the tweets found capturing the word.'),
                   hr(),
                   selectInput("word", "Choose a Word:", choices = c("coffee","starbucks", "dunkin", "shiru"))
                 ),
                 # Show Bar Plot
                 mainPanel (
                   plotOutput("plot2")
                 )
               )
             )
    ),
    tabPanel("Emotions Category Graph",
             fluidPage(
               #Application title
               titlePanel("Emotions Graph"),
               sidebarLayout(
                 #Sidebar with a slide and selection inputs
                 sidebarPanel(
                   helpText('You will begin by selecting one of the words listed below. Our app will find the top 500 tweets capturing that specific word. The Emotions Sentiment Graph displays the top sentiments expressed by the tweets, and the frequency of the words that contribute most to each sentiment.'),
                   hr(),
                   selectInput("item", "Choose a Word:", choices = c("coffee","starbucks", "dunkin", "shiru"))
                   
                 ),
                 # Show Emotions Plot
                 mainPanel (
                   plotOutput("plot3")
                 )
               )
             )
    )
  )
  )
)

server <- function(input, output, session){
  
  reactive({
    input$num
    input$selection
    input$word
    inout$item
  })
  
  #Pre load the tweets for "coffee"
  get_tweets("coffee")
  clean_tweets("coffee_tweets.csv")
  
  #Pre load the tweets for "starbucks"
  get_tweets("starbucks")
  clean_tweets("starbucks_tweets.csv")
  
  #Pre load the tweets for "dunkin"
  get_tweets("dunkin")
  clean_tweets("dunkin_tweets.csv")
  
  #Pre load the tweets for "shiru"
  get_tweets("shiru")
  clean_tweets("shiru_tweets.csv")
  
  #Load the tweets in data tables
  output$table1 <- DT::renderDataTable({
    DT::datatable(read.csv("coffee_tweets.csv")[2:2])
  })
  output$table2 <- DT::renderDataTable({
    DT::datatable(read.csv("starbucks_tweets.csv")[2:2])
  })
  output$table3 <- DT::renderDataTable({
    DT::datatable(read.csv("dunkin_tweets.csv")[2:2])
  })
  output$table4 <- DT::renderDataTable({
    DT::datatable(read.csv("shiru_tweets.csv")[2:2])
  })
  
  #Create the word clouds
  output$plot1 <- renderWordcloud2({
    if (input$selection == "coffee") {
      make_wordcloud("clean_coffee_tweets.csv", input$num)
    }
    else if (input$selection == "starbucks") {
      make_wordcloud("clean_starbucks_tweets.csv", input$num)
    }
    else if (input$selection == "dunkin") {
      make_wordcloud("clean_dunkin_tweets.csv", input$num)
    }
    else if (input$selection == "shiru") {
      make_wordcloud("clean_shiru_tweets.csv", input$num)
    }
  })
  
  #Create the bar graphs
  output$plot2 <- renderPlot({
    if (input$word == "coffee"){
      make_bar_graph("clean_coffee_tweets.csv")
    } else if (input$word == "starbucks") {
      make_bar_graph("clean_starbucks_tweets.csv")
    } else if (input$word == "dunkin") {
      make_bar_graph("clean_dunkin_tweets.csv") 
    } else if (input$word == "shiru") {
      make_bar_graph("clean_shiru_tweets.csv")
    }
  })
  
  #Create the sentiment category graphs
  output$plot3 <- renderPlot({
    if (input$item == "coffee"){
      make_cat_graph("clean_coffee_tweets.csv")
    } else if (input$item == "starbucks") {
      make_cat_graph("clean_starbucks_tweets.csv")
    } else if (input$item == "dunkin") {
      make_cat_graph("clean_dunkin_tweets.csv") 
    } else if (input$item == "shiru") {
      make_cat_graph("clean_shiru_tweets.csv")
    }
  })
  
}

shinyApp(ui = ui, server = server)
