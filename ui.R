
# The UI or the client interface in RShiny.

ui <- fluidPage(
  
  #Displays the leaflet map
  leafletOutput("map"),
  br(),
  #creates tabbed panels 
  # 1st panel displays the latitude and longitude clicked by the user and the trending tags in that location
  # 2nd panel displays the top 25 tweets in a table for the most popular topic in that location
  # 3rd panel - a wordcloud is created from the top 500 tweets for the most popular topic in that location
  # 4th panel - histogram is created and a sentiment score is given
  verbatimTextOutput("out"),
  textOutput("out3"),
  tabsetPanel(type="tab",
              tabPanel("Top Trending Topics",tableOutput("out1")),
              tabPanel(" List of Tweets",tableOutput("out2")),
              tabPanel("plot Wordcloud", plotOutput("plot")),
              tabPanel("plot sentiment Wordcloud", plotOutput("plot1",width = 450,height = 350),verbatimTextOutput("text"),
                       p("The tweets related to the most popular topic are anayzed, the application authenticates with the Twitter API and fetches those Tweets related to the topic. Then, the analytical engine, processes those Tweets, and analyzes their sentiment. With a scoring algorithm, each Tweet is given a score, based on the emotionally positive and negative entities encountered. This will aid in analyzing the emotion people have in general for a topic"))
  )
    
  )
   




# Important - To be removed later

#1. ensure that the setwd() is used and set the location to the folder that contains all these R files and positive and negative values
#2. ensure that all the libraries and packages mentioned in "global.r" are installed and loaded
#3. ensure that teh twitter connection is active
#4. run the program
#Note: If the map is clicked multiple times (more than 6), the twitter access will expire and we will have to wait for the access
#To do:
 # 1. Understand the existing code and Add comments
#2. Try and understand http://shiny.rstudio.com/reference/shiny/latest/validate.html . this is to remove the error message at the beggining 
#3. Create report
#4. Create video