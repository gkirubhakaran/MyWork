# server that contains the functions to interact with the Shiny UI

server <- function(input, output, session) {
  # Render map on the page using functions from leaflet package
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles(options = providerTileOptions(noWrap = TRUE))
  })
  
  # print the latitude and longitude of the clicked location using leaflet generic click functions and variables
  output$out <- renderPrint({
    validate(need(input$map_click, FALSE))
    str(input$map_click[1:2])
  })
  
  # call functions which are present in the global.R file
  # Pass latitude and longitude values as parameters to the functions
  output$out1 <- renderTable({validate(need(input$map_click, FALSE))
    f1(input$map_click[1],input$map_click[2])})
  
  output$out2 <- renderTable(
    { validate(need(input$map_click, FALSE))
      f2(input$map_click[1],input$map_click[2])}
    )
  
  output$out3 <- renderText(
    { 
    validate(need(input$map_click, FALSE))
    v <- closestTrendLocations(input$map_click[1],input$map_click[2])
    paste("The closest Trend Location is",v[,1])}
    
    )
  
  
  # plot wordcloud with the tweets 
  output$plot <- renderPlot({
    validate(need(input$map_click, FALSE))
    v <- getTermMatrix(input$map_click[1],input$map_click[2])
    wordcloud(v$word, v$freq, scale=c(5,1), 4, 500,TRUE,colors=brewer.pal(8, "Dark2"))
  })
  
  
  output$plot1 <- renderPlot({
    validate(need(input$map_click, FALSE))
    ctl=closestTrendLocations(input$map_click[1],input$map_click[2])
    cc<-getTrends(ctl[3])
    tweets<-searchTwitter( cc$name[1],n = 100,lang="en")
    pos = scan('positive-words.txt',what = 'charecter',comment.char = ';')
    neg = scan('negative-words.txt',what = 'charecter',comment.char = ';')
    tweets.text = laply(tweets, function(t)t$getText())
    #Removing emoticons
    tweets.text = iconv(tweets.text, "latin1", "UTF-8", sub='')
    sentences = tweets.text
    pos.words = pos
    neg.words = neg
    .progress = 'none'
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      
      sentence = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', sentence)
      
      sentence = gsub('@\\w+', '', sentence)
      
      sentence = gsub('[[:punct:]]', '', sentence)
      
      sentence = gsub('[[:cntrl:]]', '', sentence)
      
      sentence = gsub('\\d', '', sentence)
      
      sentence = tolower(sentence)
      
      word.list = str_split(sentence, '\\s')
      
      words = unlist(word.list)
      
      pos.matches = match(words, pos.words)
      
      neg.matches = match(words, neg.words)
      
      pos.matches = !is.na(pos.matches)
      
      neg.matches = !is.na(neg.matches)
      
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
      
    }, pos.words, neg.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    analysis = scores.df
    hist(analysis$score,main=paste("Sentiment of Tweets vs Frequency of Sentiment"),col = "Navy Blue")
    
    output$text <- renderText({
      paste("Mean sentiment score is:",mean(analysis$score))
    })
    
  })
  
}
