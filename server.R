#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(DT)
library(scales)
library(tm)
library(sentimentr)
library(stringr)

try.error = function(x)
{
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
        y = tolower(x)
    # result
    return(y)
}

cleanData = function(docs)
{
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    # Text stemming
    # docs <- tm_map(docs, stemDocument)
    return(docs)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
    mydata <- reactive({
        inFile <- input$finput
        if(is.null(inFile)){return()}
        read.csv(inFile$datapath, header = TRUE, sep = ',',quote = "")
    })
    
    getComments <- reactive({
        comments <- mydata()$Comment
        comments = gsub("\\'", "", comments)
        comments = gsub("\\’", "", comments)
        comments <- gsub("[[:punct:]]", "", comments)
        comments <- gsub("[[:digit:]]", "", comments)
        comments <- gsub("http\\w+", "", comments)
        comments <- gsub("[ \t]{2,}", "", comments)
        comments <- gsub("^\\s+|\\s+$", "", comments)
        comments <- sapply(comments, replace_emoticon)
        comments <- sapply(comments, try.error)
        comments <- comments[!is.na(comments)]
        names(comments) = NULL
        comments
    })
    
    emotion <- reactive({
        class_emo <- classify_emotion(getComments(), algorithm="bayes", prior=1.0)
        emotion <- class_emo[,7]
        emotion[is.na(emotion)] = "unknown"
        emotion
    })
    
    polarity <- reactive({
        class_pol <- classify_polarity(getComments(), algorithm="bayes")
        polarity <- class_pol[,4]
        polarity
    })
    
    getDataFrame <- reactive({
        sent_df <- data.frame(Text=getComments(), Emotion=emotion(),
                              Polarity=polarity(), stringsAsFactors=FALSE)
        sent_df <- within(sent_df,
                          Emotion <- factor(emotion(), levels=names(sort(table(emotion()), decreasing=TRUE))))
        sent_df
    })
    
    eliminateWords <- c("first","american", "good", "ive", "get","amp", "2yo", "3yo", "4yo",
                        "it", "will", "brother", "give", "another","didnt",
                        "called", "back", "came", "kept", "sure","ever",
                        "one", "said", "she", "put", "shes","also","dont",
                        "claim", "try", "just","back","told","tell","asked",
                        "lot","able","someone","gave","new","done","two","rep"
    )
    
    eWordCloud <- reactive({
        
        emos <- levels(factor(getDataFrame()$Emotion))
        nemo <- length(emos)
        emo.docs <- rep("", nemo)
        for (i in 1:nemo)
        {
            tmp <- getComments()[emotion() == emos[i]]
            emo.docs[i] <- paste(tmp, collapse=" ")
        }
        emo.docs <- removeWords(emo.docs, c(stopwords("english"),eliminateWords))
        corpus <- Corpus(VectorSource(emo.docs))
        corpus <- cleanData(corpus)
        tdm <- TermDocumentMatrix(corpus)
        tdm <- as.matrix(tdm)
        colnames(tdm) <- emos
        
        comparison.cloud(tdm, max.words = 200, 
                         colors = brewer.pal(6, "Dark2"),
                         scale = c(6,.8), random.order = FALSE,rot.per=.15, title.size = 1.5)
    })
    
    pWordCloud <- reactive({
        
        emos <- levels(factor(getDataFrame()$Polarity))
        nemo <- length(emos)
        emo.docs <- rep("", nemo)
        for (i in 1:nemo)
        {
            tmp <- getComments()[polarity() == emos[i]]
            emo.docs[i] <- paste(tmp, collapse=" ")
        }
        emo.docs <- removeWords(emo.docs, c(stopwords("english"),eliminateWords))
        corpus <- Corpus(VectorSource(emo.docs))
        corpus <- cleanData(corpus)
        tdm <- TermDocumentMatrix(corpus)
        tdm <- as.matrix(tdm)
        colnames(tdm) <- emos
        
        comparison.cloud(tdm, colors = brewer.pal(8, "Dark2"),
                         scale = c(8,.2), random.order = FALSE,rot.per=.15, title.size = 1.5)
    })
    
    output$eplot <- renderPlot({
        if (is.null(input$finput))
            return("NULL")
        
        print(ggplot(getDataFrame(), aes(x=emotion())) +
                  geom_bar(aes(y=..count.., fill=emotion())) + 
                  geom_text(aes(y = (..count..), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
                  scale_fill_brewer(palette="Dark2") + 
                  labs(x="emotion categories", y="number of Comments",title = "Classification by Emotion") + 
                  theme(plot.title = element_text(size=rel(2)), legend.position = "none"))
    })
    
    output$pplot <- renderPlot({
        if (is.null(input$finput))
            return(NULL)
        
        print(ggplot(getDataFrame(), aes(x=polarity())) +
                  geom_bar(aes(y=..count.., fill=polarity())) +
                  geom_text(aes(y = (..count..), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
                  scale_fill_brewer(palette="RdGy") +
                  labs(x="polarity categories", y="number of Comments") +
                  labs(title = "Classification by Polarity") +
                  theme(plot.title = element_text(size=rel(2)), legend.position = "none"))
    })
    
    
    output$ewordcloud <- renderPlot({
        if (is.null(input$finput))
            return(NULL)
        eWordCloud()
    })
    
    output$pwordcloud <- renderPlot({
        if (is.null(input$finput))
            return(NULL)
        pWordCloud()
    })
    
    output$fData <- DT::renderDataTable({
        DT::datatable(mydata(),
                      options = list(
                          initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                              "}"),
                          orderClasses = TRUE,searchHighlight = TRUE)) 
        #%>% 
            #formatDate('Date','toDateString')
    })
    
    output$eData <- DT::renderDataTable({
        if (is.null(input$finput))
            return(NULL)
        else
        {
        edata <- data.frame(Text=getComments(), Emotion=emotion(), stringsAsFactors=FALSE)
        edata <- within(edata,
                        Emotion <- factor(emotion(), levels=names(sort(table(emotion()), decreasing=TRUE))))
        DT::datatable(edata,filter = 'top',
        #DT::datatable(getDataFrame(),filter = 'top',
                      options = list(
                          initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                              "}"),
                          orderClasses = TRUE,searchHighlight = TRUE))
        }
        
    })
    
    output$pData <- DT::renderDataTable({
        if (is.null(input$finput))
            return(NULL)
        else
        {
        pdata <- data.frame(Text=getComments(), Polarity=polarity(), stringsAsFactors=FALSE)
        pdata <- within(pdata,
                        Polarity <- factor(polarity(), levels=names(sort(table(polarity()), decreasing=TRUE))))
        DT::datatable(pdata,filter = 'top',
                      #DT::datatable(getDataFrame(),filter = 'top',
                      options = list(
                          initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                              "}"),
                          orderClasses = TRUE,searchHighlight = TRUE))
        }
        
    })
    
    sampleTemplate <- reactive({
        read.csv("D:\\Share\\CSV\\SampleTemplate2.csv")
    })
    
    output$sampleTable <- DT::renderDataTable({
        DT::datatable(sampleTemplate(), options = list(
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                "}"),
            orderClasses = TRUE,searchHighlight = TRUE))
    })
    
    output$downloadCsv <- downloadHandler(
        filename = "SampleTemplate.csv",
        content = function(file) {
            write.csv(sampleTemplate(), file)
        },
        contentType = "text/csv"
    )
})
