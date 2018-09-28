# server.R
####

shinyServer(
  function(input, output, session) {
    
    # 1) Return the selected dataset using 'eventReactive()', which depends on input$Go
    # the output is only updated when the user clicks the button
    Pred_text_Ngram <- eventReactive(input$Go, {
      # process input texts and predict the next words
        predNextWords_DT(input$textPrev)
    })
    #2) Output the predicted text word
    output$predText <- renderText({
        print(as.character(Pred_text_Ngram()[, .SD[1], .SDcols=1]))
    })
    

    # 3) Show the word cloud plot 
    # the plot is updated only when the user clicks the action table
    output$wcPlot <- renderPlot({
        DT_pred_words_wc <- Pred_text_Ngram()

        set.seed(1234)
        wordcloud(words = DT_pred_words_wc[, get(names(DT_pred_words_wc)[1] )], 
                  freq = DT_pred_words_wc[, get(names(DT_pred_words_wc)[2] )],                   
                  min.freq = 1, 
                  #max.words=DT_pred_words_wc[,.N] %% 100, #
                  max.words= ifelse(DT_pred_words_wc[,.N]<100, DT_pred_words_wc[,.N], 99), # up 99 words
                  random.order=FALSE, rot.per=0.15, 
                  colors=brewer.pal(8, "Dark2"))
    })
    
  }
)