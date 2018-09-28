# ui.R

###
shinyUI(pageWithSidebar(
    headerPanel("Predicting Next Word"),
    sidebarPanel(
        
        helpText("Please, type in your text inputs"),
        textInput("textPrev", "", value = "Enter text..."),
        br(),
        hr(),
        
        helpText("Please, click 'submit' button to predict a next word"),
        actionButton("Go", label=h3("submit")),
        tags$br(),
        hr(),
        hr()
    ),
    mainPanel(
        helpText(" Predicted Next Word is: "),
        h3(textOutput("predText")),

        # word cloud plot
        plotOutput("wcPlot")
    )
))

