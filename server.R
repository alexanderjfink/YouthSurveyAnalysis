function(input, output, session) {
  ################
  # Summary Data #
  ################

  output$raceSchoolCross <- renderPrint({
    crosstab(spyc$School, spyc$SimpleRace)
  })
  output$raceGenderCross <- renderPrint({
    crosstab(spyc$SimpleRace, spyc$SimpleGender)
  })
  output$genderSchoolCross <- renderPrint({
    crosstab(spyc$SimpleGender, spyc$School)
  })
  output$raceTable <- renderPrint({
    summary(spyc$SimpleRace)
  })
  output$genderTable <- renderPrint({
    summary(spyc$SimpleGender)
  })
  output$school <- renderPrint({
    summary(spyc$School)
  })
  output$city <- renderPrint({
    summary(spyc$City)
  })
  output$neighborhood <- renderPrint({
    summary(spyc$Neighborhood)
  })

  #############
  # Bar Plots #
  #############

  # Gender
  output$genderPlot <- renderPlot({
    barplot(table(spyc$SimpleGender),
            main="Gender Frequencies",
            ylab="Number",
            xlab="Gender")
  })

  # Race
  output$racePlot <- renderPlot({
    barplot(table(spyc$SimpleRace),
            main="Race Frequencies",
            ylab="Number",
            xlab="Race")
  })

  # School
  output$schoolPlot <- renderPlot({
    barplot(table(spyc$School),
            main="School Frequencies",
            ylab="Number",
            xlab="School")
  })


  ######################
  # Word Cloud Builder #
  ######################

  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Updating word cloud...")

        # Need to get the appropriate input, basically camel case the selection they've made
        byInput <- tolower(input$selection)
        byInput <- input[[byInput]]

        getTermMatrix(input$selection, byInput, input$query, input$toRemove)
      })
    })
  })

  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)

  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })

  ##############
  # Data Table #
  ##############
  spyc <- read.csv("spycCleanedAndCoded.csv", header = TRUE, sep = ",", na.strings = "NA")
  output$dataTable <- renderDataTable(spyc)

}
