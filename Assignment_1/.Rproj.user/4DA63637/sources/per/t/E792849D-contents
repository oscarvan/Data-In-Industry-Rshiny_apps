shinyServer(function(input, output, session) {
 
    # Summary output
    output$SummaryA1 <- renderPrint({
      str(data)
    })
    
    # Summary output
    output$SummaryA2 <- renderPrint({
      summary(as.data.frame(data))
    })
    
    #Mosaic output
    output$Mosaic <- renderPlot({
      formula <- as.formula(paste("~",paste(input$Variables_cat, collapse = " + ")))
      vcd::mosaic(formula, data = data,
                  main = "Categorical variables",
                  shade = TRUE, legend = TRUE)
    })
    
    # Pairs output
    output$pairs <- renderPlot({
      neededVarsPairs <- c(input$VarsToShowPairs, input$VarToColour)
      
      dataPairs <- data[, neededVarsPairs]
      
      GGally::ggpairs(data = dataPairs, 
                      mapping = aes_string(colour=input$VarToColour),
                      title = "Pairs of the data", 
                      progress = FALSE)
    })
    
    # Correlation corrgram output
    output$Corrgram <- renderPlot({
      neededVarsCorr <- c(input$VarsToShowCorr)
      dataCorr = data[, neededVarsCorr]
      corrgram(dataCorr, 
               order = input$Group, 
               abs = input$abs, 
               cor.method = input$CorrMeth,
               text.panel = panel.txt,
               main = "Correlation")
    })
    
    # Missingness output
    output$vis_miss <- renderPlot({
      neededVarsMiss <- c(input$VarsToShowMiss)
      dataMiss = data[, neededVarsMiss]
      vis_miss(dataMiss, cluster = input$cluster) +
        labs(title = "Missingness")
    })
    
    #Boxplot (numeric) output
    output$Boxplot <- renderPlot({
        neededVarsBox <- c(input$VarsToShowBox)
        dataBox = num_data[, neededVarsBox]
        dataBox <- as.matrix(dataBox)
        dataBox <- scale(dataBox, center = input$standardise, scale = input$standardise)
        
        car::Boxplot(y = dataBox, ylab = NA, use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
                     horizontal = FALSE, outline = input$outliers, 
                     col = brewer.pal(n = dim(data)[2], name = "PuBu"),
                     range = input$range, main = "Boxplots", 
                     id = ifelse(input$outliers, list(n = Inf, location = "avoid"), FALSE)) 
    })
    
    # Rising value (Gaps) output
    output$rising_value <- renderPlot({
      
      neededVarsRis <- c(input$VarsToShowRis) #only numeric vars available on UI
      data_Ris = data[, neededVarsRis]
  
      for (col in 1:ncol(data_Ris)) {
        data_Ris[,col] <- data_Ris[order(data_Ris[,col]),col] #sort each column in ascending order
      }
      
      data_Ris <- scale(x = data_Ris, center = input$centre, scale = input$scale)
      
      mypalette <- rainbow(ncol(data_Ris))
      
      matplot(x = seq(1, 100, length.out = nrow(data_Ris)), y = data_Ris, 
              type = "l", xlab = "Percentile", ylab = "Values",
              lty = 1, lwd = 1, col = mypalette, main = "Rising value chart")
      
      legend(legend = colnames(data_Ris), x = "bottomright",  
             lty = 1, lwd = 5, col = mypalette, ncol = round(ncol(data_Ris)^0.3))
    })
    
    # Histogram output
    output$histogram <- renderPlot({
      neededVarHist <- c(input$inputVarHist)#only numeric vars available on UI
      data_Hist = data[, neededVarHist]
      
      hist(data_Hist,
           breaks = as.numeric(input$n_breaks),
           xlab = input$inputVarHist,
           main = paste("Histogram of", input$inputVarHist))
      
      if (input$individual_obs) {
        rug(data_Hist)
      }
    })
    
    # PLOTLY Histogram output
    output$histogramPlotly <- renderPlot({
      
      p <- ggplot2::ggplot(data, mapping = aes(x = Y)) +
        
        geom_histogram(50) +
        
        labs(title = "Histogram of a variable that has a gap in values")
      ggplotly(p)
    })
  
    
    output$TableX <- DT::renderDataTable({
      if (input$Responsive) {
        ext <- list(Responsive = TRUE)
      } else {
        ext <- list()
      }
      DT::datatable(data = data,
                    rownames = input$Rownames,
                    selection = input$Selection,
                    filter = list(position = input$Filter),
                    options = list(searching = TRUE,
                                   pageLength = 10,
                                   lengthMenu = list(c(10,25, -1), c("10","25","All")),
                                   dom = paste(input$Dom, collapse = ""),
                                   ordering = input$Order,
                                   orderClasses = TRUE
                    ),
                    extensions = ext
      )  %>%
        formatStyle(columns = c("Y"), backgroundColor = "lightblue")  %>%
        #       formatCurrency(c(2), '$') %>%      #uncomment this line to see currency formatting
        #       formatPercentage(c(3), 2) %>%      #uncomment this line to see percentage formatting (in this case pointless)
        formatRound(c("Y"), 3)
    })
    
    output$SelRows <- renderTable({   #"renderTable" is simpler than "renderDataTable"
      req(input$TableX_rows_selected)
      print(data[input$TableX_rows_selected,"Y"])
    })

})