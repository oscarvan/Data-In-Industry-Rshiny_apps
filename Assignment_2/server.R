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
      
      vis_miss(dataMiss, cluster = input$cluster, sort_miss = input$sort) +
        labs(title = paste("Missingess, clustered: ", input$cluster, "sorted: ", input$sort))
    })
    
    # Predicting the missingness
    output$r_part <- renderPlot({
      
      covid = data 

      covid <- covid[order(covid$CODE),]
      covid$idNum <- 1:nrow(covid)

      # step 2
      covid$missingness <- apply(X = is.na(covid), MARGIN = 1, FUN = sum)

      # step 3
      tree <- caret::train(missingness ~ .,
                           data = covid,
                           method = "rpart",
                           na.action = na.rpart) # na.rpart means "rpart will deal with missing predictors intrinsically"
      # step 4
      rpart.plot(tree$finalModel, 
                 main = "Predicting the number of missing variables in an observation",
                 sub = "Check whether the outcome variable is an important variable",
                 roundint = TRUE, 
                 clip.facs = TRUE)
    })
    

    
    # Miss_upset output 
    output$gg_miss <- renderPlot({
      neededVarsGGMiss <- c(input$VarsToShowgGGMiss)
      dataGGMiss = data[, neededVarsGGMiss]
      
      gg_miss_upset(data = dataGGMiss, nsets = input$range_upset)
    })
    
    # Missingness correlation
    output$miss_correlation <- renderPlot({

        m <- is.na(data) + 0 # this is a trick that transforms Logical to Binary
        cm <- colMeans(m)
        m <- m[, cm > 0 & cm < 1, drop = FALSE] #remove none-missing or all-missing variables
        colnames(m)

        corrgram::corrgram(cor(m), order = "OLO", abs = TRUE)
        title(main = "Variable missing value correlation")

    })
    
    output$Boxplot_answer <- renderPlot({
      d1 <- data
      cols <- sapply(d1, FUN = is.numeric)
      d <-  d1[, cols]
      d <- scale(d, center = input$BCentre, scale = input$BScale)
      
      par(cex.axis=0.5) # change size of x labels
      print(colnames(d))
      if (input$Label && input$ShowOutliers) {
        car::Boxplot(y = d, las = 2, range = input$Range, outline = TRUE,
                     id = list(labels = d1$CODE, n=20, col = "red", location = "avoid"),
                     main = "Boxplots of numeric variables")
      } else {
        car::Boxplot(y = d, las = 2, range = input$Range, outline = input$ShowOutliers,
                     id = list(n=0), main = "Boxplots of numeric variables")              
      }
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
  
    # DATA TABLE
    output$mytable <- DT::renderDataTable({
      data
    })
    
    
    ########## MODEL ##############
    

    
    # Function to clean data
    output$model <- renderPlot({
      
      d <- covid
      
      output$dim_before <- renderPrint({
        cat("Original data dimension: ",dim(d))
      })
      
      pMiss <- function(x){ sum(is.na(x)) / length(x) * 100 }
      
      # remove variables first
      cRatio <- apply(d,2,pMiss)
      d <- d[, cRatio < input$var_miss_threshold]
      # then observations
      rRatio <- apply(d,1,pMiss)
      d_cleaned <- d[rRatio < input$obs_miss_threshold, ]
      
      output$dim_after <- renderPrint({
        cat("Final data dimensions are: ",dim(d_cleaned))
      })
    
      # Test train split functions
      train <- d_cleaned[d_cleaned$OBS_TYPE == 'Train',]
      test <- d_cleaned[d_cleaned$OBS_TYPE == 'Test',]
      
      # Recipe function
      recipeA <- recipes::recipe(DEATH_RATE ~., data = train) %>%
        update_role("CODE", new_role = 'id') %>% #id is not a predictor
        update_role('OBS_TYPE', new_role = 'split') %>% #obs_type is not a predictor
        step_impute_knn(all_predictors(), neighbors = 5) %>%
        step_rm('OBS_TYPE') %>%
        step_rm("CODE") %>%
        step_center(all_numeric(), -has_role("outcome"))%>%
        step_scale(all_numeric(), -has_role("outcome")) %>%
        step_dummy(all_predictors(), -all_numeric())

      # Function to train model 
      fit.control <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
      
      set.seed(123)
      model <- caret::train(
        recipeA, 
        data = train, 
        tuneGrid = expand.grid(
          alpha = 0:1,
          lambda = seq(0.0001, 1, length = 20)
        ),
        method = 'glmnet',  
        trControl = fit.control)
      
      test$Prediction <- predict(model, newdata = test)
      rmse = caret::RMSE(test$Prediction, test$DEATH_RATE, na.rm = TRUE)
      
      output$rmse <- renderPrint({
        rmse
      })
      
      rang <- range(c(test$DEATH_RATE, test$Prediction))
      ggplot(data = test) +
        geom_point(mapping = aes(x = Prediction, y = DEATH_RATE)) +
        geom_abline(slope = 1, col = "blue") +
        labs(title = "Death Rate predictions of covid test data", y = "predicted", x = "actual") +
        coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)
      })
    
    
    
    
      
    ########## MODEL - REACTIVE CASCADE VERSION ##############
    
    output$dim_before_re <- renderPrint({
      cat("Original data dimension: ",dim(covid))
    })
    
    # Function to clean data
    getCleanData <- shiny::reactive({
      pMiss <- function(x){ sum(is.na(x)) / length(x) * 100 }
      
      # remove variables first
      cRatio <- apply(covid,2,pMiss)
      covid <- covid[, cRatio < input$var_miss_threshold_re]
      # then observations
      rRatio <- apply(covid,1,pMiss)
      covid_clean <- covid[rRatio < input$obs_miss_threshold_re, ]
      
      output$dim_after_re <- renderPrint({
        cat("Final data dimensions are: ",dim(covid_clean))
      })
    })
    
    # Test train split functions
    trainingData <- reactive({
      d = getCleanData()
      d[d$OBS_TYPE == 'Train',]
    })
    
    testData <- reactive({
      d = getCleanData()
      d[d$OBS_TYPE == 'Test',]
    })
    
    # Recipe function
    recipe <- reactive({
      recipes::recipe(DEATH_RATE ~., data = trainingData()) %>%
        update_role("CODE", new_role = 'id') %>% #id is not a predictor
        update_role('OBS_TYPE', new_role = 'split') %>% #obs_type is not a predictor
        step_impute_knn(all_predictors(), neighbors = 5) %>%
        step_rm('OBS_TYPE') %>%
        step_rm("CODE") %>%
        step_center(all_numeric(), -has_role("outcome"))%>%
        step_scale(all_numeric(), -has_role("outcome")) %>%
        step_dummy(all_predictors(), -all_numeric())
    })
    
    # prepped_recipeA <- prep(recipeA, training = train)
    # prepped_recipeA
    # 
    # covid_train_baked <- bake(prepped_recipeA, train) 
    # covid_train_baked
    # 
    # covid_test_baked <- bake(prepped_recipeA, test)
    # covid_test_baked
    
    # I DONT UNDERSTAND HOW TO APPLY THE RECIPE TO TEST DATA ??? - prep(), bake()?
    # If I use prepped and baked test set in predictions below doesnt work?
    
    # Function to train model & Output Model summary
    Linear_Model <- shiny::reactive({
      fit_control <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
      
      set.seed(123)
      caret::train(
        recipe(), 
        data = trainingData(), 
        tuneGrid = expand.grid(
          alpha = 0:1,
          lambda = seq(0.0001, 1, length = 20)
        ),
        method = 'glmnet',  
        trControl = fit_control)
    })
    
    output$Model_summary_re <- renderPrint(summary(Linear_Model()))
    
    # RMSE output
    calc_rmse <- reactive({
      testData()$Prediction <- predict(Linear_Model(), newdata = testData())
      caret::RMSE(testData()$Prediction, testData()$DEATH_RATE, na.rm = TRUE)
    })
    output$rmse_re <- renderPrint({calc_rmse()})
    
    # Plot Predictions 
    Fit <- reactive({
      rang <- range(c(testData()$DEATH_RATE, testData()$Prediction))
      
      ggplot(data = testData()) +
        geom_point(mapping = aes(x = Prediction, y = DEATH_RATE)) +
        geom_abline(slope = 1, col = "blue") +
        labs(title = "Death Rate predictions of covid test data", 
             y = "predicted", 
             x = "actual") +
        coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)
    })
    output$model_plot_re <- renderPlot(Fit())
    
    # Plot Residual Boxplot 
    res_boxplot <- reactive({
      
      best_model = Linear_Model()$finalModel
      
      # Get residuals for train data
      trainingData()$Residuals <- residuals(model)
      
      # Get residuals for test data
      testData()$Prediction <- predict(best_model, newx = testData())
      testData()$Residuals <- testData()$DEATH_RATE - testData()$Prediction
      
      # Create a data frame with residuals and data set indicator
      d1 <- rbind(
        data.frame(Data = "Test", Residuals = testData()$Residuals),
        data.frame(Data = "Train", Residuals = trainingData()$Residuals)
      )
      
      # Create boxplot with ggplot2
      p <- ggplot(data, aes(x = Data, y = Residuals)) + 
        geom_boxplot()
      
      # Label outliers
      outliers <- boxplot(data$Residuals, plot = FALSE)$out
      p + geom_text(aes(x = Data, y = outliers, label = round(outliers, 2)), 
                    hjust = -0.2, vjust = 0.5)
    })
    output$residuals_boxplot_re <- renderPlot(res_boxplot())
})