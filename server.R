# server.R
library(MASS)
library(corrplot)

source("common.R")


shinyServer(function(input, output, session) {
  
  
  ##################### load own data set #########################
  
  output$file.text <- renderText(
  {
    str <- "You can load own data sets from CSV files. Note that this is a educational application, not implemented for productive use.
    Small-scale data is recommended. 
    Performance on large data sets is limited by the server (e.g. no parallel processing in current subscription model).
    Select file, select class column and then click \"Load file\" button."
        
    invisible(str)
  })
      
  
  
  # preview of loaded data from CSV file  
  output$file.dataAsTable <- renderTable({

      df <- loadCSVFile()
    
      req(df)
      df <- head(df, 12)
      #     
#    if(! is.null(loaded_CSV_file) ){
#    }else
 #   {
  #    print("NULL")
   #   df <- as.data.frame("select file to load")
  #  }

  })
  
  
  currentlyLoadedCSVFile <- ""
  
  # load CSV file that was selected by the user  
  loadCSVFile <- reactive({
    
    
   # if(input$file.CSVFile$datapath != currentlyLoadedCSVFile){ # new file loaded?
      
      req(input$file.CSVFile$datapath)
      # load selected csv file
      loaded_CSV_file <<- read.csv(input$file.CSVFile$datapath, header = input$file.header)
      
      currentlyLoadedCSVFile <<- input$file.CSVFile$datapath
      
      # update list of columns, so that user can select class column
      updateSelectInput(session, "file.ClassColumn",
                        choices = as.list(names(loaded_CSV_file)),
                        selected = names(loaded_CSV_file)[1])
  #  }
    
    invisible(loaded_CSV_file)
  })
  
  
  # load CSV file  
  observeEvent(input$file.load, {
    
    
    if(currentlyLoadedCSVFile != ""){
      
      # load selected csv file
      loaded_CSV_file <<- read.csv(input$file.CSVFile$datapath, header = input$file.header)
      
      # TODO csv file in combbobox?
      # add selectable item (csv file) in list of data sets and set index of class column
      moduleRegistry$lookup("DA")$loaded_CSV_file <<-  which( names(loaded_CSV_file) == input$file.ClassColumn )
      
      #update list of files
      updateSelectInput(session, "mod.featureSelection.selDataset",
                        choices = as.list(names(moduleRegistry$lookup("DA")$options)),   
                        selected = names(moduleRegistry$lookup("DA")$options)[length(moduleRegistry$lookup("DA")$options)])
      
      updateCheckboxGroupInput(session, "mod.featureSelection.selColumns",  
                               choices =  as.list(names(loaded_CSV_file)[-which( names(loaded_CSV_file) == input$file.ClassColumn )]),
                               selected = as.list(names(loaded_CSV_file)[-which( names(loaded_CSV_file) == input$file.ClassColumn )]) )
    }
    
  })
  
  
  ##################### DATA ACQUISITION #####################

  
    # load selected columns from selected data set
  readData <- reactive(
    {
      loadedDataset <- NULL

      # get data from pre-defined variable by name
      loadedDataset <- get(input$mod.featureSelection.selDataset)
      flowTemplate$dat$data <<- loadedDataset[, input$mod.featureSelection.selColumns]

      classColumn <- moduleRegistry$lookup("DA")$options[[input$mod.featureSelection.selDataset]]
      flowTemplate$dat$labels <<- as.factor(loadedDataset[,classColumn])
      lev <- levels(flowTemplate$dat$labels)
      flowTemplate$dat$nmbrClasses <<- length(levels(flowTemplate$dat$labels))

      return(flowTemplate) 
    }
  ) 
  
  
  # DataAquisition: selection of columns: check boxes
  output$mod.featureSelection.selectColumns <- renderUI({
    dat <- get(input$mod.featureSelection.selDataset)
    classColumn <- moduleRegistry$lookup("DA")$options[[input$mod.featureSelection.selDataset]]
    
    checkboxGroupInput("mod.featureSelection.selColumns", "Select features (min. 2)", 
                       choices = names(dat)[-classColumn], 
                       selected = names(dat)[-classColumn] )
  })
  
  #TODO  class selection currently not used: DataAquisition: selection of class
  output$mod.featureSelection.selectClass <- renderUI({
    dat <- get(input$mod.featureSelection.selDataset)
    cols <- c(names(dat), "none")
    classColumn <- moduleRegistry$lookup("DA")$options[[input$mod.featureSelection.selDataset]]
    selectInput("mod.featureSelection.selClass", "Select class", choices  = cols, selected = cols[classColumn] ) 
  })


  ###################### output #######################
  
  ############# selected workflow parameters ############
  
  output$wf.workflowParams <- renderUI(
    { 
      str1 <- paste("Preprocessing:   ", input$mod.preprocessing.selNormalization)
      str2 <- paste("Transformation:   ", input$mod.transformation.selTransformationMethod)
      str3 <- paste("Model selection -- classifier:  ", input$mod.modelSelection.selClassifier)
      str4 <- paste("Model selection -- param. tuning:  ", input$mod.modelSelection.selTrainingMethod)
      
      HTML(paste('</br>', str1, str2,  str3, str4, sep = '<br/>'))
    }
  )
  

    
  ################ Visualizations #################  
  
  # Visualization: table  
  output$vis.dataAsTable <- renderTable({
    flow <- readData()
    
    df <- head(flow$dat$data, 30)
    df <- rbind(df, rep("...", ncol(flow$dat$data) ) ) 


  })
  


  # Visualization: scatter plot matrix
  output$vis.scatterPlot <- renderPlot( 
    {
      flow <- readData()

      if(isValidForClassification(flow$dat))
      {
        if(ncol(flow$dat$data) <=2){
          plot(flow$dat$data, cex=1.5, pch=19, col = getColors(flow$dat$nmbrClasses)[unclass(flow$dat$labels)])
          points(flow$dat$data, cex=1.5, pch=1)
        }else{
          featurePlot(x = flow$dat$data, y = flow$dat$labels, plot = "pairs", pch=19, col= getColors(flow$dat$nmbrClasses)) 
        }
      }
    
    })
  
  # Visualization: par coords plot
  output$vis.parCoordsPlot <- renderPlot( 
    {
      flow <- readData()
      
      if(isValidForClassification(flow$dat))
      {
        parcoord(flow$dat$data, col = getColors(flow$dat$nmbrClasses)[flow$dat$labels]) 
      }
    }
  )
  
  # Visualization: density plots per class
  output$vis.densityPlot <- renderPlot(
    {
      flow <- readData()
      
      if(isValidForClassification(flow$dat))
      {
        featurePlot(x = flow$dat$data, y = flow$dat$labels, plot = "density", adjust=1.5, pch='|', col= getColors(flow$dat$nmbrClasses),
                    scales = list( x=list(relation="free"), y=list(relation="free") ))
        }
    }
  )
  
  # Visualization: box plots per class
  output$vis.boxPlot <- renderPlot(
    {
      flow <- readData()
      
      if(isValidForClassification(flow$dat))
      {
        featurePlot(x = flow$dat$data, y = flow$dat$labels, plot = "box", col= getColors(flow$dat$nmbrClasses))
      }
    }
  )
  
  
  # Visualization: feature correlation matrix (Pearson's R)
  output$vis.corrPlot <- renderPlot(
    {
      flow <- readData()
      
      cm <- cor(flow$dat$data)
      corrplot(cm, method="circle")
    }
  )
  
  #feature importance as determined by rf
  output$vis.featImportancePlot <- renderPlot(
    {
      flow <- readData()
      model = randomForest(x=flow$dat$data, y=flow$dat$labels)
      varImpPlot(model, pch= 19, main="feature importance, determined by random forest (most important on top)")
    }
  )
  

  output$vis.datasetInfo <- renderText(
    {
      flow <- readData()
      
      str <- "### Data dimensions: ###\n"
      str <- paste0(str, "Number of columns (features): ", ncol(flow$dat$data), "\n" ) 
      str <- paste0(str, "Number of rows (instances): ", nrow(flow$dat$data) , "\n" ) 
      
      if(flow$dat$nmbrClasses > 0)
      {
        str <- paste0(str,  "Number of classes: ", flow$dat$nmbrClasses, "\n\n")
      }
      

      str <- paste0(str, "### Features / attributes: ### \n")
      strNames <- paste(names(flow$dat$data), collapse=" ")
      str <- paste0(str, strNames, "\n\n" ) 


      invisible(str)
    }
  )
  
    
  ############# Classification  ##############  

  # do classification 
  startClassification <- reactive(
    {
      withProgress({
        setProgress(message = "Running workflow classification...")
        
        flow <- readData()
      
        if(isValidForClassification(flow$dat))
        {
          moduleRegistry$lookup("SC")$selectOption(input$mod.preprocessing.selNormalization)          
          moduleRegistry$lookup("DR")$selectOption(input$mod.transformation.selTransformationMethod)          
          moduleRegistry$lookup("MS")$selectOption(c(input$mod.modelSelection.selClassifier, input$mod.modelSelection.selTrainingMethod))          

          flow <- classification(flow)
        }
      invisible(flow)
      })
    }
  )
  
  
  # checks if configuration allows classification
  isValidForClassification <- function(data)
  {
    validate( need(! is.null(data$labels), "Requires the selection of a class") )
    return (! is.null(data$labels) )
  }
  
  
  
  #Module classification: conf. matrix (on button click)
  output$cls.confusionMatrix <- renderPrint ( 
    { 
      startClassification()$results$cmTest
    }
  ) 

  # Module Classification: Model
  output$cls.model <- renderPrint ( 
    { 
      startClassification()$results$model 
    } 
  )

  # Module Classification: CM
  output$cls.confusionMatrixTraining <- renderPrint ( 
    { 
      startClassification()$results$cmTraining 
    } 
  )
  
  
  
  # Visualization: scatter plot matrix
  output$cls.scatterPlot <- renderPlot( 
    {
      flow <- startClassification()

      #find misclassified data points
      wasMisclassified <- as.integer(flow$results$test_data_predictions != flow$dat$test_labels) +1
      symbols <- c(19,17)      
            
      if(ncol(flow$dat$test_data) <=2){
        plot(flow$dat$test_data, cex=1.5, pch=symbols[wasMisclassified], col = getColors(flow$dat$nmbrClasses)[unclass(flow$results$test_data_predictions)])
        
      }else{
        featurePlot(x = flow$dat$test_data, y = flow$results$test_data_predictions, plot = "pairs", 
                    pch=symbols[wasMisclassified], auto.key = list(columns = flow$test_dat$nmbrClasses), col = getColors(flow$dat$nmbrClasses))
      }
    }
  )
  
  
  # Module Classification: full results
  output$cls.fullResults <- renderPrint( 
    { 

      print("")
      print("#### Normalization: ####")
      print(startClassification()$results$norm_factors)
      
      print("")
      print("#### Results on training data : ####")
      print(startClassification()$results$model)
      
      print("")
      print("#### Rebustitution error (on training data!) ####")
      print(startClassification()$results$cmTraining)
      
      print("")
      print("#### Results on test data : ####")
      print(startClassification()$results$cmTest)
      
    } 
  )
  
  strSrcCodeR <<- NULL
  strSrcCodePy <<- NULL

  output$downloadSrcR <- downloadHandler(

    filename = function() {
      paste0("src-code_snippet_", startClassification()$params$classifierName[[1]], ".R")
    },
    content = function(filename) {
      fileConn <- base::file(filename)
      writeLines(strSrcCodeR, fileConn)
      close(fileConn)
    }
  )

  output$downloadSrcPy <- downloadHandler(

    filename = function() {
      paste0("src-code_snippet_", startClassification()$params$classifierName[[1]], ".py")
    },
    content = function(filename) {
      fileConn <- base::file(filename)
      writeLines(strSrcCodePy, fileConn)
      close(fileConn)
    }
  )
  
  
  # Module Classification: full results
  output$cls.srcCodeR <- renderText( {
    strSrcCodeR <<- extractSourceCodeR()
  })

  output$cls.srcCodePy <- renderText( {
    strSrcCodePy <<- extractSourceCodePy()
  })
    
  
  ############ Exploration #########################
  
  startModelExploration <- reactive(
    {
      withProgress({
        setProgress(message = "Running workflow exploration ... ")
        
        flow <- doClassificationModelExploration()
        invisible(flow)
      })
    }
  )

  doClassificationModelExploration <- function()
  {
    flow <- readData()
    
    if(isValidForClassification(flow$dat))
    {
      moduleRegistry$lookup("SC")$selectOption(input$mod.preprocessing.selNormalization)          
      moduleRegistry$lookup("DR")$selectOption(input$mod.transformation.selTransformationMethod)          

      modExploration$selectOption(c(input$mod.modelSelection.selClassifier, input$mod.modelSelection.selTrainingMethod))          
      
      #      moduleRegistry$lookup("ME")$selectOption(c(input$mod.modelSelection.selClassifier, input$mod.modelSelection.selTrainingMethod))          
      # TODO: add as option? 
      hyperParams <- getHyperParams(input$mod.modelSelection.selClassifier)
      
      flow <- exploration(flow, hyperParams)
    }
    invisible((flow))
  }
  
  # determine range of user-adjustable parameter for the slider  
  getSliderParams <- function(selClassifier, nmbrOfParam)
  {
    classifierName = moduleRegistry$lookup("MS")$options.classifierNames[[selClassifier]]

    show <- FALSE
    min<-0
    max<-0
    mean<-0
    paramName="<none>"
        
    if(! is.null(moduleRegistry$lookup("MS")$options.classifierParams[[classifierName]]))
    {
      if( nmbrOfParam <= ncol( moduleRegistry$lookup("MS")$options.classifierParams[[classifierName]] ) )
      {
        min= moduleRegistry$lookup("MS")$options.classifierParams[[classifierName]][1,nmbrOfParam]
        max= moduleRegistry$lookup("MS")$options.classifierParams[[classifierName]][nrow( moduleRegistry$lookup("MS")$options.classifierParams[[classifierName]]), nmbrOfParam]
        
        paramName <- substring( names(moduleRegistry$lookup("MS")$options.classifierParams[[classifierName]])[nmbrOfParam], 2) # remove leading "."
        
        mean = (max+min) / 2
        
        if(min == max)
          show <- FALSE
        else
          show <- TRUE
        }
    }
    
    invisible(list(show=show, paramName=paramName, min=min, max=max, start=mean))
  } 
  
  # TODO: more generic approach, adding sliders
  
  # slider to adjust hyperparameter 1
  output$exp.sliderParam1 <- renderUI(
    {
      nmbrOfParam <- 1
      
      if( getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$show )
      {
            sliderInput(inputId = paste0("exp.hyperparam", nmbrOfParam), label = paste0("Adjust parameter ", getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$paramName), 
                  value = getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$start,  
                  min = getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$min,
                  max = getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$max
            )
      }
    }
  )  
  
  
  # slider to adjust hyperparameter 2
  output$exp.sliderParam2 <- renderUI(
    {
      nmbrOfParam <- 2

      if( getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$show )
      {
        sliderInput(inputId = paste0("exp.hyperparam", nmbrOfParam), label = paste0("Adjust parameter ", getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$paramName), 
                    value = getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$start,  
                    min = getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$min,
                    max = getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$max
        )
      }
    
    }
  )  
  
  # slider to adjust hyperparameter 3
  output$exp.sliderParam3 <- renderUI(
    {
      nmbrOfParam <- 3
      
      if( getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$show )
      {
        sliderInput(inputId = paste0("exp.hyperparam", nmbrOfParam), label = paste0("Adjust parameter ", getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$paramName), 
                    value = getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$start,  
                    min = getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$min,
                    max = getSliderParams(input$mod.modelSelection.selClassifier, nmbrOfParam)$max
        )
      }
      
    }
  )  
  
      
  # get Hyperparameters for the selected parameters
  getHyperParams <- function(selClassifier)
  {  
    classifierName = moduleRegistry$lookup("MS")$options.classifierNames[[selClassifier]]
    
    # get all Params from grid, to have the default parameters
    # does not work generically, since R converts a data frame with a single colum to a vector
    # when extracting on row:
    # hyperParams <- mod.modelSelection.options.classifierParams[[classifierName]]
    hyperParams <- NULL
    
    #TODO: generic solution would be much better...

    if(classifierName == "knn")
      hyperParams <- data.frame(.k = round(input$exp.hyperparam1))
    if(classifierName == "rpart2")
      hyperParams <- data.frame(.maxdepth = round(input$exp.hyperparam1))
    if(classifierName == "C5.0")
    {
      hyperParams <- moduleRegistry$lookup("MS")$options.classifierParams[[classifierName]][1,]
      hyperParams$.trials <- round(input$exp.hyperparam1)
    }
    if(classifierName == "rf")
      hyperParams <- data.frame(.mtry = round(input$exp.hyperparam1))
    if(classifierName == "xgbTree")
    {
      hyperParams <- moduleRegistry$lookup("MS")$options.classifierParams[[classifierName]][1,]
      hyperParams$.max_depth = input$exp.hyperparam1
      hyperParams$.eta = input$exp.hyperparam2
      hyperParams$.gamma = input$exp.hyperparam3
    }
    if(classifierName == "svmLinear")
      hyperParams <- data.frame(.C = input$exp.hyperparam1)
    if(classifierName == "svmPoly")
      hyperParams <- data.frame(.degree = round(input$exp.hyperparam1), .C=input$exp.hyperparam2, .scale=TRUE)
    if(classifierName == "svmRadial")
      hyperParams <- data.frame(.sigma=input$exp.hyperparam1, .C=input$exp.hyperparam2)
    if(classifierName == "nnet")
      hyperParams <- data.frame(.size=round(input$exp.hyperparam1), .decay=input$exp.hyperparam2)

    invisible(hyperParams)
  } 
  
  
  #Module classification: conf. matrix (on button click)
  output$exp.test <- renderPrint ( 
    { 
      str <- "accuracy on train set: "
      cmtr <- startModelExploration()$results$cmTrain
      str <- paste0(str, round(cmtr$overall["Accuracy"], 3))

      str <- paste0(str, "                 accuracy on test set: ")
      cmte <- startModelExploration()$results$cmTest
      str <- paste0(str, round(cmte$overall["Accuracy"], 3))
      
      print(str)
      #invisible(str)
    }
  )   
  
  
  #Module classification: conf. matrix (on button click)
  output$exp.confusionMatrix <- renderPrint ( 
    { 
      startModelExploration()$results$cmTest
    }
  ) 
  
  # Module Classification: Model
  output$exp.model <- renderPrint ( 
    { 
      startModelExploration()$results$model 
    } 
  )
  
  # Module Classification: CM
  output$exp.confusionMatrixTraining <- renderPrint ( 
    { 
      startModelExploration()$results$cmTraining 
    } 
  )
  
  
  # plots decision function of a two-dimensional data set
  plotDecisionFunction <- function(data, model, dims2Plot=c(1,2))
  {
    data2Plot <- data$train_data[,dims2Plot]

    #create grid of artificial test data
    gridResolution <- 50
    valRanges <- NULL
    for(i in seq(1,length(data2Plot)))
    {
      range <- max(data2Plot[,i]) - min(data2Plot[,i])
      valRange <- seq(min(data2Plot[,i]) - range*0.1, max(data2Plot[,i]) + range*0.1, length.out = gridResolution)
      valRanges <- rbind(valRanges, valRange)
    }

    testGrid <- expand.grid(valRanges[1,], valRanges[2,])
    names(testGrid) <- names(data2Plot)
    
    ### classify created test data
    predTestGrid <- predict(model, testGrid)
    predTestGrid <- as.integer(predTestGrid)
    
    ### plot test grid and train data
    plot(testGrid, cex=2, pch=15, col = adjustcolor( getColors(data$nmbrClasses)[predTestGrid], alpha.f=0.2), main="train data and decision function (reduced 2D-space)" )
    points(data2Plot, pch=19, col = getColors(data$nmbrClasses)[data$train_labels], cex=1.5)
    points(data2Plot, pch=1, cex=1.5)
  }

  
  output$exp.explorationPlot <- renderPlot(
    { 
      flow <- doClassificationModelExploration()
      plotDecisionFunction(flow$dat, flow$results$model) 
    }
  )
  
  
  
  
  ############################# About ################################
  
  
  output$about.text <- renderUI(
    { 
#TODO: removed for anon. review ECML workshop
      #Author: Andreas Theissler<br/>
      #  Affiliation: Aalen University of Applied Sciences, 73730 Aalen, Germany <br/>

      #                 A video is available: <a href=\"https://youtu.be/otIkXsi6Hzk\">EduML video</a> "

      
#      <li> Select data set, pre-processing, and classifier in the left pane. </li>
 #       <li> Navigate to either visualization of raw data or classification or experimentation with hyperparameters. </li>
  #      <li> A video is available: <a href=\"https://youtu.be/otIkXsi6Hzk\">EduML video</a> </li>

      
      # video anonym.
      # https://youtu.be/FzKfHLt27cw
              
      str <- "  <br/>
                <h4> EduML: An interactive approach to study and explore machine learning models </h4>
                <br/> <br/>
                Version: 0.9<br/>
                Release date: 15th June 2021<br/>
                <br/>
                A paper was published, allowing to cite EduML 
                <br/> <br/>
                EduML is implemented in R using the packages shiny, caret, lattice, MASS, corrplot, and Rtsne as well as the packages referenced by these. Thanks to the authors for providing such great packages!
                <br/> <br/>
                The weblinks to explanations of the used machine learning models mainly point to articles on <a href=\"https://www.towardsdatascience.com\">towardsdatascience.com</a> 
                <br/> <br/>
                A video is available: <a href=\"https://youtu.be/FzKfHLt27cw\">EduML video</a> "


      HTML(str)
    }
  )

  output$help.text <- renderUI(
    { 
      str <- "  <br/>

      <h4><b>First steps</b></h4>
      <b> EduML is an interactive tool that allows to explore machine learning models, in specific supervised models, i.e. classifiers.
      <a href=\"https://towardsdatascience.com/supervised-learning-basics-of-classification-and-main-algorithms-c16b06806cd3\">(classification explained)</a>
      <br/>
 
      Target users are students, practitioners and engineers wanting to learn about and explore common machine learning models. 
      <br/>
      <br/>
      Links to explanations on free web resources are given in the text, e.g. on towardsdatascience.com
      <br/>
      <br/>
       
      <ol>
      <li> Select data set, pre-processing, and classifier in the left pane. </li>
      <li> Navigate to either visualization of raw data or classification or experimentation with hyperparameters. </li>
      <li> A video is available: <a href=\"https://youtu.be/FzKfHLt27cw\">EduML video</a> </li>

      </ol>
      
      </b>
      <br/>


      <iframe width=\"720\" height=\"405\" src=\"https://www.youtube.com/embed/FzKfHLt27cw\" 
          frameborder=\"0\" allow=\"accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture\" allowfullscreen>
      </iframe>
      
      <br/>
      <br/>
      
      <h4><b>Workflow pane (left)</b></h4>
      The workflow definition pane on the left lets you configure a classification pipeline. A change of the options triggers updates of visualizations and models to promptly show the effects. The following steps can be configured:
      <br/>

      <h5>Data acquisition:</h5>
      Different types of data sets can be selected to explore pros and cons of different models and data preparation steps. 
      From an easily separable artificial 2 clusters data set to the XOR data set and real data sets from the UCI machine learning repository like iris, glass or pima. The attributes to be used can be selected.
      
      <h5>Pre-processing:</h5> 
      In order to understand the effect of scaling the input data, z-score, min/max-scaling, or no scaling can be applied.
      <a href=\"https://towardsdatascience.com/the-influence-of-data-scaling-on-machine-learning-algorithms-fbee9181d497\">scaling explained</a>

     
      <h5>Transformation:</h5> 
      The data set can optionally be projected onto two dimensions using PCA, IDA, t-SNE
      <a href=\"https://towardsdatascience.com/understanding-pca-fae3e243731d\">PCA explained</a>
      <a href=\"https://towardsdatascience.com/independent-component-analysis-ica-in-python-a0ef0db0955e\">ICA explained</a>
      <a href=\"https://towardsdatascience.com/t-sne-clearly-explained-d84c537f53a\">t-SNE explained</a>



      <h5>Model selection:</h5> The models were carefully selected to allow for experimentation with different families of classifiers: 

      <ul>
      <li> linear models like LDA and a linear soft-margin SVM, with the parameter C regularizing the effect of the slack variables </li>
      <a href=\"https://towardsdatascience.com/linear-discriminant-analysis-lda-101-using-r-6a97217a55a6\">LDA explained</a>
      <br/>
      <a href=\"https://towardsdatascience.com/support-vector-machines-svms-4bcccbd78369\">SVM explained</a>
              
              
      <li> k-nearest neighbours (k-NN) with the inherent parameter k which is the number of neighbours </li>
      <a href=\"https://towardsdatascience.com/k-nearest-neighbors-knn-algorithm-bd375d14eec7\">k-NN explained</a>


      <li> support vector machines (SVM) with RBF kernel, with the hyperparameter C regularizing the effect of the slack variables and sigma being the standard deviation in the RBF kernel </li>
      <a href=\"https://towardsdatascience.com/support-vector-machines-svms-4bcccbd78369\">SVM explained</a>
        

      <li> support vector machines (SVM) with polynomial kernel, with the hyperparameter C regularizing the effect of the slack variables and the degree of the polynomial</li>
      <a href=\"https://towardsdatascience.com/support-vector-machines-svms-4bcccbd78369\">SVM explained</a>

      <li> decision trees (CART) with the parameter specifying the maximum depth of the tree </li>
      <a href=\"https://towardsdatascience.com/decision-tree-algorithm-explained-83beb6e78ef4\">decision tree explained</a>

      <li> random forests which is an ensemble of trees, with the parameter mtry which is the number of attributes randomly sampled at each split </li>
      <a href=\"https://towardsdatascience.com/understanding-random-forest-58381e0602d2\">random forest explained</a>

      
      <li> XGBoost which is a ensemble of trees found by boosting, where the hyperparameters </li>
            max_depth refers to the maximum depth of the trees, eta is the learning rate, and gamma minimum loss reduction  </li>
            <br/>
      <a href=\"https://towardsdatascience.com/a-beginners-guide-to-xgboost-87f5d4c30ed7\">XGBoost explained</a>

      <li> artificial neural networks (ANN) with one hidden layer and the parameter size which corresponds to the number of nodes in the hidden layer </li>
      <a href=\"https://towardsdatascience.com/understanding-neural-networks-19020b758230\">artificial neural networks explained</a>

      <li> the rule-based methods OneRule and JRip </li>
      <a href=\"https://link.springer.com/referenceworkentry/10.1007%2F978-0-387-39940-9_559\">rule-based classification explained</a>

      </ul>

  
  
      
      <br/> <br/>
      <h4><b>Output pane (right)</b></h4>

      The specified workflow is reactively applied with each change and the results and visualizations are shown in the output pane on the right. The following subpanes can be viewed:
      <br/>

      <h5>Visualization pane:</h5>
      The user can explore the raw data set using different types of visualizations. The value ranges and distribution of features become obvious using box plots and density plots, correlations are investigated using a scatter plot matrix. This allows the user to evaluate class separability and hence understand the complexity of the classification problem. Additionally, the necessity of scaling or transformation methods can be explored.
      
      <h5>Classification pane:</h5>
      The selected classifier is trained on the specified features, scaling method and transformation method. The model is fitted to the training data by tuning the hyperparameters using k-fold cross validation or bootstrap. The best model is then used to classify the data set and the modelâs parameters can be viewed. In addition, the confusion matrix and classification metrics on the training and blind test set are reported.
      
      <h5>Experimentation pane:</h5> 
      The experimentation pane allows to explore the effect of in-/decreasing hyperparameters of the selected model.
      For an intuitive model-user-loop an x/y-plot is used. For data sets with  more than two dimensions, two features can be selected or the data can be projected onto a two-dimensional space using PCA. Otherwise the first two features are used.
      The model's hyperparameters can be altered using sliders with prompt feedback: the model is re-trained and the updated decision function is visualized showing the parameterâs effect on the shape of the decision function. The results on the training and test set are updated.
      By selecting different types of models, the flexibility of decision functions can be understood: the linear decision functions of models like LDA and linear SVM vs. the piecewise-linear decision functions of tree- and rule-based models vs. the fully flexible decision functions of ANNs, Kernel SVMs and k-NN. 
      This allows to experiment with underfitting vs. overfitting.
      <br/>

      "
      
      HTML(str)
    }
  )

  
})
