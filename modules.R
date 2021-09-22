#machine learning packages
library(caret)
library(e1071)
library(randomForest)
library(kernlab)
library(RWeka)
library(rpart)
library(nnet)
library(xgboost)
library(Rtsne)
library(fastICA)

# R6 classes for object-oriented programming
library(R6) #https://r6.r-lib.org/



###### data types to communicate between steps in the sequence of modules
dataFlowTemplate <- list(data=NULL, labels=NULL, nmbrClasses=NULL,train_data=NULL, train_labels=NULL, test_data=NULL, test_labels=NULL)
resultsTemplate <- list (cmTraining=NULL, cmTest=NULL, test_data_predictions=NULL, model=NULL)
flowTemplate <- list(dat = dataFlowTemplate, results=resultsTemplate)


#######################################################################################
###       MODULES
#######################################################################################


####################### BaseModule (base class (R6) for modules) ######################


# <EXTENSION POINT: add your own subclass of BasePanel>

BaseModule <- R6Class("BaseModule",
                    public = list(
                      nameUI = "Base Module",  # to be adapted in subclasses
                      shortName = "BM", # to be adapted in subclasses
                      options = NULL, # add options in subclasses
                      createUIPanel = function() {
                          wellPanel(style = style.ModulePanel, 
                                            h5(self$nameUI), 
                                            self$innerPanel()
                                            #fluidRow(
                                              # column(8, h5(self$nameUI)), 
                                              # column(2, checkboxInput(self$shortName, '', TRUE) )
                                             #),
                                             #conditionalPanel(style=style.ConditionalPanel, condition = paste("input.", self$shortName), 
                                             # self$innerPanel()
                                            #)  
                          )
                        },
                      innerPanel = function(){
                        #override in subclasses
                      },
                      selectOption = function(selectedOption){
                        #override in subclasses
                      },
                      process = function(){
                        #override in subclasses
                      },
                      
                      toSrcCodeR = function(){
                        ""
                        #override in subclasses
                      },
                      toSrcCodePy = function(){
                        ""
                        #override in subclasses
                      }
                    ) # end of public members
)

########################## FeatureSelectionModule #############################




FeatureSelectionModule <- R6Class("FeatureSelectionModule",
                                  inherit = BaseModule,
                                  
                                  # public members
                                  public = list(
                                    nameUI = "Data acquisition",  
                                    shortName = "DA",
                                    options = list(
                                        # data sets with class columns
                                        # created data

                                        "twoClusters" = 3,
                                        "twoClusters_stretched" = 3,
                                        "XOR" = 3,
                                        "XOR_stretched" = 3,
                                        "circle" = 3,
                                        "circle_stretched" = 3,
                                        
                                        #from UCI ML repo
                                        "wine_red" = 1,
                                        "wine_white" = 12,
                                        "glass" = 10,
                                        "wifi" = 8,
                                        
                                        # from package datasets
                                        "iris" = 5,  
                                        "Orange" = 1, 
                                        "whiteside" = 1, 
                                        
                                        # from package MASS
                                        "Pima.tr" = 8
                                      ),
                                    optionSelectedDataset = NA,
                                    
                                    innerPanel = function(){
                                      wellPanel(style = style.StepPanel,
                                                selectInput("mod.featureSelection.selDataset", 
                                                            label = "Data set",
                                                            choices = as.list(names(self$options)),   
                                                            selected = names(self$options)[1]
                                                ),
                                                uiOutput("mod.featureSelection.selectColumns")
                                      )
                                    },
                                    process = function(flow){
                                      ### split into train and test set
                                      # stratified random split (p: perc. of the data for the train set)
                                      
                                      set.seed(1) # just for reproducibility
                                      
                                      train_idx <- createDataPartition(y = flow$dat$labels, p = 0.6, list = FALSE)
                                      
                                      flow$dat$train_data <- flow$dat$data[train_idx, ]
                                      flow$dat$train_labels <- flow$dat$labels[train_idx]
                                      
                                      flow$dat$test_data <- flow$dat$data[-train_idx, ]
                                      flow$dat$test_labels <- flow$dat$labels[-train_idx]
                                      
                                      invisible(flow)
                                    },
                                    
                                    toSrcCodeR = function(){
                                      strData <- 
"
##### split into train and test set #####
data <- iris[,-5]  # put in your data here...
labels <- iris[,5] # put in your labels here...

train_idx <- createDataPartition(y = labels, p = 0.6, list = FALSE)

train_data <- data[train_idx, ]
train_labels <- labels[train_idx]

test_data <- data[-train_idx, ]
test_labels <- labels[-train_idx]
"
                                      
                                    },
                                    toSrcCodePy = function(){
                                      strData <-
"
iris = datasets.load_iris()
X, y = iris.data, iris.target
train_data, test_data, train_labels, test_labels = train_test_split(X, y, test_size=0.5, random_state=123)
"

                                    }
                                    
                                  )# end of public members
)



############################ ScalingModule #####################################

ScalingModule <- R6Class("ScalingModule",
                      inherit = BaseModule,
                      
                      # public members
                      public = list(
                        nameUI = "Scaling",  
                        shortName = "SC",
                        options = list(
                          "none" = NULL,
                          "z-score" = c("center", "scale"),
                          "min/max" = c("range")
                        ),
                        optionSelectedScaling = NULL,
                        
                        innerPanel = function() {
                                        wellPanel(style = style.StepPanel,
                                                selectInput("mod.preprocessing.selNormalization", 
                                                      label = "Scaling method",
                                                      choices = names(self$options),
                                                      selected = names(self$options)[1]
                                                  )
                                          )
                            
                        },
                        
                        selectOption = function(selectedOption){
                          self$optionSelectedScaling <- self$options[[selectedOption]]
                        },

                        process = function(flow){
                          ### Scale input data 
                          # - z-score: center, scale
                          # - min/max: range" :
                          # - none for no scaling
                          if(! is.null(self$optionSelectedScaling[[1]] ))
                          {
                            #determine scaling factors from train set
                            flow$params$norm_factors <- preProcess(flow$dat$train_data, method = self$optionSelectedScaling)  
                            
                            #scale train and test set independently
                            flow$dat$train_data <- predict(flow$params$norm_factors, flow$dat$train_data)
                            flow$dat$test_data <- predict(flow$params$norm_factors, flow$dat$test_data) 
                          }
                          else
                          {
                            flow$params$norm_factors <- "none"
                          }
                          
                          invisible(flow)
                          
                          },
                        
                        toSrcCodeR = function(){
                          strPreprocessing <- ""
                          if(!is.null(self$optionSelectedScaling) && self$optionSelectedScaling != "none")
                          {
                            if(self$optionSelectedScaling == "range")
                              self$optionSelectedScaling <- "c(\"range\")"
                            if(self$optionSelectedScaling == "center")
                              self$optionSelectedScaling <- "c(\"center\", \"scale\")"
                            strPreprocessing <- paste0(
"
##### scaling of input data #####
#determine scaling factors from train set
norm_factors <- preProcess(train_data, method = ", self$optionSelectedScaling, ")  
                                          
#scale train and test set independently
train_data <- predict(norm_factors, train_data)
test_data <- predict(norm_factors, test_data)
"
                            )
                          }
                        },

                        toSrcCodePy = function(){
                          strPreprocessing <- ""
                          if(!is.null(self$optionSelectedScaling) && self$optionSelectedScaling != "none")
                          {
                            if(self$optionSelectedScaling == "center"){
                              #z-score scaling
                              strPreprocessing <- paste0(
                                "
#scaling of input data 
scaler = StandardScaler().fit(train_data)

train_data = scaler.transform(train_data)
test_data = scaler.transform(test_data)
"
                              )
                            }else if(self$optionSelectedScaling == "range"){
                              strPreprocessing <- paste0(
"
#scaling of input data
scaler = MinMaxScaler().fit(train_data)

train_data = scaler.transform(train_data)
test_data = scaler.transform(test_data)
"
                              )
                            }
                          }
                        }
                        
                      )# end of public members
)


############################ TransformationModule #####################################

DimReductionModule <- R6Class("DimReductionModule",
                               inherit = BaseModule,
                               
                               # public members
                               public = list(
                                 nameUI = "Dimensionality reduction",  
                                 shortName = "DR",
                                 options = list(
                                     "none" = NULL,
                                     "pca" = "pca",
                                     "ica" = "ica",
                                     "t-sne" = "t-sne"
                                   ),
                                 
                                 optionSelectedDimRed = NA,
                                 
                                 innerPanel = function() {
                                       wellPanel(style = style.StepPanel, 
                                                       selectInput("mod.transformation.selTransformationMethod", 
                                                             label = "Projection method to 2D",
                                                             choices = names(self$options),
                                                             selected = names(self$options)[1]
                                                       )
                                               )
                                 },
                                 
                                 selectOption = function(selectedOption){
                                   self$optionSelectedDimRed <- self$options[[selectedOption]]
                                 },
                                 
                                 process = function(flow){
                                   if(!is.null(self$optionSelectedDimRed[[1]] ))
                                   {
                                     
                                     if(self$optionSelectedDimRed[[1]] == "t-sne" ){
                                       res_tsne <- Rtsne(rbind(flow$dat$train_data, flow$dat$test_data), dims = 2, 
                                                         check_duplicates = FALSE, perplexity=30, verbose=FALSE, max_iter = 200)
                                       
                                       proj_data <- as.data.frame(res_tsne$Y)
                                       flow$dat$train_data <- proj_data[1:nrow(flow$dat$train_data),]
                                       flow$dat$test_data <- proj_data[(nrow(flow$dat$train_data)+1) : nrow(proj_data),]
                                       
                                     }else{
                                       preProc <- preProcess(flow$dat$train_data, method=self$optionSelectedDimRed, pcaComp = 2, n.comp =2)
                                       flow$dat$train_data <- predict(preProc, flow$dat$train_data)
                                       flow$dat$test_data <- predict(preProc, flow$dat$test_data)
                                     }
                                   }
                                   
                                   invisible(flow)
                                 },
                                 
                                 toSrcCodeR = function(){
                                   strTransformation <- ""
                                   
                                   if(!is.null(self$optionSelectedDimRed) && self$optionSelectedDimRed != "none")
                                   {
                                     if(self$optionSelectedDimRed == "t-sne" ){
                                       
                                       strTransformation <- 
"
##### projection of data #####
library(Rtsne)  # requires additional package Rtsne
res_tsne <- Rtsne(rbind(train_data, test_data), dims = 2, 
                  check_duplicates = FALSE, perplexity=30, verbose=FALSE, max_iter = 200)

proj_data <- as.data.frame(res_tsne$Y)
train_data <- proj_data[1:nrow(train_data),]
test_data <- proj_data[(nrow(train_data)+1) : nrow(proj_data),]
"
                                     }else{
                                       strTransformation <- paste0(
"
##### projection of data #####
preProc <- preProcess(train_data, method = \"", self$optionSelectedDimRed, "\", pcaComp = 2, n.comp = 2)
train_data <- predict(preProc, train_data)
test_data <- predict(preProc, test_data)
"
                                       )
                                     }
                                   }
                                   
                                 },
                                 toSrcCodePy = function(){
                                   strTransformation <- ""

                                   if(!is.null(self$optionSelectedDimRed) && self$optionSelectedDimRed != "none")
                                   {
                                     if(self$optionSelectedDimRed == "t-sne"){

                                       strTransformation <-
self$optionSelectedTrainingMethod
"
##### projection of data #####
tsne = TSNE().fit_transform(train_data)

"
                                     }else if (self$optionSelectedDimRed == "pca"){
                                       strTransformation <- paste0(
"
##### projection of data #####
pca = PCA().fit_transform(train_data)

"
                                       )
                                     }else if (self$optionSelectedDimRed == "ica"){
                                       strTransformation <- paste0(
                                         "
##### projection of data #####
fastIca = FastICA().fit_transform(train_data)

"
                                       )
                                     }
                                   }

                                 }
                                 
                               )# end of public members
)



############################ ClassificationBaseModule #####################################

paramsLog = c(2^(-6:3)) # range for hyperparameters

ClassificationBaseModule <- R6Class("ClassificationBaseModule",
                                inherit = BaseModule,
                                
                                # public members
                                public = list(
                                  # classifiers: UI names and caret names
                                  #all available models: https://topepo.github.io/caret/available-models.html
                                  options.classifierNames = list(
                                    "k-NN" = "knn",
                                    "Linear Discriminant Analysis" = "lda",  #pack MASS
                                    "Quadratic Discriminant Analysis" = "qda",   #pack MASS
                                    "Decision Tree (CART)" = "rpart2",
                                    "Random forest" = "rf",  #pack. randomForest
                                    "SVM (linear, soft-margin)" = "svmLinear", #kernlab
                                    "SVM (polynomial, soft-margin)" = "svmPoly",#kernlab
                                    "SVM (RBF, soft-margin)" = "svmRadial", #kernlab
                                    "Neural network (one hidden layer)" = "nnet", #single-hidden-layer neural network, package nnet
                                    "One rule" = "OneR",   #package RWeka
                                    "Rule base (JRip)" = "JRip" #package RWeka
                                  ),
                                  # order of hyperparameters is relevant, the first N are shown in the model exploration UI
                                  options.classifierParams = list(
                                    "lda" = NULL,
                                    "qda" = NULL, 
                                    "knn"  =  expand.grid(.k = seq(1,7,1)),
                                    "rpart2" = expand.grid(.maxdepth = seq(1,7,1)),
                                    "rf" =  expand.grid(.mtry = seq(1,5,1)),
                                    "svmLinear" = expand.grid(.C=paramsLog),
                                    "svmRadial" = expand.grid(.sigma = paramsLog, .C=paramsLog),
                                    "svmPoly" = expand.grid(.degree = seq(1,15,2), .C=paramsLog, .scale=TRUE),
                                    "nnet" = expand.grid(.size = c(1, 5, 10, 25), .decay = c(0, 0.4, 0.8)), # size: nodes in hidden layer
                                    "OneR" = NULL,
                                    "JRip" = NULL
                                  ),
                                  options.trainingMethods = list(
                                    "k-fold CV" = "cv",
                                    "boot strap" = "boot" 
                                  ),
                                  optionSelectedClassifierName = NA,
                                  optionSelectedTrainingMethod = NA,
                                  
                                  innerPanel = function() {
                                    wellPanel(style = style.StepPanel, 
                                              selectInput("mod.modelSelection.selClassifier", 
                                                          label = "Classifier",
                                                          choices = names(self$options.classifierNames),
                                                          selected = names(self$classifierNames)[3]
                                              ),
                                              selectInput("mod.modelSelection.selTrainingMethod", 
                                                          label = "Parameter tuning",
                                                          choices = names(self$options.trainingMethods),
                                                          selected = names(self$options.trainingMethods)[1]
                                              )
                                    )
                                  },
                                  
                                  selectOption = function(selectedOption){
                                    if(length(selectedOption) != 2){
                                      stop("Incorrect number of parameters when selecting options of module")
                                    }
                                    self$optionSelectedClassifierName <- self$options.classifierNames[[selectedOption[1]]]
                                    self$optionSelectedTrainingMethod <- self$options.trainingMethods[[selectedOption[2]]]
                                  }

                                )# end of public members
)


############################ ModelSelectionModule #####################################

ModelSelectionModule <- R6Class("ModelSelectionModule",
                                inherit = ClassificationBaseModule,
                                public = list(
                                  nameUI = "Model selection",  
                                  shortName = "MS",
                                  
                                  innerPanel = function() {
                                                        wellPanel(style = style.StepPanel, 
                                                                  selectInput("mod.modelSelection.selClassifier", 
                                                                              label = "Classifier",
                                                                              choices = names(self$options.classifierNames),
                                                                              selected = names(self$classifierNames)[1]
                                                                  ),
                                                                  selectInput("mod.modelSelection.selTrainingMethod", 
                                                                              label = "Parameter tuning",
                                                                              choices = names(self$options.trainingMethods),
                                                                              selected = names(self$options.trainingMethods)[1]
                                                                  )
                                                        )
                                  },
                                  
                                  process = function(flow){
                                      ### Training 
                                      trainParams <- trainControl(method = self$optionSelectedTrainingMethod, number=3) 
                                      
                                      # hyperparameter space:                                      
                                      tuneGrid <- self$options.classifierParams[[self$optionSelectedClassifierName]] 
                                      
                                      # full parameter search (grid search)
                                      # training with specified hyperparameters
                                      # will optimize on metric = "accuracy", for other metrics change code here... 
                                      flow$results$model <- train(x = flow$dat$train_data, y = flow$dat$train_labels, method = self$optionSelectedClassifierName, 
                                                                  trControl = trainParams, tuneGrid = tuneGrid)
                                      
                                      # alternatively: random search where tuneLength specifies the number of models to test
                                      #  flow$results$model <- train(x = flow$dat$train_data, y = flow$dat$train_labels, method = self$optionSelectedClassifier, 
                                      #                              trControl = trainParams, tuneLength=10)
                                      
                                      # classification -- resubstitution error (on train set! not for error estimation):
                                      predicted_train <- predict(flow$results$model, flow$dat$train_data) 
                                      flow$results$cmTraining <- confusionMatrix(predicted_train, flow$dat$train_labels)
                                      
                                      ### classification -- error on test set
                                      flow$results$test_data_predictions <- predict(flow$results$model, flow$dat$test_data)
                                      flow$results$cmTest <- confusionMatrix(flow$results$test_data_predictions, flow$dat$test_labels)
                                      
                                      invisible (flow)
                                    },
                                  
                                  toSrcCodeR = function(){
                                    strTrainTest <- paste0(
"
##### Training of the classifier \"", self$optionSelectedClassifierName, "\" #####

# classifier parameters
paramsLog <- c(2^(-6:4))
mod.modelSelection.options.classifierParams <- list(
\"lda\" = NULL,
\"qda\" = NULL, 
\"knn\"  =  expand.grid(.k = seq(1,7,1)),
\"rpart2\" = expand.grid(.maxdepth = seq(1,7,1)),
\"rf\" =  expand.grid(.mtry = seq(1,5,1)),
\"svmLinear\" = expand.grid(.C=paramsLog),
\"svmRadial\" = expand.grid(.sigma = paramsLog, .C=paramsLog),
\"svmPoly\" = expand.grid(.degree = seq(1,15,2), .C=paramsLog, .scale=TRUE),
\"nnet\" = expand.grid(.size = c(1, 5, 10, 25), .decay = c(0, 0.4, 0.8)), # size: nodes in hidden layer
\"OneR\" = NULL,
\"JRip\" = NULL
)


trainParams <- trainControl(method = \"", self$optionSelectedTrainingMethod, "\", number=3) 

tuneGrid <- mod.modelSelection.options.classifierParams[[\"", self$optionSelectedClassifierName, "\"]] 

model <- train(x = train_data, y = train_labels, method = \"", self$optionSelectedClassifierName,"\", 
               trControl = trainParams, tuneGrid = tuneGrid)

predicted_train <- predict(model, train_data)  # resubstitution error!
cmTraining <- confusionMatrix(predicted_train, train_labels) 
print(cmTraining)


##### Classification of test set #####
test_data_predictions <- predict(model, test_data)

# Print results on test set
cmTest <- confusionMatrix(test_data_predictions, test_labels)
print(cmTest)
" 
                                          )

                                  },
                                  toSrcCodePy = function(){
                                    plot <- 
"

model.fit(train_data, train_labels)

# classification of test set
predictions = model.predict(test_data)

print(\"### Results on test set: ###\")

acc = accuracy_score(test_labels, predictions)

print(\"Overall accuracy: \", acc)
                                    
print(\"Confusion matrix\")
cm = confusion_matrix(test_labels, predictions)
print(cm)
"
                                    if(self$optionSelectedClassifierName == "knn"){
                                    strTrainTest <- paste0(
"
##### Training of the classifier \"", self$optionSelectedClassifierName, "\" #####
model = KNeighborsClassifier(n_neighbors = 2)
", plot
                                          )
                                    } else if (self$optionSelectedClassifierName == "lda"){
                                      strTrainTest <- paste0 (
"
##### Training of the classifier \"", self$optionSelectedClassifierName, "\" #####
model = LinearDiscriminantAnalysis()
", plot
                                        ) 
                                    } else if (self$optionSelectedClassifierName == "qda"){
                                        strTrainTest <- paste0 (
"
##### Training of the classifier \"", self$optionSelectedClassifierName, "\" #####
model = QuadraticDiscriminantAnalysis()
" , plot
                                        )
                                    } else if (self$optionSelectedClassifierName == "rpart2"){
                                        strTrainTest <- paste0(
"
##### Training of the classifier \"", self$optionSelectedClassifierName, "\" #####
model = DecisionTreeClassifier()
", plot
                                        )
                                      } else if (self$optionSelectedClassifierName == "rf"){
                                        strTrainTest <- paste0 (
"
##### Training of the classifier \"", self$optionSelectedClassifierName, "\" #####
model = RandomForestClassifier()
", plot
                                        )
                                      } else if (self$optionSelectedClassifierName == "svmLinear"){
                                        strTrainTest <- paste0 (
"
##### Training of the classifier \"", self$optionSelectedClassifierName, "\" #####
model = SVC(kernel= 'linear')
", plot
                                          ) 
                                        } else if (self$optionSelectedClassifierName == "svmPoly") {
                                          strTrainTest <- paste0 (
"
##### Training of the classifier \"", self$optionSelectedClassifierName, "\" #####
model = SVC(kernel= 'poly')
", plot
                                          )
                                        } else if (self$optionSelectedClassifierName == "svmRadial"){
                                          strTrainTest <- paste0 (
"
##### Training of the classifier \"", self$optionSelectedClassifierName, "\" #####
model = SVC(kernel= 'rbf')
", plot
                                          )
                                        } else if (self$optionSelectedClassifierName == "nnet"){
                                          strTrainTest <- paste0 (
"
##### Training of the classifier \"", self$optionSelectedClassifierName, "\" #####
model = MLPClassifier(hidden_layer_sizes= 1)
", plot
                                          )
                                        } else if (self$optionSelectedClassifierName == "OneR"){
                                          strTrainTest <- paste0 (
"
##### Training of the classifier \"", self$optionSelectedClassifierName, "\" #####
model = DummyClassifier()
", plot
                                          )
                                        } else if (self$optionSelectedClassifierName == "JRip"){
                                          strTrainTest <- paste0 (
"
##### Training of the classifier \"", self$optionSelectedClassifierName, "\" #####
model = DummyClassifier()
", plot
                                          )
                                        }
                                      
                                    }



                                )# end of public members
)

############################ ModelExplorationModule #####################################

ModelExplorationModule <- R6Class("ModelExplorationModule",
                                inherit = ClassificationBaseModule,
                                
                                # public members
                                public = list(
                                  nameUI = "Model exploration",  
                                  shortName = "ME",

                                  #TODO hyperparams
                                  process = function(flow, hyperParams){
                                    # get first two dimensions
                                    # (only for two-dimensional data, to allow for a simple scatter plot)
                                    train_data2D <- flow$dat$train_data[,1:2]
                                    test_data2D <- flow$dat$test_data[,1:2]
                                    
                                    # training with specified params. will optimize on metric = "accuracy"
                                    # for other metrics change code here..., 
                                    flow$results$model <- train(x = train_data2D,  y = flow$dat$train_labels, 
                                                                method = self$optionSelectedClassifierName, tuneGrid = hyperParams)
                                    
                                    
                                    #print("############# Results on training data : #################")
                                    predicted_train <- predict(flow$results$model, train_data2D)  # resubstitution error!
                                    flow$results$cmTraining <- confusionMatrix(predicted_train, flow$dat$train_labels) # CM-function from caret
                                    
                                    ### 4. Classification
                                    flow$results$test_data_predictions <- predict(flow$results$model, test_data2D)
                                    
                                    ### Print results
                                    flow$results$cmTest <- confusionMatrix(flow$results$test_data_predictions, flow$dat$test_labels)
                                    
                                    invisible (flow)
                                  }
                                )# end of public members
)



##################### Module registry #####################

# usage:
# moduleRegistry$addModule(FeatureSelectionModule$new())
# modBA <- moduleRegistry$lookup("BA")
# ...
# m <- moduleRegistry$getModules()
# ...
# for(m in moduleRegistry$getModules()){
# print(m$shortName)
# }


ModuleRegistry <- R6Class("ModuleRegistry",
                          public = list(
                            addModule = function(module){
                              private$kv_modules[[module$shortName]] <- module
                            },
                            lookup = function(shortName){
                              private$kv_modules[[shortName]]
                            },
                            getModules = function(){
                              return (private$kv_modules)
                            }
                          ),
                          private = list(
                            kv_modules = list()
                          )
)



###########################################################


# <EXTENSION POINT: add the object of your own subclass of BaseModule here>

moduleRegistry <- ModuleRegistry$new()

moduleRegistry$addModule(FeatureSelectionModule$new())
moduleRegistry$addModule(ScalingModule$new())
moduleRegistry$addModule(DimReductionModule$new())
moduleRegistry$addModule(ModelSelectionModule$new())

modExploration <- ModelExplorationModule$new()

