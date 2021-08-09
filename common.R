# configuration
RUN_ON_SERVER <- FALSE
RUN_PARALLEL <- RUN_ON_SERVER # parallel execution does not work on server 

library(shiny)
library(UsingR)
library(pals)
library(RColorBrewer)

library(doParallel)
library(parallel)
library(foreach)


#https://r6.r-lib.org/
#install.packages('R6')
library(R6)

source("datasets.R")
source("modules.R")


###### UI styles
#style.H5 <- "h5 { margin-bottom: 0px; font-size: 1em; font-family: 'Lobster', cursive}" #  ; color: blue; }"
style.H5 <- "h5 { margin-bottom: 0px; font-size: 1em; font-weight: bold}" #  ; color: blue; }"
style.ModulePanel <- "background-color: #367fa9 ;padding: 4px;margin: 2px; offset: 2px"
style.StepPanel   <- "background-color: #4590ac; padding: 2px;margin: 2px;offset: 2px"


getColors <- function(n){
  colors <- pals::cols25()
  return (colors)
}


####################################


startParallelRun <- function(){
  #https://topepo.github.io/caret/parallel-processing.html
  # get number of cores
  ncores <- max(1, detectCores() - 1)
  
  print(paste0("Used number of cores for parallel processing: ", ncores))
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  return (cl)
}

stopParallelRun <- function(cl){
  stopCluster(cl)
}


#### this functions runs all workflow steps
classification <- function(flow)
{
  start_time <- Sys.time()
  
  cl <- NULL
  if(RUN_PARALLEL)
    cl <- startParallelRun()

  # run classification workflow
  for(mod in moduleRegistry$getModules()){
    flow <- mod$process(flow)
  }

  if(RUN_PARALLEL)
    stopParallelRun(cl)

  end_time <- Sys.time()
  time_diff <<- end_time - start_time
  
  print(paste0("Elapsed time for training: ", round(time_diff,3), " ", attr(time_diff, "units")))
  
  invisible (flow)
}


exploration <- function(flow, hyperparams)
{
  start_time <- Sys.time()
  
  cl <- NULL
  if(RUN_PARALLEL)
    cl <- startParallelRun()

  # run model exploration workflow
  for(mod in moduleRegistry$getModules()){

    if(class(mod)[1] == "ModelSelectionModule"){
      # model exploration step requires special handling, it has interactive setting, not specified in the pipeline ...
      flow <- modExploration$process(flow, hyperparams) 
    }else{
      flow <- mod$process(flow)
    }
  }

  if(RUN_PARALLEL)
    stopParallelRun(cl)

  end_time <- Sys.time()
  time_diff <<- end_time - start_time
  
  print(paste0("Elapsed time for training: ", round(time_diff,3), " ", attr(time_diff, "units")))

  invisible (flow)
}



extractSourceCodeR <- function(){
  
  strSrcCodeR <- "
##############################################
# R source code extracted from ML pipeline
# Classification uses the caret package
##############################################

#possibly you need to install caret
library(caret)
  "

  for(mod in moduleRegistry$getModules()){
    strSrcCodeR <- paste0(strSrcCodeR, mod$toSrcCodeR())
  }
  
  return (strSrcCodeR)
}

extractSourceCodePy <- function(){

  strSrcCodePy <- "
##############################################
# Python source code extracted from ML pipeline
# Classification uses the scikit-learn package
##############################################

#possibly you need to install scikit-learn
import scikit-learn
  "

  for(mod in moduleRegistry$getModules()){
    strSrcCodePy <- paste0(strSrcCodePy, mod$toSrcCodePy())
  }

  return (strSrcCodePy)
}

