library(datasets)


####### data sets ########

cntPerClass_ArticialData <- 100
sdev <- 1

createData2Clusters <- function(mean1=2, mean2=5, sd=sdev)
{
  set.seed(123) # for reproducibility
  cnt <- cntPerClass_ArticialData
  c1_x1=rnorm(n=cnt, mean = mean1, sd=sd)
  c1_x2=rnorm(n=cnt, mean = mean1, sd=sd)
  
  c2_x1=rnorm(n=cnt, mean = mean2, sd=sd)
  c2_x2=rnorm(n=cnt, mean = mean2, sd=sd)
  
  x1 <- c(c1_x1,c2_x1)
  x2 <- c(c1_x2,c2_x2)
  
  dat <- data.frame(x1=x1, x2=x2, class = as.factor( c( rep(1,length(c1_x1)), rep(2, length(c2_x1)))) )
  return(dat)
}

createDataXOR <- function(mean1=2, mean2=6,sd=sdev)
{
  set.seed(123) # for reproducibility
  
  #class 1
  cnt <- cntPerClass_ArticialData
  cluster1_x1=rnorm(n=cnt, mean = mean1, sd=sd)
  cluster1_x2=rnorm(n=cnt, mean = mean2, sd=sd)
  
  cluster2_x1=rnorm(n=cnt, mean = mean2, sd=sd)
  cluster2_x2=rnorm(n=cnt, mean = mean1, sd=sd)
  
  x1 <- c(cluster1_x1,cluster2_x1)
  x2 <- c(cluster1_x2,cluster2_x2)
  
  #class 2
  clusters_cl2 <- createData2Clusters(mean1, mean2)
  clusters_cl2$class <- rep(1, length(clusters_cl2$x1))
  
  dat <- data.frame(x1=x1, x2=x2, class = rep(2,length(x1)) )
  dat <- rbind(dat, clusters_cl2)
  dat$class <- as.factor(dat$class)
  return(dat)
}


createDataCircle <- function(sd = sdev)
{
  set.seed(123) # for reproducibility
  cnt <- cntPerClass_ArticialData
  c1_x1=rnorm(n=cnt, mean = 0)
  c1_x2=rnorm(n=cnt, mean = 0)
  
  
  # circle x^2 + y^2 = r^2
  circle=seq(from=0, to=4*pi, 0.2)
  
  c2_x1 <- rep(0, length(circle))
  c2_x2 <- rep(0, length(circle))
  radius <- 4
  
  for(i in 1:length(circle))
  {
    deg <- circle[i]
    
    noisex1 = rnorm(n=1, mean = 0, sd = sd )
    c2_x1[i] <-radius * cos(deg) + noisex1
    
    noisex2 = rnorm(n=1, mean = 0, sd = sd )
    c2_x2[i] <-radius * sin(deg) + noisex2
  }
  
  x1 <- c(c1_x1,c2_x1)
  x2 <- c(c1_x2,c2_x2)
  
  dat <- data.frame(x1=x1, x2=x2, class = as.factor(c( rep(1,length(c1_x1)), rep(2, length(c2_x1)))) )
  return(dat)
}


twoClusters <- createData2Clusters()
XOR <- createDataXOR()
circle <- createDataCircle()

# scale one dimension, to see effects of not scaling the data
twoClusters_stretched <- twoClusters
twoClusters_stretched$x2 <- twoClusters$x2 * 100
XOR_stretched <- XOR
XOR_stretched$x2 <- XOR$x2 * 100
circle_stretched <- circle
circle_stretched$x2 <- circle$x2 * 100

#load red wine data from UCI ML repository (3 classes)
wine_red <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
                     header = FALSE, sep = ",")
names(wine_red) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315","Proline")


#load wine data from UCI ML repository
wine_white <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", 
                       header = TRUE, sep = ";")



wine_white <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", 
                       header = TRUE, sep = ";")


glass <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data", 
                     header = FALSE, sep = ",")
glass <- glass[,-1]  # remove id
names(glass) <- c("RI", "Na", "Mg", "Al", "Si", "K", "Ca", "Ba", "Fe", "class")



wifi <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/00422/wifi_localization.txt", 
                  header = FALSE, sep = "\t")



# write own data for test... write.csv(iris, "iris.csv", quote=FALSE, row.names=FALSE)

loaded_CSV_file <- NULL



