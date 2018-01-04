source("dlvPath.R")
yearOrRegion <- 1930

plotPath <- function(yearOrRegion,ind){
  # yearOrRegion is the name of data set that parameters are estimated
  # ind is the index number that indicates the pair for which the parameters are estimated
  
  
  if(is.numeric(yearOrRegion)){year <- yearOrRegion
  filename  <- paste0("data/madisonFrom-",year,".csv")
  z         <- read.table(filename,header = T,sep = ";")
  z         <- data.matrix(z)} else
  {fname1 <- paste0("data/mds_G7-1950.csv"); fname2 <- paste0("data/mds_Europe-1950.csv"); fname3 <- paste0("data/mds_S&P-1950.csv")
  z_g7         <- read.table(fname1,header = T,sep = ";"); z_g7 <- data.matrix(z_g7)
  z_eu         <- read.table(fname2,header = T,sep = ";"); z_eu <- data.matrix(z_eu)
  z_sp         <- read.table(fname3,header = T,sep = ";"); z_sp <- data.matrix(z_sp)
  
  
  if(yearOrRegion=="Europe+G7"|yearOrRegion=="G7+Europe")
  {z<- matrix(c(z_g7,z_eu),dim(z_g7)[1],); colnames(z)<- c(colnames(z_g7),colnames(z_eu))} else
    if(yearOrRegion=="Europe+S&P"|yearOrRegion=="S&P+Europe")
    {z<- matrix(c(z_eu,z_sp),dim(z_eu)[1],); colnames(z)<- c(colnames(z_eu),colnames(z_sp))} else
      if(yearOrRegion=="G7+S&P"|yearOrRegion=="S&P+G7")
      {z<- matrix(c(z_g7,z_sp),dim(z_g7)[1],); colnames(z)<- c(colnames(z_g7),colnames(z_sp))} else 
        if(yearOrRegion=="Maddison"){
          filename  <- "data/madisonFromMaxNA2-1950.csv"
          z         <- read.table(filename,header = T,sep = " ")
          z         <- data.matrix(z)
        } else
        {stop("Unknown Country List")}
  if(sum(duplicated(t(z)))!=0){z<- z[,-which(duplicated(t(z)))]}
  }
  
  z <- log(z)
  n <- ncol(z)
  cmbn<- combn(n,2)
  cnames <- colnames(z)
  pPanel <- sapply(1:(n*(n-1)/2), function(i) z[,cmbn[1,i]] - z[,cmbn[2,i]])
  
  ind <- 57
  pairNames <- paste0(cnames[cmbn[,ind]],collapse = "-")
  parms     <- get(load(paste0("results/d_",yearOrRegion,"_resALL.rda")))
  p <- parms[ind,1:6]
  
  thePath <- dlvPath(p,pPanel[,ind])
  
  x <- 1: length(pPanel[,ind])
  y <- thePath[2,]
  
  par(pch=40, col="black") # plotting symbol and color 
  # par(mfrow=c(1,1)) # all plots on one page 
  plot(x, y, type="n") # , main=heading 
  lines(x, y, type="S") 
  # }
  
  # 
  
}