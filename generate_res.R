generate_res <- function(yearOrRegion){
  
  yearOrRegion <- "Europe+G7"
  
  if(suppressWarnings(!is.na(as.numeric(yearOrRegion)))){year      <- yearOrRegion
  filename  <- paste0(data_file, "madisonFrom-",year,".csv")
  z         <- read.table(filename,header = T,sep = ";")
  z         <- data.matrix(z)} else if(yearOrRegion=="Maddison"){
    filename  <- paste0(data_file,"madisonFromMaxNA2-1950.csv")
    z         <- read.table(filename,header = T,sep = " ")
    z         <- data.matrix(z)
  } else {fname1 <- paste0(data_file,"mds_G7-1950.csv")
  fname2 <- paste0(data_file,"mds_Europe-1950.csv")
  fname3 <- paste0(data_file,"mds_S&P-1950.csv")
  z_g7         <- read.table(fname1,header = T,sep = ";"); z_g7 <- data.matrix(z_g7)
  z_eu         <- read.table(fname2,header = T,sep = ";"); z_eu <- data.matrix(z_eu)
  z_sp         <- read.table(fname3,header = T,sep = ";"); z_sp <- data.matrix(z_sp)
  
  
  if(yearOrRegion=="Europe+G7"|yearOrRegion=="G7+Europe")
  {z<- matrix(c(z_g7,z_eu),dim(z_g7)[1],); colnames(z)<- c(colnames(z_g7),colnames(z_eu))} else
    if(yearOrRegion=="Europe+S&P"|yearOrRegion=="S&P+Europe")
    {z<- matrix(c(z_eu,z_sp),dim(z_eu)[1],); colnames(z)<- c(colnames(z_eu),colnames(z_sp))} else
      if(yearOrRegion=="G7+S&P"|yearOrRegion=="S&P+G7")
      {z<- matrix(c(z_g7,z_sp),dim(z_g7)[1],); colnames(z)<- c(colnames(z_g7),colnames(z_sp))} else {stop("Unknown Country List")}
  if(sum(duplicated(t(z)))!=0){z<- z[,-which(duplicated(t(z)))]}
  }
  
  
  pdat   <- apply(combn(ncol(z),2),2, function(x) log(z[,x[1]]) - log(z[,x[2]]))
  pnames <- apply(combn(colnames(z),2), 2, function(n) paste0(n,collapse = " - ")) 
  
  europe_ind <- !sapply(pnames, function(p) grepl(paste0(colnames(z_g7)[c(1,5,7)],collapse = "|"),p))
  res_euDM2  <- get(load("Documents/twostate/results/d_Europe+G7_resALL.rda")) 
  res_euDM2  <- res_euDM2[europe_ind]
  names(res_euDM2) <- names(europe_ind)[europe_ind]
  save(res_euDM2, file="Documents/twostate/results/d_Europe_resALL.rda")
  
  res_euD2   <- get(load("Documents/twostate_d_results/d_Europe+G7_resALL.rda")) 
  res_euD2   <- res_euD2[europe_ind,]
  rownames(res_euD2) <- names(europe_ind)[europe_ind]
  save(res_euD2, file="Documents/twostate_d_results/d_Europe_resALL.rda")
  
  }