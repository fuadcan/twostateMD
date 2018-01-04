data_file <- "~/Documents/twostate/data/"
gen_pdat <- function(yearOrRegion){
  
  if(suppressWarnings(!is.na(as.numeric(yearOrRegion)))){year      <- yearOrRegion
  filename  <- paste0(data_file, "madisonFrom-",year,".csv")
  z         <- read.table(filename,header = T,sep = ";")
  z         <- data.matrix(z)} else if(yearOrRegion=="Europe"){
    fname <- paste0(data_file,"mds_Europe-1950.csv")
    z     <- read.table(fname,header = T,sep = ";"); z <- data.matrix(z)
  } else {
    fname1 <- paste0(data_file,"mds_G7-1950.csv")
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
  pnames <- apply(combn(colnames(z),2), 2, function(n) paste0(sort(n),collapse = " - ")) 
  
  
  colnames(pdat) <- pnames
  return(pdat)
  
}

resss_eug7 <- get(load("results/d_Europe+G7_resALL.rda"))
resss_eusp <- get(load("results/d_Europe+S&P_resALL.rda"))
resss_g7sp <- get(load("results/d_G7+S&P_resALL.rda"))

names(resss_eug7) <- colnames(gen_pdat("Europe+G7"))
names(resss_eusp) <- colnames(gen_pdat("Europe+S&P"))
names(resss_g7sp) <- colnames(gen_pdat("G7+S&P"))

whatwehave <- list(colnames(gen_pdat("Europe+G7")),
                   colnames(gen_pdat("Europe+S&P")),
                   colnames(gen_pdat("G7+S&P")))

ress_wehave <- c(resss_eug7,resss_eusp,resss_g7sp)
ress_wehave <- ress_wehave[!duplicated(names(ress_wehave))]

whatwehave <- unlist(whatwehave)
whatwehave <- whatwehave[!duplicated(whatwehave)]

whatweneed <- colnames(gen_pdat(1950))

length(whatwehave) # 780
length(whatweneed) # 4465

pair1950 <- gen_pdat(1950)

whatweneed_afterall <- setdiff(whatweneed,whatwehave)
dataweneed <- pair1950[,colnames(pair1950) %in%whatweneed_afterall]
save(dataweneed, file="dataweneed.rda")

pnamesweneed <- colnames(dataweneed)
rest_ress <- lapply(1:5, function(i) get(load(paste0("results/d_1950_rest_res",i,".rda"))))
rest_ress <- do.call(c,rest_ress)
names(rest_ress) <- pnamesweneed

ress <- c(ress_wehave, rest_ress)
ress <- ress[match(colnames(pair1950), names(ress))]
save(ress,file = "results/d_1950_resALL.rda")

