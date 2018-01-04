# setwd("C:/Users/fuadcan/Documents/CADF_TablesN")
library("rworldmap")
library("maps")
data(countryExData)
cDat <- countryExData[,1:4]
cDat2<-countryExData$Country
euDat<-cDat[grepl("Europe",cDat[,4]),1:4]
dirname <- paste0("C:/Users/",Sys.info()[6],"/Dropbox/FuatHarun/GrowthApplication/")

cntrs<- cDat[which(cDat[,2]%in%cnList),1:2]

clubADF <- unlist(strsplit(c("Brazil-Australia-New Zealand-Portugal-Morocco-Belgium"),"-"))
clubADF <- unlist(strsplit(c("Chile-Canada-India-Turkey-Morocco"),"-"))
clubADF <- unlist(strsplit(c("Sweden-United Kingdom-Australia-Canada-United States-Colombia"),"-"))

clubADF <- unlist(strsplit(c("Western Sahara-Belgium-Morocco-Brazil-Portugal-Australia"),"-"))
clubADF <- unlist(strsplit(c("Western Sahara-Turkey-Canada-Chile-India-Morocco"),"-"))
clubADF <- unlist(strsplit(c("Russia-Mexico-Norway-South Africa-Taiwan"),"-"))
clubADF <- unlist(strsplit(c("United Kingdom-Switzerland-Taiwan"),"-"))
clubADF <- unlist(strsplit(c("Belgium-France-New Zealand"),"-"))
clubADF <- unlist(strsplit(c("Germany-Chile-United Kingdom-Bulgaria-Canada"),"-"))
clubADF <- unlist(strsplit(c("Sri Lanka-Italy-Spain"),"-"))

clubADF <- trimws(unlist(strsplit(c("Finland - Belgium - Netherlands - United Kingdom - Canada"),"-")))
clubADF <- trimws(unlist(strsplit(c("Italy - Spain - Bulgaria - Turkey"),"-")))
clubADF <- trimws(unlist(strsplit(c("Germany - UK - USA"),"-")))
clubADF <- trimws(unlist(strsplit(c("Poland - Austria - Sweden"),"-")))

clubADF <- trimws(unlist(strsplit(c("Poland - United Kingdom - Bulgaria"),"-")))

clubADF <- trimws(unlist(strsplit(c("Turkey - China - India"),"-")))
clubADF <- trimws(unlist(strsplit(c("Austria - Canada - Denmark"),"-")))
clubADF <- trimws(unlist(strsplit(c("Sweden - Finland - New Zealand - Belgium"),"-")))

clubADF <- trimws(unlist(strsplit(c("France - Belgium - Poland"),"-")))
clubADF <- trimws(unlist(strsplit(c("Netherlands - Belgium - Poland - France - Sweden - Finland"),"-")))
clubADF <- trimws(unlist(strsplit(c("Germany - Belgium - United States - Japan"),"-")))


codes <- cDat[match(clubADF,cDat2),1]
cat(clubADF[is.na(codes)])
colorss <- c("blue","red","purple")
# [which(c("ADF","ADF & HF","HF") %in% mlr)]
malDF <- data.frame(country = codes, malaria = rep("ADF",length(codes)))
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",  nameJoinColumn = "country")

xlm= c(50,120);ylm=c(-50,50)
# xlm = NULL;ylm = NULL


png('../Desktop/daily_et2_2.png', width=900, height = 500)

mapCountryData(malMap, nameColumnToPlot="malaria",addLegend = F, catMethod = "categorical",
               oceanCol='gray',xlim= xlm,ylim=ylm, missingCountryCol = gray(.95),
               mapTitle = "",borderCol="black",colourPalette = colorss)
dev.off()
