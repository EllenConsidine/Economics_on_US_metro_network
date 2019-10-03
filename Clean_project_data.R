
#ACS migration data:
moves<- read.csv("C:/Users/ellen/OneDrive/MyDocs/CU Fall 2019/Dynamics on Networks/Final_Project/Original Data/metro-to-metro-ins-outs-nets-gross-2013-2017.csv")

#ACS SES data:
ACS<- read.csv("C:/Users/ellen/OneDrive/MyDocs/CU Fall 2019/Dynamics on Networks/Final_Project/Original Data/ACS_metro_2017.csv")

#BEA GDP data:
BEA1<- read.csv("C:/Users/ellen/OneDrive/MyDocs/CU Fall 2019/Dynamics on Networks/Final_Project/Original Data/2001_advance-2012/gmpRGDP.csv")
BEA2<- read.csv("C:/Users/ellen/OneDrive/MyDocs/CU Fall 2019/Dynamics on Networks/Final_Project/Original Data/2012_advance-2017_realGDP.csv", skip = 2)

BEA1<- BEA1[which(BEA1$IndustryId == 1),-18]
BEA2<- BEA2[,-8]

BEA1$GeoName<- sapply(BEA1$GeoName, function(x){strsplit(as.character(x), " \\(")[[1]][1]})

for(i in 2:11){
  old<- as.numeric(as.character((BEA1[,paste0("X", as.character(2000+i-1))])))
  new<- as.numeric(as.character(BEA1[,paste0("X", as.character(2000+i))]))
  assign(paste0("Y", as.character(2000+i)), round((new-old)/old,4))
}

for(j in 13:17){
  if(j < 17){
    old<- as.numeric(gsub(",", "", as.character((BEA2[,paste0("X", as.character(2000+j-1))]))))
    new<- as.numeric(gsub(",", "", as.character(BEA2[,paste0("X", as.character(2000+j))])))
    assign(paste0("Y", as.character(2000+j)), round((new-old)/old,4))
  }
  else{
    old<- as.numeric(gsub(",", "", as.character((BEA2[,"X2016"]))))
    new<- as.numeric(gsub(",", "", as.character(BEA2[,"X2017."])))
    assign(paste0("Y", as.character(2000+j)), round((new-old)/old,4))
  }
}

R1<- cbind(BEA1$FIPS, BEA1$GeoName, sapply(2:11, function(x){get(paste0("Y", as.character(2000+x)))}))
R2<- cbind(sapply(BEA2$X, as.character), sapply(13:17, function(x){get(paste0("Y", as.character(2000+x)))}))

colnames(R1)<- c("FIPS", "GeoName", sapply(2:11, function(x){paste0("Y", as.character(2000+x))}))
colnames(R2)<- c("GeoName", sapply(13:17, function(x){paste0("Y", as.character(2000+x))}))

GDP<- merge(R1, R2, by = "GeoName")

write.csv(GDP, "C:/Users/ellen/OneDrive/MyDocs/CU Fall 2019/Dynamics on Networks/Final_Project/Project Data/GDP_rates.csv", row.names = FALSE)

