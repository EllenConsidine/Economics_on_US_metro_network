
#ACS migration data:
moves<- read.csv("C:/Users/ellen/OneDrive/MyDocs/CU Fall 2019/Dynamics on Networks/Final_Project/Original Data/metro-to-metro-ins-outs-nets-gross-2013-2017.csv", skip = 2)

Moves<- moves[,c(1,2,3,4,5,7,9,11)]
colnames(Moves)<- c("FIPS A", "FIPS B", "GeoName A", "GeoName B", "B to A", "A to B",
                    "Net Mvmt", "Gross Mvmt")
write.csv(Moves, "C:/Users/ellen/OneDrive/MyDocs/CU Fall 2019/Dynamics on Networks/Final_Project/Project Data/cleaned_moves.csv", row.names = FALSE)

#ACS SES data:
ACS_names<- read.csv("C:/Users/ellen/OneDrive/MyDocs/CU Fall 2019/Dynamics on Networks/Final_Project/Original Data/ACS_metro_2017.csv")
ACS_NAMES<- ACS_names[1,]
rm(ACS_names)

ACS<- read.csv("C:/Users/ellen/OneDrive/MyDocs/CU Fall 2019/Dynamics on Networks/Final_Project/Original Data/ACS_metro_2017.csv", skip = 1)

relevant_cols<- c("Geo_FIPS", "Geo_QName", "SE_A00001_001", "PCT_SE_A02001_003",
                  sapply(2:9, function(x){paste0("PCT_SE_A01001_00", as.character(x))}),
                  sapply(10:13, function(x){paste0("PCT_SE_A01001_0", as.character(x))}), "PCT_SE_A11001_003",
                  "PCT_SE_A12002_002", "PCT_SE_A12002_005", "PCT_SE_A12002_006",
                  "PCT_SE_A17005_003", "SE_A14006_001", "SE_A14028_001", "PCT_SE_A10044_002",
                  "SE_A18009_001", sapply(3:9, function(y){paste0("PCT_SE_A09001_00", as.character(y))}),
                  "PCT_SE_A20001_003", "PCT_SE_A20001_004", "PCT_SE_A20001_005")
relevant_names<- ACS_NAMES[which(t(ACS_NAMES[1,]) %in% relevant_cols)]

my_names<- c("FIPS", "Metro-Name", "Population", "PCT female", "PCT 0-4", "PCT 5-9",
             "PCT 10-14", "PCT 15-17", "PCT 18-24", "PCT 25-34", "PCT 35-44", "PCT 45-54",
             "PCT 55-64", "PCT 65-74", "PCT 75-84", "PCT 85 plus", "PCT married", 
             "PCT less than HS", "PCT BS plus", "PCT MS plus", "PCT unemployed", "Median HH Income",
             "Gini Index", "PCT Occupied Housing", "Median Gross Rent", "Commute under 10",
             "Commute 10-19", "Commute 20-29", "Commute 30-39", "Commute 40-59", "Commute 60-89", 
             "Commute 90 plus", "PCT Health Insured", "PCT Public HI", "PCT Private HI")

my_ACS<- ACS[,relevant_cols]
colnames(my_ACS)<- my_names

my_ACS$FIPS<- sapply(my_ACS$FIPS, function(x){as.integer(substr(x,3,nchar(x)))})

write.csv(my_ACS, "C:/Users/ellen/OneDrive/MyDocs/CU Fall 2019/Dynamics on Networks/Final_Project/Project Data/cleaned_ACS.csv", row.names = FALSE)

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


###MERGE

final<- merge(my_ACS, GDP, by = "FIPS")
