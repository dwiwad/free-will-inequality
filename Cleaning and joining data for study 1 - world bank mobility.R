require(dplyr)
require(foreign)
rm(list = ls()) # clear enviroment
#set working directory to dropbox folder, e.g.:
setwd("C:/Users/merci/Dropbox/Free Will Inequality")

#GDP per capita ########################################

#GDP data were retrieved from the World Bank at: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
#data for GDP per cpita incurrent USD were dowloaded to a csv file.
gdp <- read.csv("world bank gdp per capita.csv")
gdp$gdp_pc<- gdp$X2009 #get data from 2009
gdp$Country<- as.character(gdp$Country.Name)
gdp <- dplyr::select(gdp, Country, gdp_pc) #remove data from other years

#Gini ###########################################

#Gini data were retrieved from the World Bank at: https://data.worldbank.org/indicator/SI.POV.GINI
gini <- read.csv("world bank Gini.csv")
gini$gini<- gini$X2009 #get data from 2009

#if data is missing for 2009, get most recent year avalible before 2009
gini$gini[gini$Country.Name == "Australia"] <- gini$X2008[gini$Country.Name == "Australia"]
gini$gini[gini$Country.Name == "China"] <- gini$X2008[gini$Country.Name == "China"]
gini$gini[gini$Country.Name == "Germany"] <- gini$X2007[gini$Country.Name == "Germany"]
gini$gini[gini$Country.Name == "Japan"] <- gini$X2008[gini$Country.Name == "Japan"]
gini$gini[gini$Country.Name == "New Zealand"] <- 36.2 #taken from CIA factbook (https://www.cia.gov/library/publications/the-world-factbook/rankorder/2172rank.html), as World bank does not have data for New Zealand
gini$gini[gini$Country.Name == "South Africa"] <- gini$X2008[gini$Country.Name == "South Africa"]
gini$gini[gini$Country.Name == "United States"] <- gini$X2007[gini$Country.Name == "United States"]

gini$Country<- as.character(gini$Country.Name)
gini<- dplyr::select(gini, Country, gini) #remove other years from data

#Mobility Data ######################################
mobility <- read.csv("World bank Mobility data.csv")
mobility <- filter(mobility, parent == "avg", child == "all") #select mobility data for average parent across all type of children
mobility<- mobility[which(is.na(mobility$IGEincome) == F), ] #select countries with complete cases 
mobility$Country <- as.character(mobility$countryname)
mobility$Mobility <- mobility$IGEincome
mobility <- dplyr::select(mobility, Country, Mobility, cohort) #remove other variables 


#Free Will - Free will data were retrieved from World Values Survey: http://www.worldvaluessurvey.org/WVSDocumentationWV5.jsp
#data were dowloaded as an SPSS file, to reduce size all variables except V2 (country) andV46 (free will) were removed 
wvs <- read.spss("WV5_Data.sav", to.data.frame = T)
wvs$FW <- as.numeric(wvs$V46)
wvs$Country <- as.character(wvs$V2)
wvs_df<- group_by(wvs, Country) %>% summarise(FW = mean(FW, na.rm = T))
wvs_df$Country[wvs_df$Country == "Great Britain"] <- "United Kingdom" #rename to be consistent with later data



#Remove below if only including one model in paper ---------------------------------------------------------- 
#similar to data for WVS 5, WVS 6, and 3 were retrived from: http://www.worldvaluessurvey.org/WVSContents.jsp
#data were dowloaded as an SPSS file, then all variables expect country and free will were removed. 
wvs_6 <- read.spss("WV6_Data.sav", to.data.frame = T)
wvs_6$FW <- as.numeric(wvs_6$V55)
wvs_6$Country <- as.character(wvs_6$V2)
wvs_6df <- group_by(wvs_6, Country) %>% summarise(FW = mean(FW, na.rm = T))
wvs_6df <- dplyr::filter(wvs_6df, Country %in% c("Philippines")) #select countries missing from WVS 5

wvs_3 <- read.spss("WV3_Data.sav", to.data.frame = T)
wvs_3$FW <- as.numeric(wvs_3$A173)
wvs_3$Country <- as.character(wvs_3$S003)
wvs_3df <- group_by(wvs_3, Country) %>% summarise(FW = mean(FW, na.rm = T))
wvs_3df <- dplyr::filter(wvs_3df, Country  %in% c("Croatia", "Slovakia", "Latvia")) #select countries missing from WVS 5
wvs_3df$Country[wvs_3df$Country == "Slovakia", ] <- "Slovak Republic" #rename to match later data

#Add countries missing from WVS 5
Country <- c(wvs_df$Country, wvs_6df$Country, wvs_3df$Country)
FW <- c(wvs_df$FW, wvs_6df$FW, wvs_3df$FW)
wvs_df <- data.frame(Country, FW)
wvs_df$Country <- as.character(wvs_df$Country)
#Remove  above if only including one model in paper ---------------------------------------------------------- 

#ISSP - Support for inequality measure was taken from the ISSP, dowloaded at https://www.gesis.org/issp/modules/issp-modules-by-topic/social-inequality/2009/ 
issp <- read.spss("ISSP survey.sav",  to.data.frame = T)
issp$support_inequality <- as.numeric(issp$V32)
issp$Country <- issp$V4

#Germany is listed twice (East and West) - combine this data:
issp$Country[issp$Country == "DE-E-Germany-East"] <- "DE-W-Germany-West" #Combine everything into west germany (rename to germany below)
  
issp_df <- group_by(issp, Country) %>%
  summarise(support_inequality = mean(support_inequality, na.rm = T), n = n()) #calculate mean

issp_df$Country<- as.character(issp_df$Country)
#rename country names so they do not cause trouble with function used to clean names
issp_df$Country[issp_df$Country == "BE-FLA-Flanders"] <- "FLA-Flanders" 
issp_df$Country[issp_df$Country == "DE-W-Germany-West"] <- "DE-Germany"
issp_df$Country[issp_df$Country == "GB-GBN-Great Britain"] <- "GBN-Great Britain"

remove_initial <- function(x) {strsplit(x, split = "-")[[1]][2]} #function to clean names to match other datasets 
issp_df$Country <- sapply(issp_df$Country, remove_initial)
issp_df$Country[issp_df$Country == "Great Britain"] <- "United Kingdom" #change United Kingdom to Great Britain to match
issp_df <- dplyr::select(issp_df, Country, support_inequality)

#Mobility - data taken from Corack 2016, Figure 1 (p. 11) 
#mobility <- read.csv("mobility.csv")
#mobility$Country <- as.character(mobility$Country)


#merge and analyze 
#order all data so countries are alphabetical 
mobility <- mobility[order(mobility$Country),]
issp_df <- issp_df[order(issp_df$Country),]
wvs_df <- wvs_df[order(wvs_df$Country),]
mobility <- mobility[order(mobility$Country),]
gini <- gini[order(gini$Country),]
gdp <- gdp[order(gdp$Country),]


#add gdp to mobility data
mobility_merged <-inner_join(mobility, gdp, by = "Country")
mobility$Country == mobility_merged$Country #lose Tiwan 

#add gini
mobility_merged <-inner_join(mobility_merged, gini, by = "Country")

#add issp
#match country names 
issp_df[issp_df$Country == "Flanders", ]$Country <- "Belgium"
mobility_merged[mobility_merged$Country == "Russian Federation", ]$Country <- "Russia"

mobility_merged<- inner_join(mobility_merged, issp_df, by = "Country")

#add WVS
mobility_merged<- inner_join(mobility_merged, wvs_df, by = "Country") 

#write data to csv in working directory:
write.csv(mobility_merged, "FW Study 1 World Bank Data.csv")

#regression matches results of study one:
lm(support_inequality ~ FW + Mobility + gini + gdp_pc, data = mobility_merged) %>% summary()




#Potential model without mobility --------------------------------------------------Delete if only including one model in paper
x <- inner_join(gdp, gini, by = "Country")
y <- inner_join(x, issp_df, by = "Country")
z <- inner_join(y, wvs_df, by = "Country")

lm(support_inequality ~ FW + gini + gdp_pc, data = z) %>% summary()


