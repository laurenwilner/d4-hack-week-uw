library(sf)
library(ggplot2)

setwd("H:/Shared drives/Papers 2024/D4 Hack")
ZCTA=st_read("./Data/ZCTA/ZCTA_2010/ZCTA2010.shp")
names(ZCTA)
load("./Data/Census/census_2010.RData")

census_2010$zcta[!census_2010$zcta%in%ZCTA$ZCTA]



ACSclean=function(file){
  file=file[-1,]
  names(file)[2]="ZCTA"
  file$ZCTA=gsub("ZCTA5 ","",file$ZCTA)
  file=file[file$ZCTA%in%ZCTA$ZCTA,]
}










#Five-year estimate at 2012's mid year is 2010 (2009-2012)
#Five-year estimate at 2021's mid year is 2019

Income=read.csv(list.files("./Data/ACS/Median income",pattern="2012.*Data",full.names=T), stringsAsFactors = FALSE)
Income=ACSclean(Income)
names(Income)[3]="MedIncome"
Income


# ZCTA$ZCTA[!ZCTA$ZCTA%in%Income$ZCTA]
# Z_miss=ZCTA[!ZCTA$ZCTA%in%Income$ZCTA,]
# st_write(Z_miss,"./Data/ZCTA_miss.shp")
# census_2010$zcta[!census_2010$zcta%in%Income$ZCTA]
# Z_miss=ZCTA[!census_2010$zcta%in%Income$ZCTA,]
# names(census_2010)


# # Plot all CA ZCTAs in one color (e.g., light blue)
# ggplot() +
#   geom_sf(data = ZCTA, fill = "lightblue", color = "black", alpha = 0.5) +  # All CA ZCTAs in light blue
#     # Overlay the missing ZCTAs in another color (e.g., red)
#   geom_sf(data = Z_miss, fill = "red", color = "black", alpha = 0.8) #+  # Missing ZCTAs in red
#   #   # Add titles and theme
#   # labs(title = "California ZCTAs and Missing ZCTAs",
#   #      subtitle = "All CA ZCTAs in light blue, Missing ZCTAs in red") +
#   # theme_minimal()


Employment=read.csv(list.files("./Data/ACS/Employment",pattern="2012.*Data",full.names=T), stringsAsFactors = FALSE)
Employment=ACSclean(Employment)
#Unemployment among pop in labor force
Employment$UnemployRate=as.numeric(Employment$B23025_005M)/as.numeric(Employment$B23025_002E)

Poverty=read.csv(list.files("./Data/ACS/Poverty",pattern="2012.*Data",full.names=T), stringsAsFactors = FALSE)
Poverty=ACSclean(Poverty)
#Percent Poverty
Poverty$PovertyR=as.numeric(Poverty$B17001_002E)/as.numeric(Poverty$B17001_001E)



Citizenship=read.csv(list.files("./Data/ACS/Citizenship",pattern="2012.*Data",full.names=T), stringsAsFactors = FALSE)
Citizenship=ACSclean(Citizenship)
Citizenship$NCitizenP=as.numeric(Citizenship$B05001_006E)/as.numeric(Citizenship$B05001_001E)


Education=read.csv(list.files("./Data/ACS/Education",pattern="2012.*Data",full.names=T), stringsAsFactors = FALSE)
Education=ACSclean(Education)
Education=Education[,c(1,2,seq(3,53,2))]
Education[, 3:28] <- lapply(Education[, 3:28], as.numeric)

Education$LessHigh= apply(Education[,4:18],1,sum,na.rm=T)/Education$B15003_001E
which(names(Education) == "B15003_016E")
Education$LessCollege= apply(Education[,4:22],1,sum,na.rm=T)/Education$B15003_001E
which(names(Education) == "B15003_020E")


Insurance=read.csv(list.files("./Data/ACS/Insurance",pattern="2013.*Data",full.names=T), stringsAsFactors = FALSE)
Insurance=ACSclean(Insurance)
Insurance[, 3:135] <- lapply(Insurance[, 3:135], as.numeric)
#B27010_055E = Estimate!!Total!!65 years and over!!With one type of health insurance coverage!!With Medicare coverage only
#B27010_051E = 65 up population
#B27010_052E = one type
#B27010_058E =two type
Insurance$ElderNoorMedi=(Insurance$B27010_055E+(Insurance$B27010_051E-Insurance$B27010_052E-Insurance$B27010_058E))/Insurance$B27010_051E

Linguistic=read.csv(list.files("./Data/ACS/Linguistic",pattern="2016.*Data",full.names=T), stringsAsFactors = FALSE)
Linguistic=ACSclean(Linguistic)
Linguistic[, 3:31] <- lapply(Linguistic[, 3:31], as.numeric)

#C16002_001E = Total
#C16002_004E = Estimate!!Total!!Spanish!!Limited English speaking household
#C16002_007E = Estimate!!Total!!Other Indo-European languages!!Limited English speaking household
#C16002_010E = Estimate!!Total!!Asian and Pacific Island languages!!Limited English speaking household
#C16002_013E = Estimate!!Total!!Other languages!!Limited English speaking household
Linguistic$LinIsolationP=(Linguistic$C16002_004E+Linguistic$C16002_007E+Linguistic$C16002_010E+Linguistic$C16002_013E)/Linguistic$C16002_001E


all(Linguistic$ZCTA==Income$ZCTA)

SES=data.frame(ZCTA=Linguistic$ZCTA,
               MedIncome=Income$MedIncome,
               Poverty=Poverty$PovertyR,
               Unemploy=Employment$UnemployRate,
               NCitizen=Citizenship$NCitizenP,
               EduLessHigh=Education$LessHigh,
               EduLessCollege=Education$LessCollege,
               ElderNoInsMedi=Insurance$ElderNoorMedi,
               LinIsolation=Linguistic$LinIsolationP)
write.csv(SES,"./Data/ACS/AllSES.csv")


Internet=read.csv(list.files("./Data/ACS/Internet",pattern="2017.*Data",full.names=T), stringsAsFactors = FALSE)
Internet=ACSclean(Internet)



Mobility=read.csv(list.files("./Data/ACS/Mobility",pattern="2012.*Data",full.names=T), stringsAsFactors = FALSE)
Mobility=ACSclean(Mobility)
