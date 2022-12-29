library(zipcode)
data(zipcode)
library(geosphere)
library(dplyr)

#read in data
surveyData<-read_csv("//uwsp.edu/files/CNR/Projects/MuniDashboard/2014 Survey Data//Benchmark TCUSAzj.csv")
fips<-read_csv("//uwsp.edu/files/CNR/Projects/MuniDashboard/fips.csv")
myzips<-read_csv("//uwsp.edu/files/CNR/Projects/MuniDashboard/zipcodes.csv")
fips$GU_Name<-toupper(fips$GU_Name)

#get the lat and long of a zip code using zipcode package
getlatlong<-function(zip){
  lati<-which(zip==zipcode[,1])
  lat<-zipcode$latitude[lati]
  long<-zipcode$longitude[lati]
  return(c(long,lat))
}

#get the zip code of a fips location
getZip<-function(fip){
  index<-which(fip==fips[,4])
  city<-fips$GU_Name[index]
  state<-fips$State_Abbreviation[index]
  index2<-which(city==myzips[,2] & state==myzips[,3])
  dzip<-myzips$Zipcode[index2[1]]
  return(dzip)
}

#find the distance of all cities in the survey database to chosen zip code
getDist<-function(zip){
  ill<-getlatlong(zip)
  x<-matrix(ncol=2,nrow=667)
  for(i in 1:667){
    dzip<-getZip(surveyData$FIPS[i])
    dll<-getlatlong(as.character(dzip))
    dist<-distHaversine(dll,ill)/1609.34
    x[i,]<-c(surveyData$FIPS[i],dist)
  }
  return(x)
}

#return the closest 30 cities that were included in the survey to the entered zip code
nearMe<-function(zip){
  x<-getDist(zip)
  class(x)<-"numeric"
  ord<-x[order(x[,2],decreasing = TRUE),]
  near<-tail(ord,30)
  near<-near[,1]
  city<-vector()
  for(i in 1:30){
    x<-which(near[i]==fips$FIPS_Code)
    city[i]<-fips$GU_Name[x]
  }
  return(rev(city))
}

#get the fips codes of the closest 30 cities
fipsNM<-function(zip){
  x<-getDist(zip)
  class(x)<-"numeric"
  ord<-x[order(x[,2],decreasing = TRUE),]
  near<-tail(ord,30)
  near<-near[,1]
  res<-vector()
  for(i in 1:30){
    x<-which(near[i]==fips$FIPS_Code)
    res[i]<-fips$FIPS_Code[x]
  }
  return(rev(res))
}


