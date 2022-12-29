data<-read_csv("//uwsp.edu/files/CNR/Projects/MuniDashboard/2014 Survey Data/Benchmark Clark Matheny.csv")
data[is.na(data)]<-'NA'

#helper functions for determining the counts of different responses needed to score subsections below
#count the number of existing plans or developing plans regarding the management of trees and other vegetation as described in Sec. 3 Q. 10
count310<-function(x){
  a<-if(x[6]!='N'){1}else{0}
  b<-if(x[7]!='N'){1}else{0}
  c<-if(x[8]!='N'){1}else{0}
  d<-if(x[9]!='N'){1}else{0}
  e<-if(x[10]!='N'){1}else{0}
  f<-if(x[11]!='N'){1}else{0}
  g<-if(x[12]!='N'){1}else{0}
  h<-if(x[13]!='N'){1}else{0}
  i<-if(x[14]!='N'){1}else{0}
  s<-sum(a,b,c,d,e,f,g,h,i)
  return(s)
}

#count the number of areas included in a tree inventory as described in Sec. 6 Q. 6
count66<-function(x){
  a<-if(x[33]=='Y'){1}else{0}
  b<-if(x[34]=='Y'){1}else{0}
  c<-if(x[35]=='Y'){1}else{0}
  d<-if(x[36]=='Y'){1}else{0}
  e<-if(x[37]=='Y'){1}else{0}
  f<-if(x[114]=='Y'){1}else{0}
  s<-sum(a,b,c,d,e,f)
  return(s)
}

#Count the number of select ordinances as described in Sec. 3 Q. 6 and Resource Management Protection of existing trees
count36<-function(x){
  a<-if(x[57]=='Y'){1}else{0}
  b<-if(x[67]=='Y'){1}else{0}
  c<-if(x[68]=='Y'){1}else{0}
  s<-sum(a,b,c)
  return(s)
}

#Count the number of standards followed by a community as described in Sec. 13 Q. 3
count313<-function(x){
  a<-if(x[75]=='1'){1}else{0}
  b<-if(x[76]=='1'){1}else{0}
  c<-if(x[77]=='1'){1}else{0}
  d<-if(x[78]=='1'){1}else{0}
  e<-if(x[79]=='1'){1}else{0}
  s<-sum(a,b,c,d,e)
  return(s)
}

#Count the number of ways tree inspections are done in a community as described by Sec. 7 Q. 15
count715<-function(x){
  a<-if(x[82]=='Y'){1}else{0}
  b<-if(x[83]=='Y'){1}else{0}
  c<-if(x[84]=='Y'){1}else{0}
  d<-if(x[85]=='Y'){1}else{0}
  e<-if(x[86]=='Y'){1}else{0}
  s<-sum(a,b,c,d,e)
  return(s)
}

#Count the number of ways wood/residue is disposed of as described by Sec. 7 Q. 17
count717<-function(x){
  a<-if(x[90]=='Y'){1}else{0}
  b<-if(x[91]=='Y'){1}else{0}
  c<-if(x[92]=='Y'){1}else{0}
  d<-if(x[93]=='Y'){1}else{0}
  e<-if(x[94]=='Y'){1}else{0}
  f<-if(x[95]=='Y'){1}else{0}
  g<-if(x[96]=='Y'){1}else{0}
  s<-sum(a,b,c,d,e,f,g)
  return(s)
}

#Calculate Vegetation Resource Score as described in "Municipal Urban Forestry Scorecard & Sustainability Modles.docx"
#Native Vegetation indicator is not included yet as there are no variables listed to calculate this indicator
scoreVR<-function(x){
  y<-data.frame(FIPS=character(),VRScore=numeric(),stringsAsFactors = FALSE)
  for(i in 1:667){
    s<-((if(x[i,113]=='Y'&(x[i,106]=='Y'|x[i,107]=='Y')&x[i,116]=='Y'){4}
     else if(x[i,113]=='Y'&(x[i,106]=='Y'|x[i,107]=='Y')){3}
     else if(x[i,113]=='Y'){2}
     else if(x[i,113]=='N'){1})+
    (if(x[i,113]=='Y'&x[i,118]=='Y'&x[i,110]=='Y'&x[i,116]=='Y'){4}
     else if(x[i,113]=='Y'&x[i,110]=='Y'&x[i,115]=='Y'){3}
     else if(x[i,113]=='Y'){2}
     else if(x[i,113]=='N'){1})+
    (if(x[i,113]=='Y'&x[i,114]=='Y'&x[i,115]=='Y'&x[i,116]=='Y'){4}
     else if(x[i,113]=='Y'&x[i,114]=='Y'&x[i,115]=='Y'){3}
     else if((x[i,113]=='Y'&x[i,114]=='Y')|x[i,113]=='Y'){2}
     else if(x[i,113]=='N'){1}))
    y[i,]<-c(data$FIPS[i],s)
  }
  return(y)
}

#Calculate Resource Managent Score as described in "Municipal Urban Forestry Scorecard & Sustainability Modles.docx"
scoreRM<-function(x){
  y<-data.frame(FIPS=character(),RMScore=numeric(),stringsAsFactors = FALSE)
  for(i in 1:667){
    s<-((if(x[i,5]=='1'&count310(x[i,])>=7){4}
         else if(x[i,5]=='1'&count310(x[i,])>=5){3.5}
         else if(x[i,5]=='1'&count310(x[i,])>=3){3}
         else if(x[i,5]=='1'&count310(x[i,])>=1){2.5}
         else if(x[i,5]=='1'){2}
         else if(x[i,5]=='3'){1.5}
         else if(x[i,5]=='2'){1}
         else{1})+
        (if(x[i,16]>='70'&x[i,18]=='Y'){4}
         else if(x[i,16]>='40'|x[i,18]=='Y'){3}
         else if(x[i,16]>='40'&x[i,18]=='Y'){2}
         else if(x[i,16]<'40'|x[i,18]=='N'){1})+
        (if(x[i,19]=='1'&x[i,21]=='Y'&(x[i,22]=='Y'|x[i,23]=='Y'|x[i,24]=='Y')){4}
         else if(x[i,19]=='1'&(x[i,21]=='Y')){3}
         else if(x[i,19]=='1'&x[i,26]=='Y'){2.5}
         else if(x[i,19]=='1'&x[i,25]=='Y'){2}
         else if(x[i,19]=='2'){1}
         else{1})+
        (if(x[i,19]=='1'&count66(x[i,])>=2&x[i,116]=='Y'){4}
         else if(x[i,19]=='1'&count66(x[i,])>=2){3}
         else if(x[i,19]=='1'&count66(x[i,])==1){2}
         else if(x[i,19]=='2'){1}
         else{1})+
        (if(x[i,70]=='1'&count36(x[i,])==3){4}
         else if(x[i,70]=='1'&count36(x[i,])==2){3}
         else if(x[i,70]=='1'&count36(x[i,])==1){2}
         else if(x[i,70]=='1'|x[i,70]=='2'|x[i,70]=='3'){1}
         else{1})+
        (if(x[i,70]=='2'&x[i,73]=='Y'&(x[i,71]=='Y'|x[i,72]=='Y')){4}
         else if(x[i,70]=='1'&x[i,71]=='Y'&x[i,72]=='Y'){3.5}
         else if(x[i,70]=='1'&(x[i,71]=='Y'|x[i,72]=='Y')){3}
         else if(x[i,70]=='1'){2}
         else if(x[i,70]=='2'){1}
         else if(x[i,70]=='3'){1.5}
         else{1})+
        (if(count313(x[i,])>=3){4}
         else if(count313(x[i,])==2){3}
         else if(count313(x[i,])==1){2}
         else if(x[i,74]=='1'){1}
         else if(count313(x[i,])==0){1})+
        (if(x[i,80]=='Y'&x[i,81]=='Y'&count715(x[i,])>=3){4}
         else if(x[i,80]=='Y'&x[i,81]=='Y'&count715(x[i,])>=1){3}
         else if(x[i,80]=='Y'&x[i,81]=='N'){2}
         else if(x[i,80]=='N'){1})+
        (if(x[i,88]=='Y'|x[i,89]=='Y'){1}
         else if(count717(x[i,])>=3){4}
         else if(count717(x[i,])==2){3}
         else if(count717(x[i,])==1){2}
         else{1}))
    y[i,]<-c(data$FIPS[i],s)
  }
  return(y)
}

#Calculate Community Framework Score as described in "Municipal Urban Forestry Scorecard & Sustainability Modles.docx"
scoreCF<-function(x){
  y<-data.frame(FIPS=character(),CFScore=numeric(),stringsAsFactors = FALSE)
  for(i in 1:667){
    s<-((if(x[i,98]=='5'){4}
         else if(x[i,98]=='4'){3}
         else if(x[i,98]=='3'){2.5}
         else if(x[i,98]=='2'){2}
         else if(x[i,98]=='1'){1}
         else if(x[i,98]=='6'){0}
         else if(x[i,98]=='NA'){0})+
          (if(x[i,99]=='5'){4}
           else if(x[i,99]=='4'){3}
           else if(x[i,99]=='3'){2.5}
           else if(x[i,99]=='2'){2}
           else if(x[i,99]=='1'){1}
           else if(x[i,99]=='6'){0}
           else if(x[i,99]=='NA'){0})+
          (if(x[i,100]=='5'){4}
           else if(x[i,100]=='4'){3.25}
           else if(x[i,100]=='3'){2.5}
           else if(x[i,100]=='2'){1.25}
           else if(x[i,100]=='1'){1}
           else if(x[i,100]=='6'){0}
           else if(x[i,100]=='NA'){0})+
          (if(x[i,101]=='5'){4}
           else if(x[i,101]=='4'){3}
           else if(x[i,101]=='3'){2.5}
           else if(x[i,101]=='2'){2}
           else if(x[i,101]=='1'){1}
           else if(x[i,101]=='6'){0}
           else if(x[i,101]=='NA'){0})+
          (if(x[i,102]=='5'){4}
           else if(x[i,102]=='4'){3}
           else if(x[i,102]=='3'){2.5}
           else if(x[i,102]=='2'){2}
           else if(x[i,102]=='1'){1}
           else if(x[i,102]=='6'){0}
           else if(x[i,102]=='NA'){0})+
          (if(x[i,103]=='5'){4}
           else if(x[i,103]=='4'){3}
           else if(x[i,103]=='3'){2.5}
           else if(x[i,103]=='2'){2}
           else if(x[i,103]=='1'){1}
           else if(x[i,103]=='6'){0}
           else if(x[i,103]=='NA'){0})+
          (if(x[i,104]=='5'){4}
           else if(x[i,104]=='4'){3}
           else if(x[i,104]=='3'){2.5}
           else if(x[i,104]=='2'){2}
           else if(x[i,104]=='1'){1}
           else if(x[i,104]=='6'){0}
           else if(x[i,104]=='NA'){0}))
    y[i,]<-c(data$FIPS[i],s)
  }
  return(y)
}

#Combine the different subscores for each record
combineScores<-function(x){
  CF<-data.frame(FIPS=character(),CFScore=numeric(),stringsAsFactors = FALSE)
  RM<-data.frame(FIPS=character(),RMScore=numeric(),stringsAsFactors = FALSE)
  VR<-data.frame(FIPS=character(),VRScore=numeric(),stringsAsFactors = FALSE)
  y<-data.frame(FIPS=character(),CombinedScore=numeric(),stringsAsFactors = FALSE)
  CF<-scoreCF(x)
  RM<-scoreRM(x)
  VR<-scoreVR(x)
  CF<-transform(CF,CFScore=as.numeric(CFScore))
  RM<-transform(RM,RMScore=as.numeric(RMScore))
  VR<-transform(VR,VRScore=as.numeric(VRScore))
  for(i in 1:667){
    s<-CF[i,2]+RM[i,2]+VR[i,2]
    y[i,]<-c(data$FIPS[i],s)
  }
  return(y)
}

#get the scores of the 30 cities nearest to you
nearScores<-function(loc){
  y<-data.frame(FIPS=character(),totalScore=numeric(),stringsAsFactors = FALSE)
  s<-data.frame(FIPS=character(),CombinedScore=numeric(),stringsAsFactors = FALSE)
  cities<-vector()
  cities<-fipsNM(loc)
  s<-combineScores(data)
  for(i in 1:30){
    x<-which(cities[i]==s[,1])
    y[i,]<-c(cities[i],s$CombinedScore[x])
  }
  return(y)
}

#return the avg total score and avg subsection scores for each of the closest 30 cities
nearStats<-function(loc){
  y<-vector()
  s<-data.frame(FIPS=character(),totalScore=numeric(),stringsAsFactors = FALSE)
  CF<-data.frame(FIPS=character(),CFScore=numeric(),stringsAsFactors = FALSE)
  RM<-data.frame(FIPS=character(),RMScore=numeric(),stringsAsFactors = FALSE)
  VR<-data.frame(FIPS=character(),VRScore=numeric(),stringsAsFactors = FALSE)
  NCF<-data.frame(FIPS=character(),CFScore=numeric(),stringsAsFactors = FALSE)
  NRM<-data.frame(FIPS=character(),RMScore=numeric(),stringsAsFactors = FALSE)
  NVR<-data.frame(FIPS=character(),VRScore=numeric(),stringsAsFactors = FALSE)
  CF<-scoreCF(data)
  RM<-scoreRM(data)
  VR<-scoreVR(data)
  s<-nearScores(loc)
  for(i in 1:30){
    x<-which(s[i,1]==CF[,1])
    NCF[i,]<-c(CF$FIPS[x],CF$CFScore[x])
    NRM[i,]<-c(RM$FIPS[x],RM$RMScore[x])
    NVR[i,]<-c(VR$FIPS[x],VR$VRScore[x])
  }
  y<-c(mean(as.numeric(s$totalScore)),mean(as.numeric(NVR$VRScore)),mean(as.numeric(NCF$CFScore)),mean(as.numeric(NRM$RMScore)))
  return(y)
}

#return the average total score for closest cities
nearAvg<-function(loc){
  s<-data.frame(FIPS=character(),totalScore=numeric(),stringsAsFactors = FALSE)
  s<-nearScores(loc)
  y<-mean(as.numeric(s$totalScore))
  return(y)
}

#return the average veg res score for the closest cities
nearVRAvg<-function(loc){
  VR<-data.frame(FIPS=character(),VRScore=numeric(),stringsAsFactors = FALSE)
  NVR<-data.frame(FIPS=character(),VRScore=numeric(),stringsAsFactors = FALSE)
  VR<-scoreVR(data)
  cities<-vector()
  cities<-fipsNM(loc)
  for(i in 1:30){
    x<-which(cities[i]==VR[,1])
    NVR[i,]<-c(VR$FIPS[x],VR$VRScore[x])
  }
  y<-mean(as.numeric(NVR$VRScore))
  return(y)
}

#return the average res management score for the closest cities
nearRMAvg<-function(loc){
  RM<-data.frame(FIPS=character(),RMScore=numeric(),stringsAsFactors = FALSE)
  NRM<-data.frame(FIPS=character(),RMScore=numeric(),stringsAsFactors = FALSE)
  RM<-scoreRM(data)
  cities<-vector()
  cities<-fipsNM(loc)
  for(i in 1:30){
    x<-which(cities[i]==RM[,1])
    NRM[i,]<-c(RM$FIPS[x],RM$RMScore[x])
  }
  y<-mean(as.numeric(NRM$RMScore))
  return(y)
}

#return the average comm fram score for the closest cities
nearCFAvg<-function(loc){
  CF<-data.frame(FIPS=character(),CFScore=numeric(),stringsAsFactors = FALSE)
  NCF<-data.frame(FIPS=character(),CFScore=numeric(),stringsAsFactors = FALSE)
  CF<-scoreCF(data)
  cities<-vector()
  cities<-fipsNM(loc)
  for(i in 1:30){
    x<-which(cities[i]==CF[,1])
    NCF[i,]<-c(CF$FIPS[x],CF$CFScore[x])
  }
  y<-mean(as.numeric(NCF$CFScore))
  return(y)
}