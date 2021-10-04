#import packages
library(raster)
library(rgdal)
library(spdep)
library(rgeos)
library(reshape2)

setwd("C:/Work/global_salt_marsh/USA/update_5_years/watersheds") #set working directory of folders including clipped rasters created with python script
#list folders
folders = list.dirs(path =getwd())
#iterate through all folders in the directory - corresponding to watersheds in this example
for (folds in folders){
  #list rasters within
  rasters = list.files(path = folds, full.names = TRUE, recursive = TRUE)
  #define all rasters of interest including salt extent
  r2= raster(rasters[grep(pattern='saltmarsh_em', rasters)])
  #anomaly detection rasters
  r4 = raster(rasters[grep(pattern='2000_2004.tif', rasters)])
  r5 = raster(rasters[grep(pattern='2005_2009.tif', rasters)])
  r6 = raster(rasters[grep(pattern='2010_2014.tif', rasters)])
  r7 = raster(rasters[grep(pattern='2015_2019.tif', rasters)])
  #distance from the salt marsh
  r8 = raster(rasters[grep(pattern='euc_em.tif', rasters)])
  #lclu - nlcd in two years
  r9 = raster(rasters[grep(pattern='_2016.tif', rasters)])
  r10 = raster(rasters[grep(pattern='_2001.tif', rasters)])
  #resample nlcd, distance, and salt marsh extent to the anomaly data
  r.new2 = resample(r2, r4, "bilinear")
  r.new4 = resample(r8, r4, "bilinear")
  r.new5 = resample(r9, r4, "ngb")
  r.new6 = resample(r10, r4, "ngb")
  rasters.2 = c(r4,r5,r6,r7,r.new2,r.new4,r.new5,r.new6)
  #stack resampled rasters
  raster1 = brick(rasters.2)
  print(raster1)
  #convert to points
  sp_poly = rasterToPoints(raster1,spatial=FALSE)
  sp_poly = as.data.frame(sp_poly)
  #rename columns
  names(sp_poly)= c("x","y","N2000_2004","N2005_2009","N2010_2014","N2015_2019","SM","Dist","NLCD2016","NLCD2001")
  #filter NAs
  sp_poly=sp_poly[-which(is.na(sp_poly$N2000_2004)),]
  
  #pass if no data in the watershed  
  if(length(sp_poly)==0) next
  #get watershed name 
  watershed = strsplit(rasters[1], "/")[[1]][7]
  #convert sm to character 
  sp_poly$SM = as.character(sp_poly$SM)
  #get total amount of sm in watershed at begining of change analysis
  smtotal = length(sp_poly$N2000_2004[which(sp_poly$SM==1)])
  #non salt marsh within 1.8 km of salt marsh
  sp_poly$Other = 0
  sp_poly$Other[which(is.na(sp_poly$SM))]=1
  total = length(sp_poly$N2000_2004[which(sp_poly$Other==1)])
  
  #the few sections will calculate the amount of change anomalies within various LCLU within and outside the salt marsh extent
  t0004lt100 =  table(sp_poly$NLCD2016[which(sp_poly$N2000_2004<= -0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t0004lt100)!=0){
    t0004lt100=as.data.frame(t0004lt100)
    t0004lt100$dis = 100
    t0004lt100$basin = watershed
    t0004lt100$Years = "2000_2004"
    t0004lt100$Type = "Loss"
    t0004lt100$LCLU="NLCD2016"
  }
  t0004lt1000 =  table(sp_poly$NLCD2016[which(sp_poly$N2000_2004<= -0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t0004lt1000)!=0){
    t0004lt1000=as.data.frame(t0004lt1000)
    t0004lt1000$dis = 1000
    t0004lt1000$basin = watershed
    t0004lt1000$Years = "2000_2004"
    t0004lt1000$Type = "Loss"
    t0004lt1000$LCLU="NLCD2016"
  }
  t0609lt100 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004>=-0.2&sp_poly$N2005_2009<=-0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t0609lt100)!=0){
    t0609lt100=as.data.frame(t0609lt100)
    t0609lt100$dis = 100
    t0609lt100$basin = watershed
    t0609lt100$Years = "2005_2009"
    t0609lt100$Type = "Loss"
    t0609lt100$LCLU="NLCD2016"
  }
  t0609lt1000 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004>=-0.2&sp_poly$N2005_2009<=-0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t0609lt1000)!=0){
    t0609lt1000=as.data.frame(t0609lt1000)
    t0609lt1000$dis = 1000
    t0609lt1000$basin = watershed
    t0609lt1000$Years = "2005_2009"
    t0609lt1000$Type = "Loss"
    t0609lt1000$LCLU="NLCD2016"
  }
  t1114lt100 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004>=-0.2&sp_poly$N2005_2009>=-0.2&sp_poly$N2010_2014<=-0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t1114lt100)!=0){
    t1114lt100=as.data.frame(t1114lt100)
    t1114lt100$dis = 100
    t1114lt100$basin = watershed
    t1114lt100$Years = "2010-2014"
    t1114lt100$Type = "Loss"
    t1114lt100$LCLU="NLCD2016"
    
  }
  t1114lt1000 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004>=-0.2&sp_poly$N2005_2009>=-0.2&sp_poly$N2010_2014<=-0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t1114lt1000)!=0){
    t1114lt1000=as.data.frame(t1114lt1000)
    t1114lt1000$dis = 1000
    t1114lt1000$basin = watershed
    t1114lt1000$Years = "2010-2014"
    t1114lt1000$Type = "Loss"
    t1114lt1000$LCLU="NLCD2016"
  }
  t1519lt100 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004>=-0.2&sp_poly$N2005_2009>=-0.2&sp_poly$N2010_2014>=-0.2&sp_poly$N2015_2019<=-0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t1519lt100)!=0){
    t1519lt100=as.data.frame(t1519lt100)
    t1519lt100$dis = 100
    t1519lt100$basin = watershed
    t1519lt100$Years = "2015-2019"
    t1519lt100$Type = "Loss"
    t1519lt100$LCLU = "NLCD2016"
  }
  t1519lt1000 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004>=-0.2&sp_poly$N2005_2009>=-0.2&sp_poly$N2010_2014>=-0.2&sp_poly$N2015_2019<=-0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t1519lt1000)!=0){
    t1519lt1000=as.data.frame(t1519lt1000)
    t1519lt1000$dis = 1000
    t1519lt1000$basin = watershed
    t1519lt1000$Years = "2015-2019"
    t1519lt1000$Type = "Loss"
    t1519lt1000$LCLU = "NLCD2016"
  }
  
  #all_change100v1000=rbind(t0004lt100,t0004lt1000)
  all_change100v1000=rbind(all_change100v1000,t0004lt100)
  all_change100v1000=rbind(all_change100v1000,t0004lt1000)
  all_change100v1000=rbind(all_change100v1000,t0609lt100)
  all_change100v1000=rbind(all_change100v1000,t0609lt1000)
  all_change100v1000=rbind(all_change100v1000,t1114lt100)
  all_change100v1000=rbind(all_change100v1000,t1114lt1000)
  all_change100v1000=rbind(all_change100v1000,t1519lt100)
  all_change100v1000=rbind(all_change100v1000,t1519lt1000)
  
  t0004gt100 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t0004gt100)!=0){
    t0004gt100=as.data.frame(t0004gt100)
    t0004gt100$dis = 100
    t0004gt100$basin = watershed
    t0004gt100$Years = "2000_2004"
    t0004gt100$Type = "Gain"
    t0004gt100$LCLU="NLCD2016"
    
  }
  t0004gt1000 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t0004gt1000)!=0){
    t0004gt1000=as.data.frame(t0004gt1000)
    t0004gt1000$dis = 1000
    t0004gt1000$basin = watershed
    t0004gt1000$Years = "2000_2004"
    t0004gt1000$Type = "Gain"
    t0004gt1000$LCLU="NLCD2016"
    
  }
  t0509gt100 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004<= 0.2&sp_poly$N2005_2009>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t0509gt100)!=0){
    t0509gt100=as.data.frame(t0509gt100)
    t0509gt100$dis = 100
    t0509gt100$basin = watershed
    t0509gt100$Years = "2005_2009"
    t0509gt100$Type = "Gain"
    t0509gt100$LCLU="NLCD2016"
    
  }
  t0509gt1000 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004<= 0.2&sp_poly$N2005_2009>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t0509gt1000)!=0){
    t0509gt1000=as.data.frame(t0509gt1000)
    t0509gt1000$dis = 1000
    t0509gt1000$basin = watershed
    t0509gt1000$Years = "2005_2009"
    t0509gt1000$Type = "Gain"
    t0509gt1000$LCLU="NLCD2016"
    
  }
  t1014gt100 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004<= 0.2&sp_poly$N2005_2009<= 0.2&sp_poly$N2010_2014>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t1014gt100)!=0){
    t1014gt100=as.data.frame(t1014gt100)
    t1014gt100$dis = 100
    t1014gt100$basin = watershed
    t1014gt100$Years = "2010_2014"
    t1014gt100$Type = "Gain"
    t1014gt100$LCLU="NLCD2016"
    
  }
  t1014gt1000 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004<= 0.2&sp_poly$N2005_2009<= 0.2&sp_poly$N2010_2014>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t1014gt1000)!=0){
    t1014gt1000=as.data.frame(t1014gt1000)
    t1014gt1000$dis = 1000
    t1014gt1000$basin = watershed
    t1014gt1000$Years = "2010_2014"
    t1014gt1000$Type = "Gain"
    t1014gt1000$LCLU="NLCD2016"
    
  }
  t1519gt100 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004<= 0.2&sp_poly$N2005_2009<= 0.2&sp_poly$N2010_2014<= 0.2&sp_poly$N2015_2019>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t1519gt100)!=0){
    t1519gt100=as.data.frame(t1519gt100)
    t1519gt100$dis = 100
    t1519gt100$basin = watershed
    t1519gt100$Years = "2015_2019"
    t1519gt100$Type = "Gain"
    t1519gt100$LCLU="NLCD2016"
    
  }
  t1519gt1000 = table(sp_poly$NLCD2016[which(sp_poly$N2000_2004<= 0.2&sp_poly$N2005_2009<= 0.2&sp_poly$N2010_2014<= 0.2&sp_poly$N2015_2019>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t1519gt1000)!=0){
    t1519gt1000=as.data.frame(t1519gt1000)
    t1519gt1000$dis = 1000
    t1519gt1000$basin = watershed
    t1519gt1000$Years = "2015_2019"
    t1519gt1000$Type = "Gain"
    t1519gt1000$LCLU="NLCD2016"
  }
  
  
  all_change100v1000=rbind(all_change100v1000,t0004gt100)
  all_change100v1000=rbind(all_change100v1000,t0004gt1000)
  all_change100v1000=rbind(all_change100v1000,t0509gt100)
  all_change100v1000=rbind(all_change100v1000,t0509gt1000)
  all_change100v1000=rbind(all_change100v1000,t1014gt100)
  all_change100v1000=rbind(all_change100v1000,t1014gt1000)
  all_change100v1000=rbind(all_change100v1000,t1519gt100)
  all_change100v1000=rbind(all_change100v1000,t1519gt1000)
  
  tallat100 = table(sp_poly$NLCD2016[which(sp_poly$Other == "1"&sp_poly$Dist <= 100)])
  if(length(tallat100)!=0){
    tallat100=as.data.frame(tallat100)
    tallat100$dis = 100
    tallat100$basin = watershed
    tallat100$Years = "2016"
    tallat100$Type = "All"
    tallat100$LCLU="NLCD2016"
    
  }
  tallat1000 = table(sp_poly$NLCD2016[which(sp_poly$Other == "1"&sp_poly$Dist <= 1000)])
  if(length(tallat1000)!=0){
    tallat1000=as.data.frame(tallat1000)
    tallat1000$dis = 1000
    tallat1000$basin = watershed
    tallat1000$Years = "2016"
    tallat1000$Type = "All"
    tallat1000$LCLU="NLCD2016"
  }
  
  all_change100v1000=rbind(all_change100v1000,tallat100)
  all_change100v1000=rbind(all_change100v1000,tallat1000)
  
  ######### NLCD 2001 ##########
  
  t0004lt100 =  table(sp_poly$NLCD2001[which(sp_poly$N2000_2004<= -0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t0004lt100)!=0){
    t0004lt100=as.data.frame(t0004lt100)
    t0004lt100$dis = 100
    t0004lt100$basin = watershed
    t0004lt100$Years = "2000_2004"
    t0004lt100$Type = "Loss"
    t0004lt100$LCLU="NLCD2001"
    
  }
  t0004lt1000 =  table(sp_poly$NLCD2001[which(sp_poly$N2000_2004<= -0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t0004lt1000)!=0){
    t0004lt1000=as.data.frame(t0004lt1000)
    t0004lt1000$dis = 1000
    t0004lt1000$basin = watershed
    t0004lt1000$Years = "2000_2004"
    t0004lt1000$Type = "Loss"
    t0004lt1000$LCLU="NLCD2001"
    
  }
  t0609lt100 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004>=-0.2&sp_poly$N2005_2009<=-0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t0609lt100)!=0){
    t0609lt100=as.data.frame(t0609lt100)
    t0609lt100$dis = 100
    t0609lt100$basin = watershed
    t0609lt100$Years = "2005_2009"
    t0609lt100$Type = "Loss"
    t0609lt100$LCLU="NLCD2001"
    
  }
  t0609lt1000 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004>=-0.2&sp_poly$N2005_2009<=-0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t0609lt1000)!=0){
    t0609lt1000=as.data.frame(t0609lt1000)
    t0609lt1000$dis = 1000
    t0609lt1000$basin = watershed
    t0609lt1000$Years = "2005_2009"
    t0609lt1000$Type = "Loss"
    t0609lt1000$LCLU="NLCD2001"
    
  }
  t1114lt100 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004>=-0.2&sp_poly$N2005_2009>=-0.2&sp_poly$N2010_2014<=-0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t1114lt100)!=0){
    t1114lt100=as.data.frame(t1114lt100)
    t1114lt100$dis = 100
    t1114lt100$basin = watershed
    t1114lt100$Years = "2010-2014"
    t1114lt100$Type = "Loss"
    t1114lt100$LCLU="NLCD2001"
  }
  t1114lt1000 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004>=-0.2&sp_poly$N2005_2009>=-0.2&sp_poly$N2010_2014<=-0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t1114lt1000)!=0){
    t1114lt1000=as.data.frame(t1114lt1000)
    t1114lt1000$dis = 1000
    t1114lt1000$basin = watershed
    t1114lt1000$Years = "2010-2014"
    t1114lt1000$Type = "Loss"
    t1114lt1000$LCLU="NLCD2001"
  }
  t1519lt100 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004>=-0.2&sp_poly$N2005_2009>=-0.2&sp_poly$N2010_2014>=-0.2&sp_poly$N2015_2019<=-0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t1519lt100)!=0){
    t1519lt100=as.data.frame(t1519lt100)
    t1519lt100$dis = 100
    t1519lt100$basin = watershed
    t1519lt100$Years = "2015-2019"
    t1519lt100$Type = "Loss"
    t1519lt100$LCLU="NLCD2001"
  }
  t1519lt1000 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004>=-0.2&sp_poly$N2005_2009>=-0.2&sp_poly$N2010_2014>=-0.2&sp_poly$N2015_2019<=-0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t1519lt1000)!=0){
    t1519lt1000=as.data.frame(t1519lt1000)
    t1519lt1000$dis = 1000
    t1519lt1000$basin = watershed
    t1519lt1000$Years = "2015-2019"
    t1519lt1000$Type = "Loss"
    t1519lt1000$LCLU="NLCD2001"
  }
  
  #all_change2001100v1000=rbind(t0004lt100,t0004lt1000)
  all_change2001100v1000=rbind(all_change2001100v1000,t0004lt100)
  all_change2001100v1000=rbind(all_change2001100v1000,t0004lt1000)
  all_change2001100v1000=rbind(all_change2001100v1000,t0609lt100)
  all_change2001100v1000=rbind(all_change2001100v1000,t0609lt1000)
  all_change2001100v1000=rbind(all_change2001100v1000,t1114lt100)
  all_change2001100v1000=rbind(all_change2001100v1000,t1114lt1000)
  all_change2001100v1000=rbind(all_change2001100v1000,t1519lt100)
  all_change2001100v1000=rbind(all_change2001100v1000,t1519lt1000)
  
  t0004gt100 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t0004gt100)!=0){
    t0004gt100=as.data.frame(t0004gt100)
    t0004gt100$dis = 100
    t0004gt100$basin = watershed
    t0004gt100$Years = "2000_2004"
    t0004gt100$Type = "Gain"
    t0004gt100$LCLU="NLCD2001"
    
  }
  t0004gt1000 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t0004gt1000)!=0){
    t0004gt1000=as.data.frame(t0004gt1000)
    t0004gt1000$dis = 1000
    t0004gt1000$basin = watershed
    t0004gt1000$Years = "2000_2004"
    t0004gt1000$Type = "Gain"
    t0004gt1000$LCLU="NLCD2001"
    
  }
  t0509gt100 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004<= 0.2&sp_poly$N2005_2009>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t0509gt100)!=0){
    t0509gt100=as.data.frame(t0509gt100)
    t0509gt100$dis = 100
    t0509gt100$basin = watershed
    t0509gt100$Years = "2005_2009"
    t0509gt100$Type = "Gain"
    t0509gt100$LCLU="NLCD2001"
    
  }
  t0509gt1000 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004<= 0.2&sp_poly$N2005_2009>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t0509gt1000)!=0){
    t0509gt1000=as.data.frame(t0509gt1000)
    t0509gt1000$dis = 1000
    t0509gt1000$basin = watershed
    t0509gt1000$Years = "2005_2009"
    t0509gt1000$Type = "Gain"
    t0509gt1000$LCLU="NLCD2001"
    
  }
  t1014gt100 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004<= 0.2&sp_poly$N2005_2009<= 0.2&sp_poly$N2010_2014>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t1014gt100)!=0){
    t1014gt100=as.data.frame(t1014gt100)
    t1014gt100$dis = 100
    t1014gt100$basin = watershed
    t1014gt100$Years = "2010_2014"
    t1014gt100$Type = "Gain"
    t1014gt100$LCLU="NLCD2001"
    
  }
  t1014gt1000 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004<= 0.2&sp_poly$N2005_2009<= 0.2&sp_poly$N2010_2014>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t1014gt1000)!=0){
    t1014gt1000=as.data.frame(t1014gt1000)
    t1014gt1000$dis = 1000
    t1014gt1000$basin = watershed
    t1014gt1000$Years = "2010_2014"
    t1014gt1000$Type = "Gain"
    t1014gt1000$LCLU="NLCD2001"
    
  }
  t1519gt100 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004<= 0.2&sp_poly$N2005_2009<= 0.2&sp_poly$N2010_2014<= 0.2&sp_poly$N2015_2019>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 100)])
  if(length(t1519gt100)!=0){
    t1519gt100=as.data.frame(t1519gt100)
    t1519gt100$dis = 100
    t1519gt100$basin = watershed
    t1519gt100$Years = "2015_2019"
    t1519gt100$Type = "Gain"
    t1519gt100$LCLU="NLCD2001"
    
  }
  t1519gt1000 = table(sp_poly$NLCD2001[which(sp_poly$N2000_2004<= 0.2&sp_poly$N2005_2009<= 0.2&sp_poly$N2010_2014<= 0.2&sp_poly$N2015_2019>= 0.2&sp_poly$Other==1&sp_poly$Dist <= 1000)])
  if(length(t1519gt1000)!=0){
    t1519gt1000=as.data.frame(t1519gt1000)
    t1519gt1000$dis = 1000
    t1519gt1000$basin = watershed
    t1519gt1000$Years = "2015_2019"
    t1519gt1000$Type = "Gain"
    t1519gt1000$LCLU="NLCD2001"
    
  }
  
  all_change2001100v1000=rbind(all_change2001100v1000,t0004gt100)
  all_change2001100v1000=rbind(all_change2001100v1000,t0004gt1000)
  all_change2001100v1000=rbind(all_change2001100v1000,t0509gt100)
  all_change2001100v1000=rbind(all_change2001100v1000,t0509gt1000)
  all_change2001100v1000=rbind(all_change2001100v1000,t1014gt100)
  all_change2001100v1000=rbind(all_change2001100v1000,t1014gt1000)
  all_change2001100v1000=rbind(all_change2001100v1000,t1519gt100)
  all_change2001100v1000=rbind(all_change2001100v1000,t1519gt1000)
  
  tallat100 = table(sp_poly$NLCD2001[which(sp_poly$Other == "1"&sp_poly$Dist <= 100)])
  if(length(tallat100)!=0){
    tallat100=as.data.frame(tallat100)
    tallat100$dis = 100
    tallat100$basin = watershed
    tallat100$Years = "2001"
    tallat100$Type = "All"
    tallat100$LCLU="NLCD2001"
    
  }
  tallat1000 = table(sp_poly$NLCD2001[which(sp_poly$Other == "1"&sp_poly$Dist <= 1000)])
  if(length(tallat1000)!=0){
    tallat1000=as.data.frame(tallat1000)
    tallat1000$dis = 1000
    tallat1000$basin = watershed
    tallat1000$Years = "2001"
    tallat1000$Type = "All"
    tallat1000$LCLU="NLCD2001"
    
  }
  
  all_change2001100v1000=rbind(all_change2001100v1000,tallat100)
  all_change2001100v1000=rbind(all_change2001100v1000,tallat1000)
}  setwd("C:/Work/global_salt_marsh/USA/NDWI_analysis")
#write out watershed level change estimates for lclu by sm and other locations
write.csv(all_change2001100v1000,'all_lclu.csv')
write.csv(sm_change2001100v1000,'sm_lclu.csv')