rm(list=ls());
# memory.limit(102400)
Run_files   <- "E:/Researchs/Papers/Wheat climate suatiable/Run_files/"

setwd(Run_files);

site_inf      <- readxl::read_xls('E:/Researchs/Papers/Wheat climate suatiable/LPsoil.xls')

site_inf      <- site_inf[site_inf$FCA==3,]

WHfiles       <-  "E:/Researchs/DATAS/LPWTH Nona/"   

WHX           <- readLines("C:/DSSAT47/Wheat/CHLP9601.WHX")

Model         <- grep(pattern= "SMODEL", WHX)

substr(WHX[c(Model+1)],72,76) <- 'CSCER'

write(WHX, "C:/DSSAT47/Wheat/CHLP9601.WHX")

for (i in 1: nrow(site_inf)) {
  # i=1
  
  WTHRoots  <- paste(WHfiles, site_inf[i,4], '.csv',sep = '')
  
  Linfen <- read.csv(WTHRoots)
  
  # ylen   <- table(Linfen$year)
  # 
  # jds    <- as.numeric(substr(sequence(ylen), c(nchar(ylen)-4),nchar(ylen)))
  # 
  # LAT     <- site_inf$lat[i]
  # COSei   <- -0.4093*cos(2*pi*(jds+10)/365)
  # A       <- sin(LAT*pi/180)*sin(COSei)
  # B       <- cos(LAT*pi/180)*cos(COSei)
  # E0      <- 1+0.033*cos(2*pi*(jds-10)/365);
  # Icpie   <- 1370*E0;
  # Ra      <- (3600*24*Icpie/pi)*((A*acos(-A/B))+B*sqrt(1-(A/B)^2))
  # DL      <- (24/pi)*acos(-(sin(LAT*pi/180)*sin(COSei))/(cos(LAT*pi/180)*cos(COSei)))
  # Itd     <- Ra*(0.25+0.45*Linfen$sunshine_hours/DL)/10^6
  # 
  # Linfen$Itd <- Itd
  # 
  # Linfen$Julday <-jds
  
  
  WHX      <- readLines("C:/DSSAT47/Wheat/CHLP9601.WHX")
  CULID    <- grep(pattern= "*CULTIVARS", WHX)
  INCID    <- grep(pattern= "*TREATMENTS ", WHX)
  SOLID    <- grep(pattern= "*FIELDS", WHX)
  
  # substr(WHX[SOLID+2],75, 79) <- as.character(site_inf[i,4])
  
  if(site_inf$FCA[i]==1){
    substr(WHX[CULID+2],7, 22) <- '940002 Lingfenn'
    substr(WHX[INCID+2],46, 46) <- '1'
    substr(WHX[INCID+2],55, 55) <- '1'
    substr(WHX[INCID+2],73, 73) <- '1'
    substr(WHX[INCID+2],40, 40) <- '1'
    # substr(WHX[SOLID+2],75, 79) <- as.character(site_inf[i,4])
    #substr(WHX[SOLID+2],40, 40) <- '1'
  }
  if(site_inf$FCA[i]==2){
    substr(WHX[CULID+2],7, 22) <- '940000 Xiayan22'
    substr(WHX[INCID+2],46, 46) <- '2'
    substr(WHX[INCID+2],55, 55) <- '2'
    substr(WHX[INCID+2],73, 73) <- '2'
    substr(WHX[INCID+2],40, 40) <- '2'
    # substr(WHX[SOLID+2],40, 40) <- '2'
  }
  if(site_inf$FCA[i]==3){
    substr(WHX[CULID+2],7, 22) <- '940001 Changhan'
    substr(WHX[INCID+2],46, 46) <- '3'
    substr(WHX[INCID+2],55, 55) <- '3'
    substr(WHX[INCID+2],73, 73) <- '3'
    substr(WHX[INCID+2],40, 40) <- '3'
    #substr(WHX[SOLID+2],40, 40) <- '3'
  } 
  write(WHX, "C:/DSSAT47/Wheat/CHLP9601.WHX")
  
  SMout      <- paste('year', 'site','region','gcm', 'sowing','matruity', 
                      'flowering','maturity', 'yield', 'wue', 'rain',
                      'rainday','Tmean', 'SRDA', 'CO2A', 'IRIR' , sep = ';')
  
  write(SMout, file = paste('E:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/CERES-Wheat/Baseline/', 
                            site_inf[i,4], '_smout.txt', sep = ''));
  
  weather <-   Linfen;
  
  YEAR    <- unique(weather$year)
  
  for (n in YEAR[2] : max(YEAR)) {
    
    #n=1973
    
    templateFileB1 <- readLines("C:/DSSAT47/Weather/CWQX9601.WTH");
    templateFileB2 <- readLines("C:/DSSAT47/Weather/CWQX9701.WTH");
    
    weatherB1 <- weather[weather$year==n-1,];
    #weatherB1$Itd[(weatherB1$Itd %in%NA)] <- 12
    
    weatherB2 <- weather[weather$year==n,];
    #weatherB2$Itd[(weatherB2$Itd %in%NA)] <- 12
    
    #2====================== put CWGCM data 
    if(length(weatherB1[weatherB1$year==unique(weatherB1$year)[n-1], 2])==365)
    {
      tempData1 <- sprintf('%5.1f',  weatherB1$radn)
      substr(templateFileB1[6:370], 7, 11) <- tempData1; #Input solar radiation data.
      
      tempData2 <- sprintf('%5.1f',  weatherB1$maxt)
      substr(templateFileB1[6:370], 13, 17) <- tempData2; #Input maximum temperature.
      
      tempData3 <- sprintf('%5.1f',  weatherB1$mint)
      substr(templateFileB1[6:370], 19, 23) <- tempData3; #Input minimum temperature.
      
      tempData4 <- sprintf('%5.1f', weatherB1$pre2020)
      substr(templateFileB1[6:370], 25, 29) <- tempData4; #Input precipitation.
      
      # tempData5 <- sprintf('%5.1f', CO2_RCP8.51)
      # substr(templateFileB1[6:371], 61, 65) <- tempData5; #Input CO2.
      
    }
    if(length(weatherB1[weatherB1$year == unique(weatherB1$year)[n-1], 2]) == 366)
    {
      tempData1 <- sprintf('%5.1f',  weatherB1$radn)
      substr(templateFileB1[6:370], 7, 11) <- tempData1; #Input solar radiation data.
      
      tempData2 <- sprintf('%5.1f',  weatherB1$maxt)
      substr(templateFileB1[6:370], 13, 17) <- tempData2; #Input maximum temperature.
      
      tempData3 <- sprintf('%5.1f',  weatherB1$mint)
      substr(templateFileB1[6:370], 19, 23) <- tempData3; #Input minimum temperature.
      
      tempData4 <- sprintf('%5.1f', weatherB1$pre2020)
      substr(templateFileB1[6:370], 25, 29) <- tempData4; #Input precipitation.
      
      # tempData5 <- sprintf('%5.1f', CO2_RCP8.51)
      # substr(templateFileB1[6:371], 61, 65) <- tempData5; #Input CO2.
    }
    
    if(length(weatherB2[weatherB2$year == unique(weatherB2$year)[n],4])==366)
    {
      tempData1 <- sprintf('%5.1f',  weatherB2$radn)
      substr(templateFileB2[6:370], 7, 11) <- tempData1; #Input solar radiation data.
      
      tempData2 <- sprintf('%5.1f',  weatherB2$maxt)
      substr(templateFileB2[6:370], 13, 17) <- tempData2; #Input maximum temperature.
      
      tempData3 <- sprintf('%5.1f',  weatherB2$mint)
      substr(templateFileB2[6:370], 19, 23) <- tempData3; #Input minimum temperature.
      
      tempData4 <- sprintf('%5.1f', weatherB2$pre2020)
      substr(templateFileB2[6:370], 25, 29) <- tempData4; #Input precipitation.
      
      # tempDataE <- sprintf('%5.1f', CO2_RCP8.52)
      # substr(templateFileB2[6:370], 61, 65) <- tempDataE[1:365]; #Input CO2.
    }
    if(length(weatherB2[weatherB2$year == unique(weatherB2$year)[n], 2])==365)
    {
      tempData1 <- sprintf('%5.1f',  weatherB2$radn)
      substr(templateFileB2[6:370], 7, 11) <- tempData1; #Input solar radiation data.
      
      tempData2 <- sprintf('%5.1f',  weatherB2$maxt)
      substr(templateFileB2[6:370], 13, 17) <- tempData2; #Input maximum temperature.
      
      tempData3 <- sprintf('%5.1f',  weatherB2$mint)
      substr(templateFileB2[6:370], 19, 23) <- tempData3; #Input minimum temperature.
      
      tempData4 <- sprintf('%5.1f', weatherB2$pre2020)
      substr(templateFileB2[6:370], 25, 29) <- tempData4; #Input precipitation.
      
      # tempDataE <- sprintf('%5.1f', CO2_RCP8.52)
      # substr(templateFileB2[6:370], 61, 65) <- tempDataE[1:365]; #Input CO2.
    }   
    
    
    print(n)
    write(templateFileB1, file = "C:/DSSAT47/Weather/CWQX9601.WTH")
    write(templateFileB2, file = "C:/DSSAT47/Weather/CWQX9701.WTH")
    
    
    templateCO2 <- readLines("C:/DSSAT47/StandardData/CO2047.WDA");
    y1 <- n-1
    y2 <- n
    
    CO2_1   <- matrix(rep(350,12),nrow = 12)
    CO2_2   <- matrix(rep(350,12),nrow = 12)
    
    ID1 <- grep(templateCO2, pattern = "1996")
    ID2 <- grep(templateCO2, pattern = "1997")
    
    substr(templateCO2[ID1], 14, 20) <-  sprintf('%2.2f', CO2_1);
    substr(templateCO2[ID2], 14, 20) <-  sprintf('%2.2f', CO2_2);
    write(templateCO2,file = "C:/DSSAT47/StandardData/CO2047.WDA")
    ##@@
    system("C:/DSSAT47/DSCSM047.EXE CSCER047 B DSSBatch.v47")
    # if('Evaluate.OUT'%in%list.files(Run_files)){
    #   soilwat  <- read.table(paste(Run_files, "SoilWat.OUT", sep = ''), skip = 12,
    #                          blank.lines.skip = F, stringsAsFactors = F, header = F);
    #   Soilwat  <- soilwat[,c(1:2,16:19)];
    #   
    # }
    
    
    ##@@
    print(paste("<--------",site_inf[i,4],'-',y1,'-', '------>', sep = ''))       
    
    if('Evaluate.OUT'%in%list.files(Run_files)&'Summary.OUT'%in%list.files(Run_files)){
      EvaluateOut <- readLines(paste(Run_files, "Evaluate.OUT", sep = ''), n = -1);
      Summary     <- readLines(paste(Run_files, "Summary.OUT", sep = ''))
      
      
      sowing      <- substr(Summary[5], 111, 117)
      matruity    <- substr(Summary[5], 135, 141)
      flowering <- substr(EvaluateOut[6], 67, 69)
      maturity  <- substr(EvaluateOut[6], 79, 81)
      yield     <- substr(EvaluateOut[6], 89, 93)
      wue       <- substr(Summary[5], 457, 461)
      
      WHOUT     <- read.table(paste(Run_files,'Weather.OUT', sep = ''),skip = 10,
                              blank.lines.skip = F, stringsAsFactors = F, header = T)
      if( as.numeric(sowing)==-99){
        rain      <- NA
        Tmean     <- NA
        SRDA      <- NA
        CO2A      <- NA
        IRIR      <- NA
        
      }else {
        WHOUT$JD  <- paste(WHOUT$X.YEAR,WHOUT$DOY,sep = '')
        WHOUT     <- WHOUT[which(WHOUT$JD==sowing): nrow(WHOUT),]
        rain      <- sum(WHOUT$PRED)
        rainday   <- length(which(WHOUT$PRED!=0))
        Tmean     <- mean(WHOUT$TMND)
        SRDA      <- sum(WHOUT$SRAD)
        CO2A      <- sum(WHOUT$CO2D)
        IRIR      <- substr(Summary[5], 238, 241) }
      
    } else
    {
      sowing      <- NA
      matruity    <- NA
      flowering <- NA
      maturity  <- NA
      yield     <- NA
      wue       <- NA
      rain      <- NA
      rainday   <- NA
      Tmean     <- NA
      SRDA      <- NA
      CO2A      <- NA
      IRIR      <- NA
      
    }        
    
    year        <- paste(c(n-1), '-',n,sep = '')
    site        <- site_inf[i,4]
    GCM         <- 'baseline'
    SMDATA      <- paste(year, site, site_inf$FCA[i], GCM, 
                         sowing,matruity, flowering, maturity, yield, wue, 
                         rain, rainday, Tmean, SRDA, CO2A, IRIR, sep = ';')
    
    write(SMDATA, file = paste('E:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/CERES-Wheat/Baseline/', 
                               site_inf[i,4], '_smout.txt', sep = ''), append = T)
    
    
  }
  
}  