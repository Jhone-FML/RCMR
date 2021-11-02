rm(list=ls());
# memory.limit(102400)
Run_files   <- "E:/Researchs/Papers/Wheat climate suatiable/Run_files/"

setwd(Run_files);

site_inf      <- readxl::read_xls('E:/Researchs/Papers/Wheat climate suatiable/LPsoil.xls')

#site_inf      <- site_inf[site_inf$FCA!=1,]

WHfiles       <- list.files("E:/Researchs/Papers/Wheat climate suatiable/CMIP6/RCP85") 

WHfiles       <- t(data.frame(matrix(unlist(strsplit(WHfiles,  split = '_')), ncol = 8100)))

WHX           <- readLines("C:/DSSAT47/Wheat/CHLP9601.WHX")

Model         <- grep(pattern= "SMODEL", WHX)

substr(WHX[c(Model+1)],72,76) <- 'CSCRP'

write(WHX, "C:/DSSAT47/Wheat/CHLP9601.WHX")
#3667-3672~136

# for (i in 1:nrow(site_inf)){
 for (i in 1:nrow(site_inf)){ 
  # for (i in 1:17){
  # i=1
  
  ID    <- which(WHfiles[,1]==as.character(site_inf[i,4]))
  
  root  <- paste("E:/Researchs/Papers/Wheat climate suatiable/CMIP6/RCP85/",
                 paste(site_inf[i,4], WHfiles[ID,2],WHfiles[ID,3],WHfiles[ID,4], sep='_'),sep='')   
  # for (j in 1)
  for (j in 1:length(root))
  {
    
    # j=1
    
    weatherfiles  <- read.table(root[j], blank.lines.skip = F,skip  = c(32),  stringsAsFactors = F) #!!!!!!!!!!
    
    colnas        <- c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'Hum', 'vp', 'code')
    
    weatherdata            <-  data.frame(matrix(as.numeric(unlist(weatherfiles[-c(1),])), ncol = 9))
    
    colnames(weatherdata)  <-  colnas
    
    weather                <-  weatherdata[weatherdata$year %in% c(c(1970:2011),c(2040:2061), c(2080:2100)), ]
    
    TIM_TMAX <- function(x){
      #x <- weather 
      for(m in 1:length(x$maxt)){
        if(x$mint[m]==x$maxt[m]){
          x$mint[m] <- x$mint[m]-0.1
        }
        else{
          x$mint[m] <- x$mint[m] 
        }
      }
      return(x)
    }
    weather  <- TIM_TMAX(weather)
    
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
      # substr(WHX[SOLID+2],75, 79) <- as.character(site_inf[i,4])
      substr(WHX[SOLID+2],40, 40) <- '1'
    }
    if(site_inf$FCA[i]==2){
      substr(WHX[CULID+2],7, 22) <- '940000 Xiayan22'
      substr(WHX[INCID+2],46, 46) <- '2'
      substr(WHX[INCID+2],55, 55) <- '2'
      substr(WHX[INCID+2],73, 73) <- '2'
      substr(WHX[INCID+2],40, 40) <- '2'
    }
    if(site_inf$FCA[i]==3){
      substr(WHX[CULID+2],7, 22) <- '940001 Changhan'
      substr(WHX[INCID+2],46, 46) <- '3'
      substr(WHX[INCID+2],55, 55) <- '3'
      substr(WHX[INCID+2],73, 73) <- '3'
      substr(WHX[INCID+2],40, 40) <- '3'
    } 
    write(WHX, "C:/DSSAT47/Wheat/CHLP9601.WHX")
    
    
    SMout      <- paste('year', 'site','rcp','gcm', 'sowing','matruity', 
                        'flowering','maturity', 'yield', 'wue', 'rain',
                        'rainday','Tmean', 'SRDA', 'CO2A', 'IRIR' , sep = ';')
    
    write(SMout, file = paste('E:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/CSCRP/RCP8.5/', 
                              site_inf[i,4],'_',WHfiles[ID,2][j],'_',WHfiles[ID,3][j],
                              '_smout.txt', sep = ''))
    
    for (n in c(c(1971:2011),c(2040:2061), c(2080:2100))) {
      # n=1971
      
      templateFileB1 <- readLines("C:/DSSAT47/Weather/CWQX9601.WTH");
      templateFileB2 <- readLines("C:/DSSAT47/Weather/CWQX9701.WTH");
      
      weatherB1 <- weather[weather$year==n-1,];
      weatherB2 <- weather[weather$year==n,];
      
      # sowindow   <-  function(f,w,s,e,r){
      #   # s<- 213
      #   # e <- 293
      #   # r <- weatherB1$pre2020
      #   for (i in c(0:c(e-s-w))) {
      #     rainfall <- sum(r[c(s+i):+(s+i+w)])
      #     if (rainfall> f){
      #       window  <- s+i+w;
      #       break;
      #     }else{window<- e}
      #   }
      #   return(window) 
      # }
      # 
      # # sowindow   <- sowindow(80, 31, 213, 273, weatherB1$pre2020);
      # sowindow   <- sowindow(80, 41, 213, 284, weatherB1$rain);
      # PLTID      <- grep(pattern= "*PLANTING DETAILS", WHX)
      # substr(WHX[PLTID+2],6, 8) <- as.character(sowindow);
      # write(WHX, "C:/DSSAT47/Wheat/CHLP9601.WHX");
      #2====================== put CWGCM data 
      if(length(weatherB1[weatherB1$year==unique(weatherB1$year)[n-1], 2])==365)
      {
        tempData1 <- sprintf('%5.1f',  weatherB1[, 3])
        substr(templateFileB1[6:370], 7, 11) <- tempData1; #Input solar radiation data.
        
        tempData2 <- sprintf('%5.1f',  weatherB1[, 4])
        substr(templateFileB1[6:370], 13, 17) <- tempData2; #Input maximum temperature.
        
        tempData3 <- sprintf('%5.1f',  weatherB1[, 5])
        substr(templateFileB1[6:370], 19, 23) <- tempData3; #Input minimum temperature.
        
        tempData4 <- sprintf('%5.1f', weatherB1[, 6])
        substr(templateFileB1[6:370], 25, 29) <- tempData4; #Input precipitation.
        
        # tempData5 <- sprintf('%5.1f', CO2_RCP8.51)
        # substr(templateFileB1[6:371], 61, 65) <- tempData5; #Input CO2.
        
      }
      if(length(weatherB1[weatherB1$year == unique(weatherB1$year)[n-1], 2]) == 366)
      {
        tempData1 <- sprintf('%5.1f', weatherB1[, 3])
        substr(templateFileB1[6:370], 7, 11) <- tempData1[-1]; #Input solar radiation data.
        
        tempData2 <- sprintf('%5.1f', weatherB1[, 4])
        substr(templateFileB1[6:370], 13, 17) <- tempData2[-1]; #Input maximum temperature.
        
        tempData3 <- sprintf('%5.1f', weatherB1[, 5])
        substr(templateFileB1[6:370], 19, 23) <- tempData3[-1]; #Input minimum temperature.
        
        tempData4 <- sprintf('%5.1f', weatherB1[, 6])
        substr(templateFileB1[6:370], 25, 29) <- tempData4[-1]; #Input precipitation.
        
        # tempData5 <- sprintf('%5.1f', CO2_RCP8.51)
        # substr(templateFileB1[6:371], 61, 65) <- tempData5; #Input CO2.
      }
      
      if(length(weatherB2[weatherB2$year == unique(weatherB2$year)[n],4])==366)
      {
        tempDataA <- sprintf('%5.1f',  weatherB2[, 3])
        substr(templateFileB2[6:370], 7, 11) <- tempDataA[1:365]; #Input solar radiation data.
        
        tempDataB <- sprintf('%5.1f', weatherB2[, 4])
        substr(templateFileB2[6:370], 13, 17) <- tempDataB[1:365]; #Input maximum temperature.
        
        tempDataC <- sprintf('%5.1f', weatherB2[, 5])
        substr(templateFileB2[6:370], 19, 23) <- tempDataC[1:365]; #Input minimum temperature.
        
        tempDataD <- sprintf('%5.1f', weatherB2[, 6])
        substr(templateFileB2[6:370], 25, 29) <- tempDataD[1:365]; #Input precipitation.
        
        # tempDataE <- sprintf('%5.1f', CO2_RCP8.52)
        # substr(templateFileB2[6:370], 61, 65) <- tempDataE[1:365]; #Input CO2.
      }
      if(length(weatherB2[weatherB2$year == unique(weatherB2$year)[n], 2])==365)
      {
        tempDataA <- sprintf('%5.1f',  weatherB2[, 3])
        substr(templateFileB2[6:370], 7, 11) <- tempDataA[1:365]; #Input solar radiation data.
        
        tempDataB <- sprintf('%5.1f', weatherB2[, 4])
        substr(templateFileB2[6:370], 13, 17) <- tempDataB[1:365]; #Input maximum temperature.
        
        tempDataC <- sprintf('%5.1f', weatherB2[, 5])
        substr(templateFileB2[6:370], 19, 23) <- tempDataC[1:365]; #Input minimum temperature.
        
        tempDataD <- sprintf('%5.1f', weatherB2[, 6])
        substr(templateFileB2[6:370], 25, 29) <- tempDataD[1:365]; #Input precipitation.
        
        # tempDataE <- sprintf('%5.1f', CO2_RCP8.52)
        # substr(templateFileB2[6:370], 61, 65) <- tempDataE[1:365]; #Input CO2.
      }   
      
      
      print(n)
      write(templateFileB1, file = "C:/DSSAT47/Weather/CWQX9601.WTH")
      write(templateFileB2, file = "C:/DSSAT47/Weather/CWQX9701.WTH")
      
      
      templateCO2 <- readLines("C:/DSSAT47/StandardData/CO2047.WDA");
      y1 <- n-1
      y2 <- n
      # ######## RCP4.5 Caculations
      # CO2_RCP4.51  <- 650.18+((0.000075326*y1-0.16276)/
      #                 (0.00022299-727.97/(y1^2)))-0.00018747*
      #                 (y1-2045)^3
      # CO2_RCP4.51  <- matrix(round(CO2_RCP4.51,2),nrow = 12)
      # 
      # CO2_RCP4.52  <- 650.18+((0.000075326*y2-0.16276)/
      #                 (0.00022299-727.97/(y2^2)))-0.00018747*
      #                 (y2-2045)^3
      # CO2_RCP4.52  <- matrix(round(CO2_RCP4.52,2),nrow = 12)
      
      # ID1 <- grep(templateCO2, pattern = "1996")
      # ID2 <- grep(templateCO2, pattern = "1997")
      # substr(templateCO2[ID1], 14, 20) <-  sprintf('%2.2f', CO2_RCP4.51);
      # substr(templateCO2[ID2], 14, 20) <-  sprintf('%2.2f', CO2_RCP4.52);
      # write(templateCO2,file = "C:/DSSAT47/StandardData/CO2047.WDA")
      ####### RCP8.5 Caculations
      CO2_RCP8.51  <- 757.44+(84.938-1.537*y1)/(2.2011-3.8289*y1^-0.45242)+2.4712*10^-4*(y1+15)^2+1.9299*10^-5*(y1-1937)^3+5.1137*10^-7*(y1-1910)^4
      
      CO2_RCP8.51   <- matrix(round(CO2_RCP8.51,2),nrow = 12)
      
      
      CO2_RCP8.52  <- 757.44+(84.938-1.537*y2)/(2.2011-3.8289*y2^-0.45242)+2.4712*10^-4*(y2+15)^2+1.9299*10^-5*(y2-1937)^3+5.1137*10^-7*(y2-1910)^4
      
      CO2_RCP8.52   <- matrix(round(CO2_RCP8.52,2),nrow = 12)
      ID1 <- grep(templateCO2, pattern = "1996")
      ID2 <- grep(templateCO2, pattern = "1997")
      substr(templateCO2[ID1], 14, 20) <-  sprintf('%2.2f', CO2_RCP8.51);
      substr(templateCO2[ID2], 14, 20) <-  sprintf('%2.2f', CO2_RCP8.52);
      write(templateCO2,file = "C:/DSSAT47/StandardData/CO2047.WDA")
      
      system("C:/DSSAT47/DSCSM047.EXE WHCER047 B DSSBatch.v47")
      
      print(paste("<--------",i,'_',site_inf[i,4],'_',WHfiles[ID,2][j],'_',
                  WHfiles[ID,3][j], WHfiles[ID,4][j],'-','------>', sep = ''))       
      
      if('Evaluate.OUT'%in%list.files(Run_files)&'Summary.OUT'%in%list.files(Run_files)){
        EvaluateOut <- readLines(paste(Run_files, "Evaluate.OUT", sep = ''), n = -1);
        Summary     <- readLines(paste(Run_files, "Summary.OUT", sep = ''))
        
        
        sowing      <- substr(Summary[5], 111, 117)
        matruity    <- substr(Summary[5], 135, 142)
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
      GCM         <- unique(WHfiles[,3])[j]
      SMDATA      <- paste(year, site,WHfiles[ID,2][j],WHfiles[ID,3][j], 
                             sowing,matruity, flowering, maturity, yield, wue, 
                             rain, rainday, Tmean, SRDA, CO2A, IRIR, sep = ';')
      
      write(SMDATA, file = paste('E:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/CSCRP/RCP8.5/', 
                                 site_inf[i,4],'_',WHfiles[ID,2][j],'_',WHfiles[ID,3][j],
                                 '_smout.txt', sep = ''), append = T)
      
      
    }
    
  }    
  
}

#////////////////////////////////////////////////////////////////////////////////////////////




#@@@@@@@@@@@@@@@@@@@@//////////////////////////////!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////
#================================================= RCP4.5 ===============================
#////////////////////////////////////////////////////////////////////////////////////////

rm(list=ls());

# memory.limit(102400)

Run_files   <- "E:/Researchs/Papers/Wheat climate suatiable/Run_files/"

setwd(Run_files);

site_inf      <- readxl::read_xls('E:/Researchs/Papers/Wheat climate suatiable/LPsoil.xls')

WHfiles       <- list.files("E:/Researchs/Papers/Wheat climate suatiable/CMIP6/RCP45") 

WHfiles       <- t(data.frame(matrix(unlist(strsplit(WHfiles,  split = '_')), ncol = 8100)))

WHX           <- readLines("C:/DSSAT47/Wheat/CHLP9601.WHX")

Model         <- grep(pattern= "SMODEL", WHX)

substr(WHX[c(Model+1)],72,76) <- 'CSCRP'

write(WHX, "C:/DSSAT47/Wheat/CHLP9601.WHX")

for (i in 1:nrow(site_inf)){
  # for (i in 18){ 
  # for (i in 1:17){
  # i=1
  
  ID    <- which(WHfiles[,1]==as.character(site_inf[i,4]))
  
  root  <- paste("E:/Researchs/Papers/Wheat climate suatiable/CMIP6/RCP45/",
                 paste(site_inf[i,4], WHfiles[ID,2],WHfiles[ID,3],WHfiles[ID,4], sep='_'),sep='')   
  # for (j in 1)
  for (j in 1:length(root))
  {
    
    # j=1
    
    weatherfiles  <- read.table(root[j], blank.lines.skip = F,skip  = c(32),  stringsAsFactors = F) #!!!!!!!!!!
    
    colnas        <- c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'Hum', 'vp', 'code')
    
    weatherdata            <-  data.frame(matrix(as.numeric(unlist(weatherfiles[-c(1),])), ncol = 9))
    
    colnames(weatherdata)  <-  colnas
    
    weather                <-  weatherdata[weatherdata$year %in% c(c(1970:2011),c(2040:2061), c(2080:2100)), ]
    
    TIM_TMAX <- function(x){
      #x <- weather 
      for(m in 1:length(x$maxt)){
        if(x$mint[m]==x$maxt[m]){
          x$mint[m] <- x$mint[m]-0.1
        }
        else{
          x$mint[m] <- x$mint[m] 
        }
      }
      return(x)
    }
    weather  <- TIM_TMAX(weather)
    
    WHX      <- readLines("C:/DSSAT47/Wheat/CHLP9601.WHX")
    CULID    <- grep(pattern= "*CULTIVARS", WHX)
    INCID    <- grep(pattern= "*TREATMENTS ", WHX)
    SOLID    <- grep(pattern= "*FIELDS", WHX)
    
    
    
    substr(WHX[SOLID+2],75, 79) <- as.character(site_inf[i,4])
    
    if(site_inf$FCA[i]==1){
      substr(WHX[CULID+2],7, 22) <- '940002 Lingfenn'
      substr(WHX[INCID+2],46, 46) <- '1'
      substr(WHX[INCID+2],55, 55) <- '1'
      substr(WHX[INCID+2],73, 73) <- '1'
    }
    if(site_inf$FCA[i]==2){
      substr(WHX[CULID+2],7, 22) <- '940000 Xiayan22'
      substr(WHX[INCID+2],46, 46) <- '2'
      substr(WHX[INCID+2],55, 55) <- '2'
      substr(WHX[INCID+2],73, 73) <- '2'
    }
    if(site_inf$FCA[i]==3){
      substr(WHX[CULID+2],7, 22) <- '940001 Changhan'
      substr(WHX[INCID+2],46, 46) <- '3'
      substr(WHX[INCID+2],55, 55) <- '3'
      substr(WHX[INCID+2],73, 73) <- '3'
    } 
    write(WHX, "C:/DSSAT47/Wheat/CHLP9601.WHX")
    
    SMout      <- paste('year', 'site','rcp','gcm', 'sowing','matruity', 
                        'flowering','maturity', 'yield', 'wue', 'rain',
                        'rainday','Tmean', 'SRDA', 'CO2A', 'IRIR' , sep = ';')
    
    write(SMout, file = paste('E:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/CSCRP/RCP4.5/', 
                              site_inf[i,4],'_',WHfiles[ID,2][j],'_',WHfiles[ID,3][j],
                              '_smout.txt', sep = ''))
    
    for (n in c(c(1971:2011),c(2040:2061), c(2080:2100))) {
      # n=1971
      
      templateFileB1 <- readLines("C:/DSSAT47/Weather/CWQX9601.WTH");
      templateFileB2 <- readLines("C:/DSSAT47/Weather/CWQX9701.WTH");
      
      weatherB1 <- weather[weather$year==n-1,];
      weatherB2 <- weather[weather$year==n,];
      
      # sowindow   <-  function(f,w,s,e,r){
      #   # s<- 213
      #   # e <- 293
      #   # r <- weatherB1$pre2020
      #   for (i in c(0:c(e-s-w))) {
      #     rainfall <- sum(r[c(s+i):+(s+i+w)])
      #     if (rainfall> f){
      #       window  <- s+i+w;
      #       break;
      #     }else{window<- e}
      #   }
      #   return(window) 
      # }
      # 
      # # sowindow   <- sowindow(80, 31, 213, 273, weatherB1$pre2020);
      # sowindow   <- sowindow(80, 41, 213, 284, weatherB1$rain);
      # PLTID      <- grep(pattern= "*PLANTING DETAILS", WHX)
      # substr(WHX[PLTID+2],6, 8) <- as.character(sowindow);
      # write(WHX, "C:/DSSAT47/Wheat/CHLP9601.WHX");
      #2====================== put CWGCM data 
      if(length(weatherB1[weatherB1$year==unique(weatherB1$year)[n-1], 2])==365)
      {
        tempData1 <- sprintf('%5.1f',  weatherB1[, 3])
        substr(templateFileB1[6:370], 7, 11) <- tempData1; #Input solar radiation data.
        
        tempData2 <- sprintf('%5.1f',  weatherB1[, 4])
        substr(templateFileB1[6:370], 13, 17) <- tempData2; #Input maximum temperature.
        
        tempData3 <- sprintf('%5.1f',  weatherB1[, 5])
        substr(templateFileB1[6:370], 19, 23) <- tempData3; #Input minimum temperature.
        
        tempData4 <- sprintf('%5.1f', weatherB1[, 6])
        substr(templateFileB1[6:370], 25, 29) <- tempData4; #Input precipitation.
        
        # tempData5 <- sprintf('%5.1f', CO2_RCP8.51)
        # substr(templateFileB1[6:371], 61, 65) <- tempData5; #Input CO2.
        
      }
      if(length(weatherB1[weatherB1$year == unique(weatherB1$year)[n-1], 2]) == 366)
      {
        tempData1 <- sprintf('%5.1f', weatherB1[, 3])
        substr(templateFileB1[6:370], 7, 11) <- tempData1[-1]; #Input solar radiation data.
        
        tempData2 <- sprintf('%5.1f', weatherB1[, 4])
        substr(templateFileB1[6:370], 13, 17) <- tempData2[-1]; #Input maximum temperature.
        
        tempData3 <- sprintf('%5.1f', weatherB1[, 5])
        substr(templateFileB1[6:370], 19, 23) <- tempData3[-1]; #Input minimum temperature.
        
        tempData4 <- sprintf('%5.1f', weatherB1[, 6])
        substr(templateFileB1[6:370], 25, 29) <- tempData4[-1]; #Input precipitation.
        
        # tempData5 <- sprintf('%5.1f', CO2_RCP8.51)
        # substr(templateFileB1[6:371], 61, 65) <- tempData5; #Input CO2.
      }
      
      if(length(weatherB2[weatherB2$year == unique(weatherB2$year)[n],4])==366)
      {
        tempDataA <- sprintf('%5.1f',  weatherB2[, 3])
        substr(templateFileB2[6:370], 7, 11) <- tempDataA[1:365]; #Input solar radiation data.
        
        tempDataB <- sprintf('%5.1f', weatherB2[, 4])
        substr(templateFileB2[6:370], 13, 17) <- tempDataB[1:365]; #Input maximum temperature.
        
        tempDataC <- sprintf('%5.1f', weatherB2[, 5])
        substr(templateFileB2[6:370], 19, 23) <- tempDataC[1:365]; #Input minimum temperature.
        
        tempDataD <- sprintf('%5.1f', weatherB2[, 6])
        substr(templateFileB2[6:370], 25, 29) <- tempDataD[1:365]; #Input precipitation.
        
        # tempDataE <- sprintf('%5.1f', CO2_RCP8.52)
        # substr(templateFileB2[6:370], 61, 65) <- tempDataE[1:365]; #Input CO2.
      }
      if(length(weatherB2[weatherB2$year == unique(weatherB2$year)[n], 2])==365)
      {
        tempDataA <- sprintf('%5.1f',  weatherB2[, 3])
        substr(templateFileB2[6:370], 7, 11) <- tempDataA[1:365]; #Input solar radiation data.
        
        tempDataB <- sprintf('%5.1f', weatherB2[, 4])
        substr(templateFileB2[6:370], 13, 17) <- tempDataB[1:365]; #Input maximum temperature.
        
        tempDataC <- sprintf('%5.1f', weatherB2[, 5])
        substr(templateFileB2[6:370], 19, 23) <- tempDataC[1:365]; #Input minimum temperature.
        
        tempDataD <- sprintf('%5.1f', weatherB2[, 6])
        substr(templateFileB2[6:370], 25, 29) <- tempDataD[1:365]; #Input precipitation.
        
        # tempDataE <- sprintf('%5.1f', CO2_RCP8.52)
        # substr(templateFileB2[6:370], 61, 65) <- tempDataE[1:365]; #Input CO2.
      }   
      
      
      print(n)
      write(templateFileB1, file = "C:/DSSAT47/Weather/CWQX9601.WTH")
      write(templateFileB2, file = "C:/DSSAT47/Weather/CWQX9701.WTH")
      
      
      templateCO2 <- readLines("C:/DSSAT47/StandardData/CO2047.WDA");
      y1 <- n-1
      y2 <- n
      ######### RCP4.5 Caculations

      CO2_RCP4.51  <- 650.18+((0.000075326*y1-0.16276)/
                      (0.00022299-727.97/(y1^2)))-0.00018747*
                      (y1-2045)^3
      CO2_RCP4.51  <- matrix(round(CO2_RCP4.51,2),nrow = 12)

      CO2_RCP4.52  <- 650.18+((0.000075326*y2-0.16276)/
                      (0.00022299-727.97/(y2^2)))-0.00018747*
                      (y2-2045)^3
      CO2_RCP4.52  <- matrix(round(CO2_RCP4.52,2),nrow = 12)

      ID1 <- grep(templateCO2, pattern = "1996")
      ID2 <- grep(templateCO2, pattern = "1997")
      substr(templateCO2[ID1], 14, 20) <-  sprintf('%2.2f', CO2_RCP4.51);
      substr(templateCO2[ID2], 14, 20) <-  sprintf('%2.2f', CO2_RCP4.52);
      write(templateCO2,file = "C:/DSSAT47/StandardData/CO2047.WDA")
     
       ####### RCP8.5 Caculations
      # CO2_RCP8.51  <- 757.44+(84.938-1.537*y1)/(2.2011-3.8289*y1^-0.45242)+2.4712*10^-4*(y1+15)^2+1.9299*10^-5*(y1-1937)^3+5.1137*10^-7*(y1-1910)^4
      # 
      # CO2_RCP8.51   <- matrix(round(CO2_RCP8.51,2),nrow = 12)
      # 
      # 
      # CO2_RCP8.52  <- 757.44+(84.938-1.537*y2)/(2.2011-3.8289*y2^-0.45242)+2.4712*10^-4*(y2+15)^2+1.9299*10^-5*(y2-1937)^3+5.1137*10^-7*(y2-1910)^4
      # 
      # CO2_RCP8.52   <- matrix(round(CO2_RCP8.52,2),nrow = 12)
      # ID1 <- grep(templateCO2, pattern = "1996")
      # ID2 <- grep(templateCO2, pattern = "1997")
      # substr(templateCO2[ID1], 14, 20) <-  sprintf('%2.2f', CO2_RCP8.51);
      # substr(templateCO2[ID2], 14, 20) <-  sprintf('%2.2f', CO2_RCP8.52);
      write(templateCO2,file = "C:/DSSAT47/StandardData/CO2047.WDA")
      
      system("C:/DSSAT47/DSCSM047.EXE WHCER047 B DSSBatch.v47")
      
      print(paste("<--------",site_inf[i,4],'_',WHfiles[ID,2][j],'_',
                  WHfiles[ID,3][j], WHfiles[ID,4][j],'-','------>', sep = ''))       
      
      if('Evaluate.OUT'%in%list.files(Run_files)){
        EvaluateOut <- readLines(paste(Run_files, "Evaluate.OUT", sep = ''), n = -1);
        Summary     <- readLines(paste(Run_files, "Summary.OUT", sep = ''))
        
        
        sowing      <- substr(Summary[5], 111, 117)
        matruity    <- substr(Summary[5], 135, 142)
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
      GCM         <- unique(WHfiles[,3])[j]
      SMDATA      <- paste(year, site,WHfiles[ID,2][j],WHfiles[ID,3][j], 
                           sowing,matruity, flowering, maturity, yield, wue, 
                           rain, rainday, Tmean, SRDA, CO2A, IRIR, sep = ';')
      
      write(SMDATA, file = paste('E:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/CSCRP/RCP4.5/', 
                                 site_inf[i,4],'_',WHfiles[ID,2][j],'_',WHfiles[ID,3][j],
                                 '_smout.txt', sep = ''), append = T)
      
      
    }
    
  }    
  
}





















