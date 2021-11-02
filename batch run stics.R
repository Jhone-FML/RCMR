###################################################################################################
#################################!!!!!!!!!!!!!!!!!!! Bal_wheat RCP4.5 !!!!!!!!!!!!#################
###################################################################################################
setwd('F:/Researchs/JavaSTICS-v85/')
site_inf      <- readxl::read_xls('F:/Researchs/Papers/Wheat climate suatiable/LPsoil.xls')
#site_inf      <- site_inf[site_inf$FCA==3,]
WHfiles       <- list.files("F:/Researchs/Papers/Wheat climate suatiable/CMIP6/RCP85") 
WTH.SPL       <- t(data.frame(matrix(unlist(strsplit(WHfiles,  split = '_')), ncol = 8100)))
SOW            <- list.files('F:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/CSCRP/RCP8.5', pattern = '.txt', full.names = T);
SOW.DIR       <- list.files('F:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/CSCRP/RCP8.5', pattern = '.txt');
SOW.SPL       <- t(data.frame(matrix(unlist(strsplit(SOW.DIR,  split = '_')), ncol = 4482)))


for (i in 90:nrow(site_inf)){
  #for(i in 1: length(SOW.SPL)){
  
  ID    <- which(WTH.SPL[,1]==as.character(site_inf[i,4]))
  
  WTHroot  <- paste("F:/Researchs/Papers/Wheat climate suatiable/CMIP6/RCP85/",
                    paste(site_inf[i,4], WTH.SPL[ID,2],WTH.SPL[ID,3],WTH.SPL[ID,4], sep='_'),sep='') 
  SOWroot  <- paste("F:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/CSCRP/RCP8.5/",
                    paste(site_inf[i,4], WTH.SPL[ID,2],WTH.SPL[ID,3],'smout.txt', sep='_'),sep='')  
  #for (j in 1){
  for(j in 1:length(WTHroot)) {
    #j=1
    SOWD     <- read.table(SOWroot[j], blank.lines.skip = F,stringsAsFactors = F, sep = ';', header = T)
    
    #MET.DIR <- paste('F:/Researchs/Papers/Wheat climate suatiable/CMIP6/RCP85/',
    #paste(WTH.SPL[which(WTH.SPL[,1]==SOW.SPL[i,1]&WTH.SPL[,2]==SOW.SPL[i,2]&WTH.SPL[,3]==SOW.SPL[i,3]),], collapse = '_'), sep = '')
    
    METO    <- read.table(WTHroot[j],blank.lines.skip = F,skip  = c(32),  stringsAsFactors = F) #!!!!!!!!!!
    
    
    
    OUTDIR           <-  paste('F:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/STICS/RCP8.5/', 
                               paste(site_inf[i,4], WTH.SPL[ID,2][j],WTH.SPL[ID,3][j],
                                     "STICS_OUT.txt", sep='_'), sep = '');
    
    colnames         <- paste('year', 'site','rcp','gcm', 'sowing','flowering','maturity','biomass', 'yield', sep = ';   ');
    
    write(colnames, OUTDIR);
    
    USM              <- readLines('F:/Researchs/JavaSTICS-v85/CHLP/usms.xml');
    
    CULID    <- grep(pattern= 'plante dominance="1"', USM)+1
    INCID    <- grep(pattern= "<finit>mais_", USM)
    SOLID    <- grep(pattern= "<nomsol>sol", USM)
    
    
    if(site_inf$FCA[i]==1){
      USM[CULID] <- '            <fplt>Lingfen_plt.xml</fplt>' #
      substr(USM[INCID],21, 21) <- '1'
      substr(USM[SOLID],20, 20) <- '1'
    }
    if(site_inf$FCA[i]==2){
      USM[CULID] <- '            <fplt>Xiaoyan22_wheat_plt.xml</fplt>' #
      substr(USM[INCID],21, 21) <- '2'
      substr(USM[SOLID],20, 20) <- '2'
    }
    if(site_inf$FCA[i]==3){
      USM[CULID] <- '            <fplt>Changhan_plt.xml</fplt>'
      substr(USM[INCID],21, 21) <- '3'
      substr(USM[SOLID],20, 20) <- '3'
    } 
    
    write(USM, "F:/Researchs/JavaSTICS-v85/CHLP/usms.xml")
    
    
    for(n in 1:nrow(SOWD)){
      #j=1
      
      TECS     <- readLines('F:/Researchs/JavaSTICS-v85/CHLP/CHLP_tec.xml');
      
      sowid    <- grep(pattern=  '<formalisme nom="sowing">', TECS)+1
      
      if(is.na(SOWD$sowing[n])==T){
        substr(TECS[sowid],59,61)  <- '325'
      }
      if(is.na(SOWD$sowing[n])==F){
        substr(TECS[sowid],59,61)  <- substr(SOWD$sowing[n],5,7)
      }
      
      fertilizeID <-grep(pattern=  '<intervention nb_colonnes="2">', TECS)
      
      if(site_inf$FCA[i]==1){
        substr(TECS[fertilizeID+1],51, 53) <- '257'
        substr(TECS[fertilizeID+2],49, 51) <- '150'
      }
      if(site_inf$FCA[i]==2){
        substr(TECS[fertilizeID+1],51, 53) <- '277'
        substr(TECS[fertilizeID+2],49, 51) <- '120'
      }
      if(site_inf$FCA[i]==3){
        substr(TECS[fertilizeID+1],51, 53) <- '257'
        substr(TECS[fertilizeID+2],49, 51) <- '160'
      }
      
      write(TECS, "F:/Researchs/JavaSTICS-v85/CHLP/CHLP_tec.xml")
      TECS     <- readLines('F:/Researchs/JavaSTICS-v85/CHLP/CHLP_tec.xml');
      TOMET1 <- read.table('F:/Researchs/JavaSTICS-v85/CHLP/CLIMAISJ.1996');
      TOMET2 <- read.table('F:/Researchs/JavaSTICS-v85/CHLP/CLIMAISJ.1997');
      y1  <- as.numeric(substr(SOWD[n,1],1,4))
      y2  <- as.numeric(substr(SOWD[n,1],6,9))
      
      MET.YEAR1  <- METO[METO[,1]==substr(SOWD[n,1],1,4),] 
      TOMET1[,6] <- MET.YEAR1[1:365,5];
      TOMET1[,7] <- MET.YEAR1[1:365,4];
      TOMET1[,8] <- MET.YEAR1[1:365,3];
      TOMET1[,10] <- MET.YEAR1[1:365,6];
      TOMET1[,11]    <- -999.9
      #TOMET1[,13]    <- 350
      
      
      CO2_RCP8.5   <- 757.44+(84.938-1.537*y1)/(2.2011-3.8289*y1^-0.45242)+2.4712*10^-4*(y1+15)^2+1.9299*10^-5*(y1-1937)^3+5.1137*10^-7*(y1-1910)^4
      
      TOMET1[,13] <- rep(round(CO2_RCP8.5,1), 365)
      
      
      MET.YEAR2  <- METO[METO[,1]==substr(SOWD[n,1],6,9),] 
      TOMET2[,6] <- MET.YEAR2[1:365,5];
      TOMET2[,7] <- MET.YEAR2[1:365,4];
      TOMET2[,8] <- MET.YEAR2[1:365,3];
      TOMET2[,10] <- MET.YEAR2[1:365,6];
      TOMET2[,11]    <- -999.9
      #TOMET1[,13]    <- 350
      
      CO2_RCP8.5  <- 757.44+(84.938-1.537*y1)/(2.2011-3.8289*y1^-0.45242)+2.4712*10^-4*(y1+15)^2+1.9299*10^-5*(y1-1937)^3+5.1137*10^-7*(y1-1910)^4
      
      TOMET2[,13] <- rep(round(CO2_RCP8.5,1), 365)
      
      write.table(TOMET1, 'F:/Researchs/JavaSTICS-v85/CHLP/CLIMAISJ.1996',
                  row.names = F, col.names = F, sep = ' ', quote = FALSE);
      
      write.table(TOMET2, 'F:/Researchs/JavaSTICS-v85/CHLP/CLIMAISJ.1997',
                  row.names = F, col.names = F, sep = ' ', quote = FALSE);
      
      
      system('CHLP.bat')
      
      SMOUTS <- read.table('F:/Researchs/JavaSTICS-v85/CHLP/mod_sCHLP.sti',
                           blank.lines.skip = F, stringsAsFactors = F, sep = ';')
      colnames(SMOUTS) <- SMOUTS[1,]
      SMOUTS           <- matrix(as.numeric(unlist(SMOUTS[-1,])),ncol = 8)
      
      outs             <- apply(SMOUTS[,5:8], 2, max)
      
      outs[1]          <- outs[1]- as.numeric(substr(TECS[sowid],59,61))
      outs[2]          <- outs[2]- as.numeric(substr(TECS[sowid],59,61))
      
      outsPUT          <- paste(paste(SOWD[n,1:4],collapse = '; '), 
                                as.numeric(substr(TECS[sowid],59,61)),
                                paste(outs, collapse = '; '), sep = ' ');
      
      
      write(outsPUT,  OUTDIR, append = T)  
    }
  }
}
###################################################################################################
#################################!!!!!!!!!!!!!!!!!!! Bal_wheat RCP8.5 !!!!!!!!!!!!#################
###################################################################################################

setwd('F:/Work Space/JavaSTICS-v85/')

MET.DIR <- list.files('F:/Work Space/APSIM results/49002_Balranald/RCP85', pattern = '.MET', full.names = T);
SOW.DIR <- list.files('F:/Work Space/APSIM results/49002_Balranald/RCP85', pattern = '.out', full.names = T);

for(i in 1: length(MET.DIR)){
  #for(i in 1){
  METO  <- read.table(MET.DIR[i],blank.lines.skip = F,skip  = c(28),  stringsAsFactors = F)[-2,];
  SOWD  <- read.table(SOW.DIR[i],blank.lines.skip = F,skip  = c(6),  stringsAsFactors = F, sep = ',');
  colnames(METO) <- METO[1,];
  METO           <- METO[-1,]; 
  
  OUTDIR           <-  paste('C:/Users/JTC/Desktop/Bal_Wheat/Bal_wheat RCP8.5/BAL15_', 
                             substr(MET.DIR[i], 51, 70),".txt", sep = '');
  
  colnames         <- paste('Year', 'sowing_date(doy)','emergence_date(das)',
                            'end_of_juvenile_date(das)','flowering_date(das)',
                            'start_grain_fill_date(das)',  'maturity_date(das)', 
                            'harvesting_day(das)','biomass(kg/ha)','yield(kg/ha)',
                            sep = ';   ');
  
  write(colnames, OUTDIR);
  
  for(j in 1961:2100){
    
    SOW     <- readLines('F:/Work Space/JavaSTICS-v85/Bal_wheat/15balyear_tec.xml');
    TOMET <- read.table('F:/Work Space/JavaSTICS-v85/Bal_wheat/AUSbal-climate.2015');
    MET.YEAR  <- METO[METO$year==j,] 
    TOMET[,6] <- MET.YEAR$mint[1:365];
    TOMET[,7] <- MET.YEAR$maxt[1:365];
    TOMET[,8] <- MET.YEAR$radn[1:365];
    TOMET[,10] <- MET.YEAR$rain[1:365];
    
    
    CO2_RCP8.5   <- 1034.3+(267.78-(1.6188*j))/(4.0143+53.342/(j^5.2822))+
      21.746*((j-2010)/100)^3+100.65*((j-1911)/100)^3 
    TOMET[,13] <- rep(round(CO2_RCP8.5,1), 365)
    
    write.table(TOMET, 'F:/Work Space/JavaSTICS-v85/Bal_wheat/AUSbal-climate.2015',
                row.names = F, col.names = F, sep = ' ', quote = FALSE);
    
    SOWRD      <- grep(pattern = '<formalisme nom="sowing">', SOW)
    substr(SOW[SOWRD +1], 59, 61) <-  sprintf('%2.0f', SOWD[j-1960,2])
    
    NFID       <- grep(pattern = '<colonne nom="absolute_value/%">60</colonne>', SOW)
    substr(SOW[NFID -1], 51, 53) <-  sprintf('%2.0f', SOWD[j-1960,2])
    write.table(SOW,'F:/Work Space/JavaSTICS-v85/Bal_wheat/15balyear_tec.xml',
                row.names = F, col.names = F, sep = ' ', quote = FALSE)
    
    system('Bal_wheat')
    
    SMOUTS <- read.table('F:/Work Space/JavaSTICS-v85/Bal_wheat/mod_sbal15.sti',
                         blank.lines.skip = F, stringsAsFactors = F, sep = ';')
    colnames(SMOUTS) <- SMOUTS[1,]
    SMOUTS           <- matrix(as.numeric(unlist(SMOUTS[-1,])),ncol = 14)
    
    outs             <- apply(SMOUTS[,5:13], 2, max)
    outsPUT          <- paste(j, SOWD[j-1960,2], abs(outs[1]-SOWD[j-1960,2]),
                              outs[2]-SOWD[j-1960,2], outs[3]-SOWD[j-1960,2],
                              outs[4]-SOWD[j-1960,2], outs[6]-SOWD[j-1960,2],
                              outs[7]-SOWD[j-1960,2], outs[8],outs[9],sep = ';   ')
    
    write(outsPUT,  OUTDIR, append = T)
  }
}

###################################################################################################
#################################!!!!!!!!!!!!!!!!!!! Bal_wheat Basline !!!!!!!!!!!!#################
###################################################################################################



setwd('F:/Work Space/JavaSTICS-v85/')

METO  <- read.table('F:/Work Space/APSIM results/49002_Balranald/baseline/49002.MET',
                    blank.lines.skip = F,skip  = c(20),  stringsAsFactors = F)[-2,];

SOWD  <- read.table('F:/Work Space/APSIM results/49002_Balranald/baseline/49002_baseline.out',
                    blank.lines.skip = F,skip  = c(6),  stringsAsFactors = F, sep = ',');
colnames(METO) <- METO[1,];
METO           <- METO[-1,]; 

OUTDIR           <-  paste('C:/Users/JTC/Desktop/Bal_Wheat/Bal_Wheat Basline/BAL_', 
                           'Basline',".txt", sep = '')

colnames         <- paste('Year', 'sowing_date(doy)','emergence_date(das)',
                          'end_of_juvenile_date(das)','flowering_date(das)',
                          'start_grain_fill_date(das)',  'maturity_date(das)', 
                          'harvesting_day(das)','biomass(kg/ha)','yield(kg/ha)',
                          sep = ';   ');

write(colnames, OUTDIR);

for(j in 1961:2000){
  SOW     <- readLines('F:/Work Space/JavaSTICS-v85/Bal_wheat/15balyear_tec.xml');
  TOMET   <- read.table('F:/Work Space/JavaSTICS-v85/Bal_wheat/AUSbal-climate.2015');
  
  MET.YEAR  <- METO[METO$year==j,] 
  TOMET[,6] <- MET.YEAR$mint[1:365];
  TOMET[,7] <- MET.YEAR$maxt[1:365];
  TOMET[,8] <- MET.YEAR$radn[1:365];
  TOMET[,10] <- MET.YEAR$rain[1:365];
  
  
  
  TOMET[,13] <- rep(350, 365)
  
  write.table(TOMET, 'F:/Work Space/JavaSTICS-v85/Bal_wheat/AUSbal-climate.2015',
              row.names = F, col.names = F, sep = ' ', quote = FALSE);
  
  SOWRD      <- grep(pattern = '<formalisme nom="sowing">', SOW)
  substr(SOW[SOWRD +1], 59, 61) <-  sprintf('%2.0f', SOWD[j-1960,2])
  
  NFID       <- grep(pattern = '<colonne nom="absolute_value/%">60</colonne>', SOW)
  substr(SOW[NFID -1], 51, 53) <-  sprintf('%2.0f', SOWD[j-1960,2])
  write.table(SOW,'F:/Work Space/JavaSTICS-v85/Bal_wheat/15balyear_tec.xml',
              row.names = F, col.names = F, sep = ' ', quote = FALSE)
  
  system('Bal_wheat')
  
  SMOUTS <- read.table('F:/Work Space/JavaSTICS-v85/Bal_wheat/mod_sbal15.sti',
                       blank.lines.skip = F, stringsAsFactors = F, sep = ';')
  colnames(SMOUTS) <- SMOUTS[1,]
  SMOUTS           <- matrix(as.numeric(unlist(SMOUTS[-1,])),ncol = 14)
  
  outs             <- apply(SMOUTS[,5:13], 2, max)
  outsPUT          <- paste(j, SOWD[j-1960,2], abs(outs[1]-SOWD[j-1960,2]),
                            outs[2]-SOWD[j-1960,2], outs[3]-SOWD[j-1960,2],
                            outs[4]-SOWD[j-1960,2], outs[6]-SOWD[j-1960,2],
                            outs[7]-SOWD[j-1960,2], outs[8],outs[9],sep = ';   ')
  
  write(outsPUT,  OUTDIR, append = T)
}























