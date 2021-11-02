rm(list=ls());
library(apsimx)
# memory.limit(102400)
site_inf      <- readxl::read_xls('E:/Researchs/Papers/Wheat climate suatiable/LPsoil.xls')
site_inf      <- site_inf[site_inf$FCA==1,]

WHfiles       <- list.files("E:/Researchs/Papers/Wheat climate suatiable/CMIP6/RCP45") 

WHfiles       <- t(data.frame(matrix(unlist(strsplit(WHfiles,  split = '_')), ncol = 8100)))

#3667-3672~136
#37:nrow(site_inf)
for (i in 1: nrow(site_inf)){
  
  # i=1
  
  ID    <- which(WHfiles[,1]==as.character(site_inf[i,4]))
  
  root  <- paste("E:/Researchs/Papers/Wheat climate suatiable/CMIP6/RCP45/",
                 paste(site_inf[i,4], WHfiles[ID,2],WHfiles[ID,3],WHfiles[ID,4], sep='_'),sep='')
  
  Taroot <- paste("C:/APSIM2021/Examples/WeatherFiles/", paste(site_inf[i,4],
                                                               WHfiles[ID,2],WHfiles[ID,3],WHfiles[ID,4], sep='_'),sep='')
  
  met.name   <- paste(site_inf[i,4], WHfiles[ID,2],WHfiles[ID,3],WHfiles[ID,4], sep='_')
  
  for (j in 1)
  
  #for (j in 1:length(root))
  {
    
    # j=1
    
    # SMout      <- paste('year', 'site','rcp','gcm', 'sowing',
    #                     'flowering','maturity', 'yield', sep = ';')
    # 
    # write(SMout, file = paste('E:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/Apsim/RCP45/', 
    #                           site_inf[i,4],'_',WHfiles[ID,2][j],'_',WHfiles[ID,3][j],
    #                           '_smout.txt', sep = ''))
    
    file.copy(from = root[j], to = Taroot[j])
    
    TempMet <- readLines(Taroot[j])
    
    substr(TempMet[25],1,1) <- ' '
    
    writeLines(TempMet, Taroot[j])
    
    if(site_inf$FCA[i]==1){
      
      APSIM_TEMP <- readLines('C:\\APSIM2021\\CHLP1.apsimx')
      
      MetID      <- grep(pattern= '"Models.Climate.Weather', APSIM_TEMP)
      
      substr(APSIM_TEMP[MetID+2],48,77) <- met.name[j]
      
      write(APSIM_TEMP,'C:\\APSIM2021\\CHLP1.apsimx')
      
      # for (n in c(c(1971:2011),c(2040:2061), c(2080:2100))) {
      #   
      #   #n=1971
      #   
      #   APSIM_TEMP <- readLines('C:\\APSIM2021\\CHLP1.apsimx')
      #   
      #   MetID      <- grep(pattern= 'Models.Clock', APSIM_TEMP)
      #   
      #   substr(APSIM_TEMP[MetID+1], 21, 24)  <-  as.character(n-1) 
      #   
      #   substr(APSIM_TEMP[MetID+2], 19, 22)  <-  as.character(n) 
      #   
      #   CO2ID    <- grep(pattern= 'ReferenceCO2', APSIM_TEMP)
      #   
      #   y1 <- n-1
      #   # ######## RCP4.5 Caculations
      #   if (n<2040) {
      #     CO2_RCP4.5 <- 350
      #   }else {
      #     CO2_RCP4.5  <- 650.18+((0.000075326*y1-0.16276)/(0.00022299-727.97/(y1^2)))-0.00018747*(y1-2045)^3
      #     
      #   }
      #    
      #   APSIM_TEMP[CO2ID[1]+4] <-  paste('                  "Value": "',round(CO2_RCP4.5,0) ,'"',sep = '')
      #   
      #   write(APSIM_TEMP,'C:\\APSIM2021\\CHLP1.apsimx')
      
      extd.dir  <- 'C:\\APSIM2021'
      
      SIM       <- apsimx('CHLP1.apsimx', src.dir= extd.dir, value = "report",
                          cleanup = F)
      
      # file.remove('C:/APSIM2021/CHLP1.db')
      
      out   <- SIM[,-c(1:3,11)]#read.table('C:\\APSIM710\\Examples\\CHLP.out',skip  = 4)[,3:7]
      
      colnames(out) <- c('year','sow', 'flowering', 'maturity', 'niomass', 'yield', 'CO2')
      # out   <- apply(out, 2, max)
      # 
      # out   <- out[c(5,3,4,2)]
      # 
      # year <- paste((n-1), n, sep = '-')    
      
      out$site <- as.character(site_inf[i,4])
      
      out$rcp <- WHfiles[ID,2][j]
      
      out$gcm <- WHfiles[ID,3][j]
      
      SMDATA    <- out#paste(year, site, rcp, gcm, paste(out, collapse = ';'), sep = ';')
      
      write.table(out, file = paste('E:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/Apsim/RCP45/', 
                                    site_inf[i,4],'_',WHfiles[ID,2][j],'_',WHfiles[ID,3][j],
                                    '_smout.txt', sep = ''), row.names = F)
      
    }
    
    
    
    if(site_inf$FCA[i]==2){
      
      APSIM_TEMP <- readLines('C:\\APSIM2021\\CHLP2.apsimx')
      
      MetID      <- grep(pattern= '"Models.Climate.Weather', APSIM_TEMP)
      
      substr(APSIM_TEMP[MetID+2],48,77) <- met.name[j]
      
      write(APSIM_TEMP,'C:\\APSIM2021\\CHLP2.apsimx')
      
      extd.dir  <- 'C:\\APSIM2021'
      
      SIM       <- apsimx('CHLP2.apsimx', src.dir= extd.dir, value = "report",
                          cleanup = F)
      
      out   <- SIM[,-c(1:3,11)]#read.table('C:\\APSIM710\\Examples\\CHLP.out',skip  = 4)[,3:7]
      
      colnames(out) <- c('year','sow', 'flowering', 'maturity', 'niomass', 'yield', 'CO2')
      
      out$site <- as.character(site_inf[i,4])
      
      out$rcp <- WHfiles[ID,2][j]
      
      out$gcm <- WHfiles[ID,3][j]
      
      SMDATA    <- out
      
      write.table(out, file = paste('E:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/Apsim/RCP45/', 
                                    site_inf[i,4],'_',WHfiles[ID,2][j],'_',WHfiles[ID,3][j],
                                    '_smout.txt', sep = ''), row.names = F)
      
    }
    
    
    if(site_inf$FCA[i]==3){
      
      APSIM_TEMP <- readLines('C:\\APSIM2021\\CHLP3.apsimx')
      
      MetID      <- grep(pattern= '"Models.Climate.Weather', APSIM_TEMP)
      
      substr(APSIM_TEMP[MetID+2],48,77) <- met.name[j]
      
      write(APSIM_TEMP,'C:\\APSIM2021\\CHLP3.apsimx')
      
      extd.dir  <- 'C:\\APSIM2021'
      
      SIM       <- apsimx('CHLP3.apsimx', src.dir= extd.dir, value = "report",
                          cleanup = F)
      
      out   <- SIM[,-c(1:3,11)]#read.table('C:\\APSIM710\\Examples\\CHLP.out',skip  = 4)[,3:7]
      
      colnames(out) <- c('year','sow', 'flowering', 'maturity', 'niomass', 'yield', 'CO2')
      
      out$site <- as.character(site_inf[i,4])
      
      out$rcp <- WHfiles[ID,2][j]
      
      out$gcm <- WHfiles[ID,3][j]
      
      SMDATA    <- out
      
      write.table(out, file = paste('E:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/Apsim/RCP45/', 
                                    site_inf[i,4],'_',WHfiles[ID,2][j],'_',WHfiles[ID,3][j],
                                    '_smout.txt', sep = ''), row.names = F)
      
    }
    
  } 
  
  file.remove(Taroot[j])
  
}   

# y1 <- c(2041:2100)
# 
# CO2_RCP4.5  <- 650.18+((0.000075326*y1-0.16276)/(0.00022299-727.97/(y1^2)))-0.00018747*(y1-2045)^3


