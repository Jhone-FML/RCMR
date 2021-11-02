library(Evapotranspiration)

data("constants")

ET.PenmanMonteith  <- function (data, constants, ts = "daily", solar = "data", 
                                wind = "no", crop = "short", message = "yes", 
                                AdditionalStats = "yes", save.csv = "no", ...)
{
  if (is.null(data$Tmax) | is.null(data$Tmin)) {
    stop("Required data missing for 'Tmax' and 'Tmin', or 'Temp'")
  }
  #if (is.null(data$va) | is.null(data$vs)) {
  if (is.null(data$RH)) {
    stop("Required data missing: need either 'va' and 'vs', or 'RHmax' and 'RHmin' (or 'RH')")
  }
  #}
  if (wind == "yes") {
    if (is.null(data$u2) & is.null(data$uz)) {
      stop("Required data missing for 'uz' or 'u2'")
    }
  }
  if (solar == "data" & is.null(data$Rs)) {
    stop("Required data missing for 'Rs'")
  } else if (solar == "sunshine hours" & is.null(data$n)) {
    stop("Required data missing for 'n'")
  }else if (solar == "cloud" & is.null(data$Cd)) {
    stop("Required data missing for 'Cd'")
  }else if (solar == "monthly precipitation" & is.null(data$Precip)) {
    stop("Required data missing for 'Precip'")
  }
  if (wind != "yes" & wind != "no") {
    stop("Please choose if actual data will be used for wind speed from wind = 'yes' and wind = 'no'")
  }
  if (wind == "yes") {
    if (crop != "short" & crop != "tall") {
      stop("Please enter 'short' or 'tall' for the desired reference crop type")
    }
    else {
      alpha <- 0.23
      if (crop == "short") {
        z0 <- 0.02
      }
      else {
        z0 <- 0.1
      }
    }
  }else {
    z0 <- 0.02
    alpha <- 0.25
  }
  Ta <- (data$Tmax + data$Tmin)/2
  if (!is.null(data$va) & !is.null(data$vs)) {
    vabar <- data$va
    vas <- data$vs
  }else {
    vs_Tmax <- 0.6108 * exp(17.27 * data$Tmax/(data$Tmax + 
                                                 237.3))
    vs_Tmin <- 0.6108 * exp(17.27 * data$Tmin/(data$Tmin + 
                                                 237.3))
    vas <- (vs_Tmax + vs_Tmin)/2
    vabar <- data$RH*vas/100#(vs_Tmin * data$RHmax/100 + vs_Tmax * data$RHmin/100)/2
  }
  P <- 101.3 * ((293 - 0.0065 * constants$Elev)/293)^5.26
  delta <- 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/((Ta + 
                                                                237.3)^2)
  gamma <- 0.00163 * P/constants$lambda
  d_r2 <- 1 + 0.033 * cos(2 * pi/365 * data$J)
  delta2 <- 0.409 * sin(2 * pi/365 * data$J - 1.39)
  w_s <- acos(-tan(constants$lat_rad) * tan(delta2))
  N <- 24/pi * w_s
  R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(constants$lat_rad) * 
                                               sin(delta2) + cos(constants$lat_rad) * cos(delta2) * 
                                               sin(w_s))
  R_so <- (0.75 + (2 * 10^-5) * constants$Elev) * R_a
  if (solar == "data") {
    R_s <- data$Rs
  }else if (solar != "monthly precipitation" & solar != 
            "cloud") {
    R_s <- (constants$as + constants$bs * (data$n/N)) * R_a
  }else {
    R_s <- (0.85 - 0.047 * data$Cd) * R_a
  }
  R_nl <- constants$sigma * (0.34 - 0.14 * sqrt(vabar)) * ((data$Tmax + 
                                                              273.2)^4 + (data$Tmin + 273.2)^4)/2 * (1.35 * R_s/R_so - 
                                                                                                       0.35)
  R_nsg <- (1 - alpha) * R_s
  R_ng <- R_nsg - R_nl
  if (wind == "yes") {
    if (is.null(data$u2)) {
      u2 <- data$uz * 4.87/log(67.8 * constants$z - 5.42)
    }
    else {
      u2 <- data$u2
    }
    if (crop == "short") {
      r_s <- 70
      CH <- 0.12
      ET_RC.Daily <- (0.408 * delta * (R_ng - constants$G) + 
                        gamma * 900 * u2 * (vas - vabar)/(Ta + 273))/(delta + 
                                                                        gamma * (1 + 0.34 * u2))
    }
    else {
      r_s <- 45
      CH <- 0.5
      ET_RC.Daily <- (0.408 * delta * (R_ng - constants$G) + 
                        gamma * 1600 * u2 * (vas - vabar)/(Ta + 273))/(delta + 
                                                                         gamma * (1 + 0.38 * u2))
    }
    ET.Daily <- ET_RC.Daily
    ET.Monthly <- aggregate(ET.Daily, as.yearmon(data$Date.daily, 
                                                 "%m/%y"), FUN = sum)
    ET.Annual <- aggregate(ET.Daily, floor(as.numeric(as.yearmon(data$Date.daily))), 
                           FUN = sum)
  }else {
    RHmean    <- zoo(data$RH, as.Date(substr(data$Date.daily, 1, 10)))
    R_s.Monthly <- aggregate(R_s, as.yearmon(data$Date.daily, 
                                             "%m/%y"), mean)
    R_a.Monthly <- aggregate(R_a, as.yearmon(data$Date.daily, 
                                             "%m/%y"), mean)
    Ta.Monthly <- aggregate(Ta, as.yearmon(data$Date.daily, 
                                           "%m/%y"), mean)
    RHmean.Monthly <- aggregate(RHmean, as.yearmon(data$Date.daily, 
                                                   "%m/%y"), mean)
    ET_RC.Monthly <- 0.038 * R_s.Monthly * sqrt(Ta.Monthly + abs(min(Ta.Monthly))) - 2.4 * (R_s.Monthly/R_a.Monthly)^2 + 0.075 * (Ta.Monthly + 20) * (1 - RHmean.Monthly/100)
    ET_RC.Daily <- data$Tmax
    for (cont in 1:length(data$i)) {
      ET_RC.Daily[(((as.numeric(as.yearmon(time(ET_RC.Daily)))) - 
                      floor(as.numeric(as.yearmon(time(ET_RC.Daily))))) * 
                     12 + 1) == data$i[cont]] <- ET_RC.Monthly[cont]
    }
    ET.Daily <- ET_RC.Daily
    ET.Monthly <- aggregate(ET.Daily, as.yearmon(data$Date.daily, 
                                                 "%m/%y"), FUN = sum)
    ET.Annual <- aggregate(ET.Daily, floor(as.numeric(as.yearmon(data$Date.daily))), 
                           FUN = sum)
  }
  ET.MonthlyAve <- ET.AnnualAve <- NULL
  if (AdditionalStats == "yes") {
    for (mon in min(as.POSIXlt(data$Date.daily)$mon):max(as.POSIXlt(data$Date.daily)$mon)) {
      i = mon - min(as.POSIXlt(data$Date.daily)$mon) + 
        1
      ET.MonthlyAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$mon == 
                                          mon])
    }
    for (year in min(as.POSIXlt(data$Date.daily)$year):max(as.POSIXlt(data$Date.daily)$year)) {
      i = year - min(as.POSIXlt(data$Date.daily)$year) + 
        1
      ET.AnnualAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$year == 
                                         year])
    }
  }
  if (wind == "no") {
    ET_formulation <- "Penman-Monteith (without wind data)"
    ET_type <- "Reference Crop ET"
    Surface <- paste("short grass, albedo =", alpha, 
                     "; roughness height =", z0, "m")
  }else {
    if (crop == "short") {
      ET_formulation <- "Penman-Monteith FAO56"
      ET_type <- "Reference Crop ET"
      Surface <- paste("FAO-56 hypothetical short grass, albedo =", 
                       alpha, "; surface resistance =", r_s, "sm^-1; crop height =", 
                       CH, " m; roughness height =", z0, "m")
    }else {
      ET_formulation <- "Penman-Monteith ASCE-EWRI Standardised"
      ET_type <- "Reference Crop ET"
      Surface <- paste("ASCE-EWRI hypothetical tall grass, albedo =", 
                       alpha, "; surface resistance =", r_s, "sm^-1; crop height =", 
                       CH, " m; roughness height =", z0, "m")
    }
  }
  if (solar == "data") {
    message1 <- "Solar radiation data have been used directly for calculating evapotranspiration"
  }else if (solar == "sunshine hours") {
    message1 <- "Sunshine hour data have been used for calculating incoming solar radiation"
  }else if (solar == "cloud") {
    message1 <- "Cloudiness data have been used for calculating sunshine hour and thus incoming solar radiation"
  }else {
    message1 <- "Monthly precipitation data have been used for calculating incoming solar radiation"
  }
  if (wind == "yes") {
    message2 <- "Wind data have been used for calculating the reference crop evapotranspiration"
  } else {
    message2 <- "Alternative calculation for reference crop evapotranspiration without wind data have been performed"
  }
  results <- list(ET.Daily = ET.Daily, ET.Monthly = ET.Monthly, 
                  ET.Annual = ET.Annual, ET.MonthlyAve = ET.MonthlyAve, 
                  ET.AnnualAve = ET.AnnualAve, ET_formulation = ET_formulation, 
                  ET_type = ET_type, message1 = message1, message2 = message2)
  if (ts == "daily") {
    res_ts <- ET.Daily
  } else if (ts == "monthly") {
    res_ts <- ET.Monthly
  }else if (ts == "annual") {
    res_ts <- ET.Annual
  }
  if (message == "yes") {
    message(ET_formulation, " ", ET_type)
    message("Evaporative surface: ", Surface)
    message(message1)
    message(message2)
    message("Timestep: ", ts)
    message("Units: mm")
    message("Time duration: ", time(res_ts[1]), " to ", 
            time(res_ts[length(res_ts)]))
    if (NA %in% res_ts) {
      message(length(res_ts), " ET estimates obtained; ", 
              length(which(is.na(res_ts))), " NA output entries due to missing data")
      message("Basic stats (NA excluded)")
      message("Mean: ", round(mean(res_ts, na.rm = T), 
                              digits = 2))
      message("Max: ", round(max(res_ts, na.rm = T), 
                             digits = 2))
      message("Min: ", round(min(res_ts, na.rm = T), 
                             digits = 2))
    }else {
      message(length(res_ts), " ET estimates obtained")
      message("Basic stats")
      message("Mean: ", round(mean(res_ts), digits = 2))
      message("Max: ", round(max(res_ts), digits = 2))
      message("Min: ", round(min(res_ts), digits = 2))
    }
  }
  if (save.csv == "yes") {
    for (i in 1:length(results)) {
      namer <- names(results[i])
      write.table(as.character(namer), file = "ET_PenmanMonteith.csv", 
                  dec = ".", quote = FALSE, col.names = FALSE, 
                  row.names = F, append = TRUE, sep = ",")
      write.table(data.frame(get(namer, results)), file = "ET_PenmanMonteith.csv", 
                  col.names = F, append = T, sep = ",")
    }
    invisible(results)
  } else {
    return(results)
  }
}


site_inf      <- readxl::read_xls('E:/Researchs/Papers/Wheat climate suatiable/LPsoil.xls')

site_inf      <- site_inf[site_inf$FCA==3,]

WHfiles       <- list.files("E:/Researchs/Papers/Wheat climate suatiable/CMIP6/RCP85") 

WHfiles       <- t(data.frame(matrix(unlist(strsplit(WHfiles,  split = '_')), ncol = 8100)))

Codays        <- read.table("C:/Users/JATCN/Desktop/Juliandays.txt", header = TRUE,
                            blank.lines.skip = F,skip  = c(6),  stringsAsFactors = F) #

Codays        <- Codays[-1,-1]

colnames(Codays) <- c('January', 'February','March','April','May','June','July',
                      'August','September', 'October','November', 'December')  

for (i in 1:nrow(site_inf)){
  
  # i=1
  
  ID    <- which(WHfiles[,1]==as.character(site_inf[i,4]))
  
  root  <- paste("E:/Researchs/Papers/Wheat climate suatiable/CMIP6/RCP85/",
                 paste(site_inf[i,4], WHfiles[ID,2],WHfiles[ID,3],WHfiles[ID,4], sep='_'),sep='')   
  
  sowpath  <- paste("E:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/Nwheat/RCP8.5/",
                    paste(site_inf[i,4], WHfiles[ID,2],WHfiles[ID,3],'smout.txt', sep='_'),sep='')   
  
  
  for (j in 1)
  #for (j in 2:length(root))
  {
    
    # j=1
    sow           <- read.table(sowpath[j],  sep = ';', header = T)
    
    weatherfiles  <- read.table(root[j], blank.lines.skip = F,skip  = c(32),  stringsAsFactors = F) #!!!!!!!!!!
    
    colnas        <- c('Year', 'Julday', 'Rs', 'Tmax', 'Tmin', 'Precip', 'RH', 'vp', 'code')
    
    weatherdata            <-  data.frame(matrix(as.numeric(unlist(weatherfiles[-c(1),])), ncol = 9))
    
    colnames(weatherdata)  <-  colnas
    
    weather                <-  weatherdata[weatherdata$Year %in% c(c(1970:2011),c(2040:2061), c(2080:2100)), ]
    
    TIM_TMAX <- function(x){
      #x <- weather 
      for(m in 1:length(x$Tmax)){
        if(x$Tmin[m]==x$Tmax[m]){
          x$Tmin[m] <- x$Tmin[m]-0.1
        }
        else{
          x$Tmin[m] <- x$Tmin[m] 
        }
      }
      return(x)
    }
    
    weather  <- TIM_TMAX(weather)
    
    Date                   <- seq.Date(as.Date("1970/1/1"), as.Date("2100/12/31"), by = "day")
    
    yyyymmdd               <- data.frame(t(matrix(as.numeric(unlist(strsplit(as.character(Date),split = '-'))), nrow  = 3))) 
    
    colnames(yyyymmdd)     <- c('Year', 'Month', 'Day')
    
    weather$Month          <- yyyymmdd$Month[yyyymmdd$Year%in% c(c(1970:2011),c(2040:2061), c(2080:2100))]  
    
    weather$Day            <- yyyymmdd$Day[yyyymmdd$Year%in% c(c(1970:2011),c(2040:2061), c(2080:2100))]  
    
    constants$lat          <- site_inf$lat[i]
    
    constants$Elev         <- as.numeric(site_inf[i,10])
    
    weather$u2             <- NA
    
    weather$Temp           <- (weather$Tmax+weather$Tmin)/2
    
    eoT <-   0.611*exp(17.27*weather$Temp/(weather$Temp+237.3)) 
    
    ea  <-  weather$RH*eoT/100 
    
    weather$Tdew              <- (116.91+237.3*log10(ea))/(16.78-log10(ea))
    
    CHLP             <- weather[c('Year', 'Month', 'Day','Temp', "Tdew",'Tmax', 'Tmin', "Rs", 'u2', 'RH','Precip')]
    
    SMout      <- paste('year', 'site','rcp','gcm', 'sowing', 'flowering',
                        'maturity', 'biomass','yield',  sep = ';')
    
    write(SMout, file = paste('E:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/Auqacrop/RCP8.5/', 
                              site_inf[i,4],'_',WHfiles[ID,2][j],'_',WHfiles[ID,3][j],
                              '_smout.txt', sep = ''))
    
    
    for (n in c(c(1971:2011),c(2041:2061), c(2081:2100))) {
      # n=1971
      
      CHLPLT           <- CHLP[CHLP$Year %in% c((n-1),n),] 
      
      CHLPETo          <- ReadInputs(varnames = c('Temp', "Tdew",'Tmax', 'Tmin', "Rs"),
                                     CHLPLT, 
                                     constants, 
                                     stopmissing=c(10,10,3),
                                     timestep = "daily",
                                     interp_missing_days = FALSE, 
                                     interp_missing_entries = FALSE, 
                                     interp_abnormal = FALSE, 
                                     missing_method = NULL, 
                                     abnormal_method = NULL)
      CHLPETo$RH  <- CHLPLT$RH
      # Call ET.PenmanMonteith under the generic function ET
      
      #data <- LFETo
      
      
      results <- ET.PenmanMonteith(CHLPETo, constants, ts="daily", solar="data",
                                   wind="no", crop = "short", message="yes",
                                   AdditionalStats="yes", save.csv="no")
      
      CHLPLT$ETo <- results$ET.Daily
      
      #============================================================> ETo --------------------------------------
      #                                       Generate weather data
      #============================================================> ETo --------------------------------------
      Auqacrop_ETo  <- NULL 
      
      Auqacrop_ETo[1] <- 'China Loess Plateau - daily data: 1 January 2001 - 31 December 2002'
      Auqacrop_ETo[2] <- '     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)'
      Auqacrop_ETo[3] <- '     1  : First day of record (1, 11 or 21 for 10-day or 1 for months)'
      Auqacrop_ETo[4] <- '     1  : First month of record'
      Auqacrop_ETo[5] <- '  2001  : First year of record (1901 if not linked to a specific year)'
      Auqacrop_ETo[6] <- ''
      Auqacrop_ETo[7] <- '  Average ETo (mm/day)'
      Auqacrop_ETo[8] <- '======================='
      Auqacrop_ETo[9:(length(round(CHLPLT$ETo,1))+8)] <- sprintf('%2.1f',round(CHLPLT$ETo,1))
      
      write(Auqacrop_ETo, 'C:/AQUACROP/DATA/CHLP.ETo')
      
      #============================================================> PLU --------------------------------------
      Auqacrop_PLU  <- NULL 
      
      Auqacrop_PLU[1] <- 'China Loess Plateau - daily data: 1 January 2001 - 31 December 2002'
      Auqacrop_PLU[2] <- '     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)'
      Auqacrop_PLU[3] <- '     1  : First day of record (1, 11 or 21 for 10-day or 1 for months)'
      Auqacrop_PLU[4] <- '     1  : First month of record'
      Auqacrop_PLU[5] <- '  2001  : First year of record (1901 if not linked to a specific year)'
      Auqacrop_PLU[6] <- ''
      Auqacrop_PLU[7] <- '  Total Rain (mm)'
      Auqacrop_PLU[8] <- '======================='
      Auqacrop_PLU[9:(length(round(CHLPLT$Precip,2))+8)] <-  sprintf('%2.1f',round(CHLPLT$Precip,1))
      
      write(Auqacrop_PLU, 'C:/AQUACROP/DATA/CHLP.PLU')
      
      #============================================================> TMP --------------------------------------
      
      Auqacrop_TMP  <- NULL 
      
      Auqacrop_TMP[1] <- 'China Loess Plateau - daily data: 1 January 2001 - 31 December 2002'
      Auqacrop_TMP[2] <- '     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)'
      Auqacrop_TMP[3] <- '     1  : First day of record (1, 11 or 21 for 10-day or 1 for months)'
      Auqacrop_TMP[4] <- '     1  : First month of record'
      Auqacrop_TMP[5] <- '  2001  : First year of record (1901 if not linked to a specific year)'
      Auqacrop_TMP[6] <- ''
      Auqacrop_TMP[7] <- '  Tmin (C)   TMax (C)'
      Auqacrop_TMP[8] <- '======================='
      Auqacrop_TMP[9:(length(round(CHLPLT$Precip,1))+8)] <- paste(sprintf('%2.1f',round(CHLPLT$Tmin,1)),
                                                                  sprintf('%2.1f',round(CHLPLT$Tmax,1)))
      
      write(Auqacrop_TMP, 'C:/AQUACROP/DATA/CHLP.TMP')
      
      PRO            <-  readLines('C:/ACsaV60/LIST/CHLP.PRO')
      
      if(site_inf$FCA[i]==1){
        PRO[44] <- '   Linfen_Wheat.CRO'
        PRO[53] <- '   Linfen.SOL'
        PRO[59] <- '   Linfen.SW0'
      }
      if(site_inf$FCA[i]==2){
        PRO[44] <- '   Yangling.CRO'
        PRO[53] <- '   Yangling.SOL'
        PRO[59] <- '   Yangling.SW0'
      }
      if(site_inf$FCA[i]==3){
        PRO[44] <- '   Changwu.CRO'
        PRO[53] <- '   Changwu.SOL'
        PRO[59] <- '   Changwu.SW0'
      } 
      
      if(is.na(sow$sowing[sow$year==paste((n-1),'-',n,sep = '')])==T){
        sowdata    <- 325
      } else if(sow$sowing[sow$year==paste((n-1),'-',n,sep = '')]==-99){
        sowdata    <- 325
      }else {
        sowdata    <- as.numeric(substr(sow$sowing[sow$year==paste((n-1),'-',n,sep = '')],5,7))
      }
      
      substr(PRO[c(5)], 3,7) <- as.character(36526+sowdata)
      
      DATEID <-  (which(Codays==sowdata, arr.ind = TRUE))      
      
      #substr(PRO[3], 52, 68) <- paste(DATEID[,1],' ', colnames(Codays)[DATEID[,2]],' ', 2001, sep = '')
      
      substr(PRO[5], 50, 68) <- paste(DATEID[,1],' ', colnames(Codays)[DATEID[,2]],' ', 2001, sep = '')
      
      write(PRO, "C:/ACsaV60/LIST/CHLP.PRO")
      
      
      CO2          <- readLines('C:/AQUACROP/SIMUL/MaunaLoa.CO2')
      
      y1 <- n-1
      
      y2 <- n
      
      CO2_RCP8.51  <- 757.44+(84.938-1.537*y1)/(2.2011-3.8289*y1^-0.45242)+2.4712*10^-4*(y1+15)^2+1.9299*10^-5*(y1-1937)^3+5.1137*10^-7*(y1-1910)^4#650.18+((0.000075326*y1-0.16276)/(0.00022299-727.97/(y1^2)))-0.00018747*(y1-2045)^3
      
      CO2_RCP8.52  <- 757.44+(84.938-1.537*y2)/(2.2011-3.8289*y2^-0.45242)+2.4712*10^-4*(y2+15)^2+1.9299*10^-5*(y2-1937)^3+5.1137*10^-7*(y2-1910)^4#650.18+((0.000075326*y2-0.16276)/ (0.00022299-727.97/(y2^2)))-0.00018747*(y2-2045)^3
      
      substr(CO2[64], 9, 14)    <-  as.character(round(CO2_RCP8.51,2))
      
      substr(CO2[65], 9, 14)    <-  as.character(round(CO2_RCP8.52,2))
      
      write(CO2,'C:/AQUACROP/SIMUL/MaunaLoa.CO2')
      
      system('C:/ACsaV60/ACsaV60.exe')
      
      
      print(paste("<----->",'<-->',i,'<-->',j,'<-->',n,'<-->',site_inf[i,4],'_',WHfiles[ID,2][j],'_',
                  WHfiles[ID,3][j], WHfiles[ID,4][j],'-','---->', sep = '')) 
      
      sim         <- read.table('C:/ACsaV60/OUTP/CHLPPROday.OUT', 
                                blank.lines.skip = F, skip  = 4, 
                                stringsAsFactors = F)
      
      outs       <-   c(sim$V4[which(sim$V5==2)[length(which(sim$V5==2))]],
                        sim$V4[which(sim$V5==4)[length(which(sim$V5==4))]],
                        apply(sim[,c(40,42)],2,max)*1000)
      
      SIMOUT     <- paste(paste(sow[sow$year==paste((n-1),'-',n,sep = ''), 1:5], collapse = '; '), 
                          paste(outs, collapse = '; '), sep = '; ')      
      
      write(SIMOUT, file = paste('E:/Researchs/Papers/Wheat climate suatiable/SIMOUTS/Auqacrop/RCP8.5/', 
                                 site_inf[i,4],'_',WHfiles[ID,2][j],'_',WHfiles[ID,3][j],
                                 '_smout.txt', sep = ''), append = T)
    }
    
  }
}
    
    
    
    
                                                                                
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
        
    