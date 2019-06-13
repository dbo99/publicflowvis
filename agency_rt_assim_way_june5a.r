

{
  rm(list = ls()) 
  #setwd("~/R/proj/publicflowscrapeapp")
  #setwd("~/Documents/publicflowvis")
  source("libs.r")
  source("fun_defs.r")
  source("usgs_scraper.r")
  source("cdec_scraper__flowscrape_jointotablecoords.r")
  cnrfc <- read_csv("db33river_gage.csv") %>% mutate(name_cnrfc = paste0(river, " - ", local)) %>% select(-river, -local)
  usgs_nws_key <- read_csv("usgs_to_nws_key.csv")
  cdec_nws_key <- read_csv("cdec_to_nws_key.csv")
  
  usgs_to_cdec <- left_join(cdec_nws_key, usgs_nws_key, na_matches = "never")
  cdec_to_usgs <- left_join(usgs_nws_key, cdec_nws_key, na_matches = "never")
  
  nws_usgs_cdec_key <- rbind(usgs_to_cdec, cdec_to_usgs) %>% distinct()
  #nws_usgs_cdec_key <- left_join(nws_usgs_cdec_key, cnrfc, na_matches = "never")  ## add rfc metadata
  rm(usgs_nws_key, cdec_nws_key, usgs_to_cdec, cdec_to_usgs)
  
  
  cdec <- left_join(cdec, nws_usgs_cdec_key, na_matches = "never")
  usgs <- left_join(usgs,nws_usgs_cdec_key,  na_matches = "never")
  cdec <- cdec[,order(colnames(cdec))]
  usgs <- usgs[,order(colnames(usgs))]
  colnames(cdec)
  colnames(usgs)
  
  cdec_usgs_cnrfc1 <- left_join(cdec, usgs, na_matches = "never")
  cdec_usgs_cnrfc2 <- left_join(usgs, cdec, na_matches = "never")
  cdec_usgs_cnrfc1 <- cdec_usgs_cnrfc1[,order(colnames(cdec_usgs_cnrfc1))]
  cdec_usgs_cnrfc2 <- cdec_usgs_cnrfc2[,order(colnames(cdec_usgs_cnrfc2))]
  colnames(cdec_usgs_cnrfc1)
  colnames(cdec_usgs_cnrfc2)
  cdec_usgs_cnrfc <- rbind(cdec_usgs_cnrfc1, cdec_usgs_cnrfc2) %>% distinct()
  rm(cdec_usgs_cnrfc1, cdec_usgs_cnrfc2, nws_usgs_cdec_key)
  cdec_usgs_cnrfc <- left_join(cdec_usgs_cnrfc, cnrfc) %>% 
    mutate(cfs_most_recent = case_when(
      is.na(cfs_usgs) | is.na(cfs_cdec) ~ coalesce(cfs_usgs, cfs_cdec),
      most_recent_cdec >= most_recent_usgs ~ cfs_cdec,
      TRUE ~ cfs_usgs),
      lat = if_else(is.na(lat_usgs), lat_cdec, lat_usgs),   
      lon = if_else(is.na(lon_usgs), lon_cdec, lon_usgs))
  cdec_usgs_cnrfc <- cdec_usgs_cnrfc[,order(colnames(cdec_usgs_cnrfc))]
  
  rm(cnrfc, usgs, cdec)
  colnames(cdec_usgs_cnrfc)
  
  # convert to spatial object
  cdec_usgs_cnrfc <- cdec_usgs_cnrfc[!is.na(cdec_usgs_cnrfc$lat),]  #remove any rows with NAs for lat - no blank row for st_as_sf()
  cdec_usgs_cnrfc <- st_as_sf(cdec_usgs_cnrfc, coords = c("lon", "lat"), crs = 4326) 
  as_tibble(cdec_usgs_cnrfc)
  as_tibble(tail(cdec_usgs_cnrfc))
  
  cdec_usgs_cnrfc <- cdec_usgs_cnrfc %>% mutate(group_to_visualize = 
                                                  case_when(
                                                    cfs_most_recent <100  ~ 1.55,#0.70,                                  
                                                    cfs_most_recent <200  ~ 1.60,#0.74,
                                                    cfs_most_recent <300  ~ 1.65,#0.78,                          
                                                    cfs_most_recent <400  ~ 1.70,#0.82,                           
                                                    cfs_most_recent <500  ~ 1.75,#0.86,                            
                                                    cfs_most_recent <600  ~ 1.80,#0.90,
                                                    cfs_most_recent <700  ~ 1.85,#0.94,
                                                    cfs_most_recent <800  ~ 1.90,#0.98,                          
                                                    cfs_most_recent <900  ~ 1.95,#1.03,                           
                                                    cfs_most_recent <1000 ~ 2.0,#1.1,
                                                    cfs_most_recent <1500 ~ 2.5,#1.5,                                    
                                                    cfs_most_recent <2000 ~ 3.0,#2.0,
                                                    cfs_most_recent <2500 ~ 3.5,#2.5,
                                                    cfs_most_recent <3000 ~ 4.0,#3.0,
                                                    cfs_most_recent <3500 ~ 4.5,#3.5,
                                                    cfs_most_recent <4000 ~ 5.0,#4.0,
                                                    cfs_most_recent <4500 ~ 5.5,#4.5,
                                                    cfs_most_recent <5000 ~ 6.0,#5.0,
                                                    cfs_most_recent <5500 ~ 6.5,#5.5,
                                                    cfs_most_recent <6000 ~ 7.0,#6.0,
                                                    cfs_most_recent <6500 ~ 7.5,#6.5,
                                                    cfs_most_recent <7000 ~ 8.0,#7.0,
                                                    cfs_most_recent <7500 ~ 8.5,#7.5,
                                                    cfs_most_recent <8000 ~ 9.0,#8.0,
                                                    cfs_most_recent <8500 ~ 9.5,#8.5,
                                                    cfs_most_recent <9000 ~ 10.0,#9.0,
                                                    cfs_most_recent <9500 ~ 10.5,#9.5,
                                                    cfs_most_recent <10000 ~11.0,# 10.0,
                                                    cfs_most_recent <11000 ~11.5,# 10.5,
                                                    cfs_most_recent <12000 ~12.0,# 11.0,
                                                    cfs_most_recent <13000 ~12.5,# 11.5,                                   
                                                    cfs_most_recent <14000 ~13.0,# 12.0,
                                                    cfs_most_recent <15000 ~13.5,# 12.5,
                                                    cfs_most_recent <16000 ~14.0,# 13.0,
                                                    cfs_most_recent <17000 ~14.5,# 13.5,
                                                    cfs_most_recent <18000 ~15.0,# 14.0,
                                                    cfs_most_recent <19000 ~15.5,# 14.5,
                                                    cfs_most_recent <20000 ~16.0,# 15.0,
                                                    cfs_most_recent <22500 ~16.5,# 15.5,
                                                    cfs_most_recent <25000 ~17.0,# 16.0,
                                                    cfs_most_recent <30000 ~18.0,# 17.0,
                                                    cfs_most_recent <35000 ~19.0,# 18.0,
                                                    cfs_most_recent <40000 ~20.0,# 19.0,
                                                    cfs_most_recent <45000 ~21.0,# 20.0,
                                                    cfs_most_recent <50000 ~22.0,# 20.1,
                                                    cfs_most_recent <60000 ~23.0,# 20.2,
                                                    cfs_most_recent <70000 ~24.0,# 20.3,
                                                    cfs_most_recent <80000 ~25.0,# 20.4,
                                                    cfs_most_recent <90000 ~26.0,# 20.5,
                                                    cfs_most_recent <100000 ~ 27.0,# 20.6,
                                                    cfs_most_recent <150000 ~ 28.0,# 20.7,
                                                    cfs_most_recent <300000 ~ 29.0,# 20.8,
                                                    cfs_most_recent <500000 ~ 30.0,# 20.9,
                                                    cfs_most_recent >1000000 ~31.0, #20.95,
                                                    cfs_most_recent >5000000 ~32.0)) #20.99))
  
  cdec_usgs_cnrfc <- cdec_usgs_cnrfc %>% filter(lat_cdec < 43 | lat_usgs < 43 ) 
}
