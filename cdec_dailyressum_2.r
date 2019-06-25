

{

  rm(list= ls()[!(ls() %in% mainkeepers)])
setwd("~/Documents/publicflowvis")
source("libs.r")



source("libs.r")
source("fun_defs.r")
cdec_res_table <- "http://cdec.water.ca.gov/reportapp/javareports?name=RES"
#cdec_res_table <- "https://cdec.water.ca.gov/reportapp/javareports?name=FNF"
cdec_res_table  <- readHTMLTable(cdec_res_table )
cdec_res_table  <- data.frame(cdec_res_table )

reportdate <- Sys.Date()  #cant scrape date from cdec's page!
                                                                                        #param        #source #timeseq#meastype  #unit  #date          
cdec_res_table  <- cdec_res_table %>% rename("res" = !!names(.[1]),
                                             "cdecid" = !!names(.[2]),
                                             "totalcapacity_cdec_latest_instant_af_date" = !!names(.[3]),            # totalcapacity_cdec_latest_instant_af_date
                                             "elevation_cdec_latest_instant_ft_date" = !!names(.[4]),       # elevation_cdec_latest_instant_ft_date
                                             "storage_cdec_latest_instant_af_date" = !!names(.[5]) ,       # storage_cdec_latest_instant_af_date
                                             "storage_cdec_dailychange_instant_af_date" = !!names(.[6]),            # storage_cdec_dailychange_instant_af_date
                                             "storage_cdec_latest_instant_percentfull_date" = !!names(.[7]),     # storage_cdec_latest_instant_percentfull_date
                                             "histstorvol_cdec_hist_histmean_af_date" = !!names(.[8]),          # histstorvol_cdec_hist_histmean_af_date
                                             "histstorval_cdec_hist_histmean_percent_date" = !!names(.[9])  ,    # histstorval_cdec_hist_histmean_percent_date                                     
                                             "outflow_cdec_latest_meandly_cfs_date" = !!names(.[10]),            # outflow_cdec_latest_meandly_cfs_date
                                             "inflow_cdec_latest_meandly_cfs_date" = !!names(.[11]),             # inflow_cdec_latest_meandly_cfs_date
                                             "storage_cdec_prevyeartoday_instant_af_date" = !!names(.[12]))   # storage_cdec_prevyeartoday_instant_af_date

cdec_res_table <- cdec_res_table %>% na.omit()                         
cdec_res_table <- cdec_res_table[!grepl("Reservoir Name", cdec_res_table$res),] #remove any row with Station Name
#as_tibble(cdec_res_table)
cdec_res_table$totalcapacity_cdec_latest_instant_af_date <- gsub(",", "", cdec_res_table$totalcapacity_cdec_latest_instant_af_date)
cdec_res_table$elevation_cdec_latest_instant_ft_date <- gsub(",","", cdec_res_table$elevation_cdec_latest_instant_ft_date)
cdec_res_table$storage_cdec_latest_instant_af_date <- gsub(",", "", cdec_res_table$storage_cdec_latest_instant_af_date)
cdec_res_table$storage_cdec_dailychange_instant_af_date <- gsub(",","", cdec_res_table$storage_cdec_dailychange_instant_af_date)
cdec_res_table$histstorvol_cdec_hist_histmean_af_date <- gsub(",", "", cdec_res_table$histstorvol_cdec_hist_histmean_af_date)
cdec_res_table$outflow_cdec_latest_meandly_cfs_date <- gsub(",", "", cdec_res_table$outflow_cdec_latest_meandly_cfs_date)
cdec_res_table$inflow_cdec_latest_meandly_cfs_date <- gsub(",", "", cdec_res_table$inflow_cdec_latest_meandly_cfs_date)
cdec_res_table$storage_cdec_prevyeartoday_instant_af_date <- gsub(",", "", cdec_res_table$storage_cdec_prevyeartoday_instant_af_date)
as_tibble(cdec_res_table)

#as_tibble(cdec_res_table)
cdec_res_table <- cdec_res_table %>%
                         mutate(
                           totalcapacity_cdec_latest_instant_af_date = as.character(totalcapacity_cdec_latest_instant_af_date), 
                           elevation_cdec_latest_instant_ft_date = as.character(elevation_cdec_latest_instant_ft_date),
                           storage_cdec_latest_instant_af_date = as.character(storage_cdec_latest_instant_af_date),
                           storage_cdec_dailychange_instant_af_date = as.character(storage_cdec_dailychange_instant_af_date),
                           storage_cdec_latest_instant_percentfull_date = as.character(storage_cdec_latest_instant_percentfull_date),
                           histstorvol_cdec_hist_histmean_af_date = as.character(histstorvol_cdec_hist_histmean_af_date),
                           histstorval_cdec_hist_histmean_percent_date = as.character(histstorval_cdec_hist_histmean_percent_date),
                           outflow_cdec_latest_meandly_cfs_date = as.character(outflow_cdec_latest_meandly_cfs_date),
                           inflow_cdec_latest_meandly_cfs_date = as.character(inflow_cdec_latest_meandly_cfs_date),
                           storage_cdec_prevyeartoday_instant_af_date = as.character(storage_cdec_prevyeartoday_instant_af_date) )
                           
as_tibble(cdec_res_table)     

cdec_res_table <- cdec_res_table %>%
                         mutate(
                           totalcapacity_cdec_latest_instant_af_date = as.double(totalcapacity_cdec_latest_instant_af_date), 
                           elevation_cdec_latest_instant_ft_date = as.double(elevation_cdec_latest_instant_ft_date),
                           storage_cdec_latest_instant_af_date = as.double(storage_cdec_latest_instant_af_date),
                           storage_cdec_dailychange_instant_af_date = as.double(storage_cdec_dailychange_instant_af_date),
                           storage_cdec_latest_instant_percentfull_date = as.double(storage_cdec_latest_instant_percentfull_date),
                           histstorvol_cdec_hist_histmean_af_date = as.double(histstorvol_cdec_hist_histmean_af_date),
                           histstorval_cdec_hist_histmean_percent_date = as.double(histstorval_cdec_hist_histmean_percent_date),
                           outflow_cdec_latest_meandly_cfs_date = as.double(outflow_cdec_latest_meandly_cfs_date),
                           inflow_cdec_latest_meandly_cfs_date = as.double(inflow_cdec_latest_meandly_cfs_date),
                           storage_cdec_prevyeartoday_instant_af_date = as.double(storage_cdec_prevyeartoday_instant_af_date) )
                           
cdec_res_table <- cdec_res_table %>% mutate(res = tolower(`res`), 
res = sapply(res, CapStr)) 

cdec_res_table$res <- gsub("buchanan", "Buchanan", cdec_res_table$res)
cdec_res_table$res <- gsub("hidden", "Hidden", cdec_res_table$res)
cdec_res_table$res <- gsub("exchequer", "Exchequer", cdec_res_table$res)
cdec_res_table$res <- gsub("friant", "Friant", cdec_res_table$res)
cdec_res_table$res <- gsub("terminus", "Terminus", cdec_res_table$res)
cdec_res_table$res <- gsub("coyote", "Coyote", cdec_res_table$res)
cdec_res_table$res <- gsub("Sonoma(warm Springs)", "Sonoma (Warm Springs)", cdec_res_table$res)
cdec_res_table$res <- gsub("warm", "Warm", cdec_res_table$res)
cdec_res_table$res <- gsub("Sonoma", "Sonoma ", cdec_res_table$res)
cdec_res_table$res <- gsub("Donnell's", "Donnell", cdec_res_table$res)
cdec_res_table$res <- gsub("Mc Clure", "McClure", cdec_res_table$res)
cdec_res_table$res <- gsub("Lake", "", cdec_res_table$res)
                      


cdec_res_table <- cdec_res_table %>% gather(key = "param_source_timeseq_meastype_unit_date", value = "value", 3:length(cdec_res_table))
as_tibble(cdec_res_table)
cdec_res_table <- cdec_res_table %>%  separate(param_source_timeseq_meastype_unit_date, into = c("param_source_timeseq_meastype_unit", "date"), sep="_(?=[^_]+$)") 
as_tibble(cdec_res_table)                         
cdec_res_table <- cdec_res_table %>%  separate(param_source_timeseq_meastype_unit, into = c("param_source_timeseq_meastype", "unit"), sep="_(?=[^_]+$)") 
as_tibble(cdec_res_table)  
cdec_res_table <- cdec_res_table %>%  separate(param_source_timeseq_meastype, into = c("param_source_timeseq", "meastype"), sep="_(?=[^_]+$)") 
as_tibble(cdec_res_table)  
cdec_res_table <- cdec_res_table %>%  separate(param_source_timeseq, into = c("param_source", "timeseq"), sep="_(?=[^_]+$)") 
as_tibble(cdec_res_table)  



cdec_res_table <- cdec_res_table %>%  separate(param_source, into = c("param", "source"), sep="_(?=[^_]+$)") 
as_tibble(cdec_res_table)   

cdec_res_table <- cdec_res_table %>% mutate(res = trimws(res)) %>% mutate(date = as.Date(reportdate))
as_tibble(cdec_res_table)           

cdec_res_table <- cdec_res_table %>% mutate( date = as.Date(date))
as_tibble(cdec_res_table)  






cdec_res_table_percempty <- cdec_res_table %>% filter(unit == "percentfull") %>% 
                            mutate(value = 100 - value) %>%
                            mutate(value = round(value, 1)) %>%
                            mutate(unit = "percentempty")
cdec_res_table <- rbind(cdec_res_table, cdec_res_table_percempty)
  


cdec_res_table_dlychange <- cdec_res_table %>% filter(timeseq == "dailychange", param == "storage") %>% select(res, cdecid, date, value) 
as_tibble(cdec_res_table_dlychange)
cdec_res_table_latest <- cdec_res_table %>% filter(timeseq == "latest", param == "storage", unit == "af")
as_tibble(cdec_res_table_latest)
cdec_res_table_dlychange <- right_join(cdec_res_table_latest, cdec_res_table_dlychange, by = c("res", "cdecid", "date")) %>%
                            mutate(value = value.y/(value.x - value.y) * 100 ) %>% mutate(value = round(value, 1)) %>%
                            mutate(unit = "percentchange") %>%
                            mutate(timeseq = "dailychange") %>%
                            select(-value.x, -value.y)
as_tibble(cdec_res_table_dlychange)



cdec_res_table <- rbind(cdec_res_table, cdec_res_table_dlychange ) 


## add inflow volume class from cfs

cdec_res_table_inflowdlyaf <- cdec_res_table %>% filter(param == "inflow", meastype == "meandly", unit == "cfs") %>% mutate(value = value * 1.98347, unit = "af")
cdec_res_table <- rbind(cdec_res_table, cdec_res_table_inflowdlyaf) 

## add outflow volume class from cfs

cdec_res_table_outflowdlyaf <- cdec_res_table %>% filter(param == "outflow", meastype == "meandly", unit == "cfs") %>% mutate(value = value * 1.98347, unit = "af")
cdec_res_table <- rbind(cdec_res_table, cdec_res_table_outflowdlyaf) 



cdec_res_table <- cdec_res_table %>% select(-source) %>% rename_all(paste0, "_cdec") %>% 
                               mutate(cdecid = cdecid_cdec) %>% select(-cdecid_cdec)


cdec_dailysum <- cdec_res_table




#}  

#cdec_res_coords <- read_csv("cdec_reservoir_coords.csv") %>% select(-res)
#
#cdec_res_table <- left_join(cdec_res_table, cdec_res_coords)
#cdec_res_table <- st_as_sf(cdec_res_table, coords = c("lon_middleres_wgs84", "lat_middleres_wgs84"), crs = 4326) 
rm(list= ls()[!(ls() %in% mainkeepers)])

}
