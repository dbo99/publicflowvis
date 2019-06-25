
{
rm(list = ls()) 
mainkeepers <- c("shasta_fnf", "friant_fnf", "newmelones_fnf", "pineflat_fnf", "cdec_dailysum", "mainkeepers", "usbr_fnf", "corps_fnf", "df")
rm(list= ls()[!(ls() %in% mainkeepers)])
setwd("~/Documents/publicflowvis")
source("libs.r")
source("fun_defs.r")
}

#### full natural flow ####
# usbr_fnf
#{
source("shasta_fnf_scrape_2.r")
source("friant_fnf_scrape_2.r")
source("newmelones_fnf_scrape_2.r")
# corps_fnf
source("pineflat_fnf_scrape_2.r")

###  reservoir dailies ###
# cdec
source("cdec_dailyressum_2.r")


usbr_fnf <- rbind(shasta_fnf, friant_fnf, newmelones_fnf) %>% mutate(source = "usbr")
as_tibble(usbr_fnf)

corps_fnf <- pineflat_fnf %>% mutate(source = "corps")
as_tibble(corps_fnf)

cdec_dailysum <- cdec_dailysum %>% mutate(source = "cdec")
as_tibble(cdec_dailysum)


#{
usbr_fnf <- usbr_fnf %>% unite(params, param_usbr, timeseq_usbr, unit_usbr, meastype_usbr, source) %>%  spread(params, value_usbr)
as_tibble(usbr_fnf)
corps_fnf <- corps_fnf %>% unite(params, param_corps, timeseq_corps, unit_corps, meastype_corps, source) %>%  spread(params, value_corps)
as_tibble(corps_fnf)
cdec_dailysum <- cdec_dailysum %>% unite(params, param_cdec, timeseq_cdec, unit_cdec, meastype_cdec, source) %>%  spread(params, value_cdec)
as_tibble(cdec_dailysum)
#}

{
  res_coords <- read_csv("res_coords.csv") %>% mutate(nwsid = tolower(nwsid)) %>% select(-res_cdec)
  as_tibble(res_coords ) } 

#{
  
  cdec_dailysum <- right_join(cdec_dailysum, res_coords, by = c("cdecid"), na_matches = "never") %>% select(-res_cdec)

  usbrfnf_cdecsum <- right_join(usbr_fnf, cdec_dailysum, by = c("nwsid"), na_matches = "never")  %>% select( -res_usbr)

  usbrfnf_cdecsum_corpsfnf <- right_join(corps_fnf,usbrfnf_cdecsum, by = c("nwsid"), na_matches = "never") %>% select(-res_corps)

  
  df_w <- usbrfnf_cdecsum_corpsfnf
#}



df_w <- df_w[!is.na(df_w$lat_middleres_wgs84),]
df_w <- st_as_sf(df_w, coords = c("lon_middleres_wgs84", "lat_middleres_wgs84"), crs = 4326) 
df_w <- df_w[,order(colnames(df_w))]
as_tibble(df_w)
colnames(df_w)


# normalize - need single value if two agencies have same data
#storage
df_w <- df_w %>% mutate(storage_latest_af_instant = ifelse(is.na(storage_latest_af_instant_cdec), storage_latest_af_instant_usbr, storage_latest_af_instant_cdec))
df_w <- df_w %>% mutate(storage_dailychange_af_instant = ifelse(is.na(storage_dailychange_af_instant_cdec), storage_dailychange_af_instant_usbr, storage_dailychange_af_instant_cdec))
df_w <- df_w %>% mutate(storage_latest_percentfull_instant = ifelse(is.na(storage_latest_percentfull_instant_cdec), storage_latest_percentfull_instant_usbr, storage_latest_percentfull_instant_cdec))
df_w <- df_w %>% mutate(storage_dailychange_percentchange_instant = ifelse(is.na(storage_dailychange_percentchange_instant_cdec), storage_dailychange_percentchange_instant_usbr, storage_dailychange_percentchange_instant_cdec))
# inflow
df_w <- df_w %>% mutate(inflow_latest_cfs_meandly = ifelse(is.na(inflow_latest_cfs_meandly_cdec), inflow_latest_cfs_meandly_usbr, inflow_latest_cfs_meandly_cdec))
df_w <- df_w %>% mutate(inflow_latest_af_meandly = ifelse(is.na(inflow_latest_af_meandly_cdec), inflow_latest_af_meandly_usbr, inflow_latest_af_meandly_cdec))


df_w <- df_w[,order(colnames(df_w))]
colnames(df_w)

df_w <- df_w %>% mutate(storage_dailychange_af_instant_cdec_abs = abs(storage_dailychange_af_instant_cdec))


maptypes = c(

  "Stamen.TopOSMRelief", 
 
  "Esri.WorldPhysical",  
  "OpenTopoMap" )


grp <- c(    "usgs hydrography",   "0.5 reflectivity","hrrr p_1hr", "hrrr p_2hr",   "hrrr p_4hr", "hrrr p_6hr",
             "mrms p_1hr", "mrms p_24hr", "mrms p_48hr", "mrms p_72hr") # "Coarse Geo") # 


lopt = labelOptions(noHide = TRUE,
                    direction = 'top',
                    textOnly = TRUE)
#pal <- colorNumeric(c("#d7191c","#fdae61","#ffffbf","#abd9e9", "#2c7bb6"), df_w$storage_dailychange_af_instant_cdec, reverse = TRUE)
pal = mapviewPalette("mapviewSpectralColors")
#m <- mapview(df_w["inflow_latest_af_meandly_cdec_2019-06-24"], #burst = TRUE, hide = TRUE, 
m <- mapview(df_w["storage_dailychange_af_instant_cdec"], #burst = TRUE, hide = TRUE, 
             #col.regions = RColorBrewer::RdBu, 
             col.regions = pal(100),
             #at = seq(min_rt, max_rt+2500, 2500),
           cex = df_w$storage_dailychange_af_instant_cdec_abs/1000,  #24
           #cex = df_w[31],  #29
             #cex = "storage_dailychange_af_instant_usbr_2019-06-23",  #14          
             alpha.regions = 0.3,
             map.types = maptypes,
             legend = TRUE,
             popup = popupTable(df_w),
             #popup = popupTable(gage_points, zcol = c("STATION_NM", "usgs_id", "cfs", "stage_ft", "hist_pkflow",
             #                                         "usgs_nwsfloodstg_ft", "dt")),
             #lwd = nhd_fyh_sf$Q0001A/1000,
             layer.name =  "layername") 

  m


colnames(df_w)
      
