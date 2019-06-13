

{
#rm(list = ls()) 
source("libs.r")
source("fun_defs.r")
cdec_res_table <- "http://cdec.water.ca.gov/reportapp/javareports?name=RES"
cdec_res_table  <- readHTMLTable(cdec_res_table )
cdec_res_table  <- data.frame(cdec_res_table )

cdec_res_table  <- cdec_res_table %>% rename("res_name_cdec" = !!names(.[1]),
                                             "res_id_cdec" = !!names(.[2]),
                                             "res_cap_cdec" = !!names(.[3]),
                                             "res_storelev_cdec" = !!names(.[4]),
                                             "res_storvol_cdec" = !!names(.[5]) ,
                                             "res_storvoldlychnge_cdec" = !!names(.[6]),
                                             "res_percentcap_cdec" = !!names(.[7]),
                                             "res_avgstorvol_cdec" = !!names(.[8]),
                                             "res_percavgstorvol_cdec" = !!names(.[9])  ,                                          
                                             "res_outflow_cdec" = !!names(.[10]),
                                             "res_inflow_cdec" = !!names(.[11]),
                                             "res_storvol_prevyear_cdec" = !!names(.[12]))

cdec_res_table <- cdec_res_table %>% na.omit()                         
cdec_res_table <- cdec_res_table[!grepl("Reservoir Name", cdec_res_table$res_name_cdec),] #remove any row with Station Name
#as_tibble(cdec_res_table)
cdec_res_table$res_cap_cdec <- gsub(",", "", cdec_res_table$res_cap_cdec)
cdec_res_table$res_storelev_cdec <- gsub(",","", cdec_res_table$res_storelev_cdec)
cdec_res_table$res_storvol_cdec <- gsub(",", "", cdec_res_table$res_storvol_cdec)
cdec_res_table$res_storvoldlychnge_cdec <- gsub(",","", cdec_res_table$res_storvoldlychnge_cdec)
cdec_res_table$res_avgstorvol <- gsub(",", "", cdec_res_table$res_avgstorvol_cdec)
cdec_res_table$res_outflow <- gsub(",", "", cdec_res_table$res_outflow_cdec)
cdec_res_table$res_inflow <- gsub(",", "", cdec_res_table$res_inflow_cdec)
cdec_res_table$res_storvol_prevyear <- gsub(",", "", cdec_res_table$res_storvol_prevyear_cdec)
as_tibble(cdec_res_table)

#as_tibble(cdec_res_table)
cdec_res_table <- cdec_res_table %>%
                         mutate(
                           res_cap_cdec = as.character(res_cap_cdec), 
                           res_storelev_cdec = as.character(res_storelev_cdec),
                           res_storvol_cdec = as.character(res_storvol_cdec),
                           res_storvoldlychnge_cdec = as.character(res_storvoldlychnge_cdec),
                           res_percentcap_cdec = as.character(res_percentcap_cdec),
                           res_avgstorvol_cdec = as.character(res_avgstorvol_cdec),
                           res_percavgstorvol_cdec = as.character(res_percavgstorvol_cdec),
                           res_outflow_cdec = as.character(res_outflow_cdec),
                           res_inflow_cdec = as.character(res_inflow_cdec),
                           res_storvol_prevyear_cdec = as.character(res_storvol_prevyear) )
                           
as_tibble(cdec_res_table)     

cdec_res_table <- cdec_res_table %>%
                         mutate(
                           res_cap_cdec = round(as.double(res_cap_cdec)/1000, 0),
                           res_storelev_cdec = round(as.double(res_storelev_cdec),0),
                           res_storvol_cdec = round(as.double(res_storvol_cdec)/1000,0),
                           res_storvoldlychnge_cdec = round(as.double(res_storvoldlychnge_cdec)/1000,1),
                           res_percentcap_cdec = round(as.double(res_percentcap_cdec),0),
                           res_avgstorvol_cdec = round(as.double(res_avgstorvol)/1000,0),
                           res_percavgstorvol_cdec = round(as.double(res_percavgstorvol_cdec),0),
                           res_outflow_cdec = round(as.double(res_outflow),0),
                           res_inflow_cdec = round(as.double(res_inflow),0),
                           res_storvol_prevyear_cdec = round(as.double(res_storvol_prevyear)/10000),0 )
                           
cdec_res_table <- cdec_res_table %>% mutate(res_name_cdec = tolower(`res_name_cdec`), 
res_name_cdec = sapply(res_name_cdec, CapStr)) 

cdec_res_table$res_name_cdec <- gsub("buchanan", "Buchanan", cdec_res_table$res_name_cdec)
cdec_res_table$res_name_cdec <- gsub("hidden", "Hidden", cdec_res_table$res_name_cdec)
cdec_res_table$res_name_cdec <- gsub("exchequer", "Exchequer", cdec_res_table$res_name_cdec)
cdec_res_table$res_name_cdec <- gsub("friant", "Friant", cdec_res_table$res_name_cdec)
cdec_res_table$res_name_cdec <- gsub("terminus", "Terminus", cdec_res_table$res_name_cdec)
cdec_res_table$res_name_cdec <- gsub("coyote", "Coyote", cdec_res_table$res_name_cdec)
cdec_res_table$res_name_cdec <- gsub("Sonoma(warm Springs)", "Sonoma (Warm Springs)", cdec_res_table$res_name_cdec)
cdec_res_table$res_name_cdec <- gsub("warm", "Warm", cdec_res_table$res_name_cdec)
cdec_res_table$res_name_cdec <- gsub("Sonoma", "Sonoma ", cdec_res_table$res_name_cdec)
cdec_res_table$res_name_cdec <- gsub("Donnell's", "Donnell", cdec_res_table$res_name_cdec)
cdec_res_table$res_name_cdec <- gsub("Mc Clure", "McClure", cdec_res_table$res_name_cdec)
cdec_res_table$res_name_cdec <- gsub("Lake", "", cdec_res_table$res_name_cdec)
                       
                           
#cdec_res_table %>% transmute(res_name_cdec, res_id_cdec) %>% write_csv("cdec_res_table_addXYs.csv")                           
                          
cdec_res_coords <- read_csv("cdec_reservoir_coords.csv") %>% select(-res_name_cdec)

cdec_res_table <- left_join(cdec_res_table, cdec_res_coords)
rm(cdec_res_coords)
cdec_res_table <- st_as_sf(cdec_res_table, coords = c("lon_middleres_wgs84", "lat_middleres_wgs84"), crs = 4326) 


maptypes = c(
  "Stamen.TopOSMRelief", 
  "Stamen.TerrainBackground",
  "NASAGIBS.ModisTerraTrueColorCR",
  #"ESRI.WorldImagery",
  "Esri.WorldPhysical",  
  "OpenTopoMap" )

mapview(cdec_res_table["res_storvol_cdec"], 
        col.regions = viridisLite::viridis, 
        cex = cdec_res_table$res_storvol_cdec/200,
        color = "green",
        alpha.regions = 0.3,
        map.types = maptypes,
        legend = FALSE,
        popup = popupTable(cdec_res_table, zcol = c("res_name_cdec",
                                                   "res_id_cdec",
                                                   "res_cap_cdec",
                                                   "res_storelev_cdec",
                                                   "res_storvol_cdec",   
                                                   "res_storvoldlychnge_cdec",
                                                   "res_percentcap_cdec",   
                                                   "res_avgstorvol_cdec",
                                                   "res_percavgstorvol_cdec",
                                                   "res_outflow_cdec",
                                                   "res_inflow_cdec",
                                                   "res_storvol_prevyear_cdec" )),
        
        layer.name = "reservoir storage (taf)")       
  
} 


#  mapview(#cdec_res_table["res_cap_cdec"], 
#    col.regions = viridisLite::viridis, 
#    cex = cdec_res_table$res_cap_cdec,
#    color = "green",
#    alpha.regions = 0.0,
#    map.types = maptypes,
#    legend = FALSE,
#    popup = popupTable(cdec_res_table, zcol = c("res_name_cdec",
#                                              "res_id_cdec",
#                                              "res_cap_cdec",
#                                              "res_storelev_cdec",
#                                              "res_storvol_cdec",   
#                                              "res_storvoldlychnge_cdec",
#                                              "res_percentcap_cdec",   
#                                              "res_avgstorvol_cdec",
#                                              "res_percavgstorvol_cdec",
#                                              "res_outflow_cdec",
#                                              "res_inflow_cdec",
#                                              "res_storvol_prevyear_cdec" )),
#    
#    layer.name = "reservoir capacity (taf)")     
