

{
rm(list = ls()) 
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
                                             "res_avgstorvol" = !!names(.[8]),
                                             "res_percavgstorvol" = !!names(.[9])  ,                                          
                                             "res_outflow" = !!names(.[10]),
                                             "res_inflow" = !!names(.[11]),
                                             "res_storvol_prevyear" = !!names(.[12]))

cdec_res_table <- cdec_res_table %>% na.omit()                         
cdec_res_table <- cdec_res_table[!grepl("Reservoir Name", cdec_res_table$res_name_cdec),] #remove any row with Station Name
#as_tibble(cdec_res_table)
cdec_res_table$res_cap_cdec <- gsub(",", "", cdec_res_table$res_cap_cdec)
cdec_res_table$res_storelev_cdec <- gsub(",","", cdec_res_table$res_storelev_cdec)
cdec_res_table$res_storvol_cdec <- gsub(",", "", cdec_res_table$res_storvol_cdec)
cdec_res_table$res_storvoldlychnge_cdec <- gsub(",","", cdec_res_table$res_storvoldlychnge_cdec)
cdec_res_table$res_avgstorvol <- gsub(",", "", cdec_res_table$res_avgstorvol)
cdec_res_table$res_outflow <- gsub(",", "", cdec_res_table$res_outflow)
cdec_res_table$res_inflow <- gsub(",", "", cdec_res_table$res_inflow)
cdec_res_table$res_storvol_prevyear <- gsub(",", "", cdec_res_table$res_storvol_prevyear)
as_tibble(cdec_res_table)

#as_tibble(cdec_res_table)
cdec_res_table <- cdec_res_table %>%
                         mutate(
                           res_cap_cdec = as.character(res_cap_cdec), 
                           res_storelev_cdec = as.character(res_storelev_cdec),
                           res_storvol_cdec = as.character(res_storvol_cdec),
                           res_storvoldlychnge_cdec = as.character(res_storvoldlychnge_cdec),
                           res_percentcap_cdec = as.character(res_percentcap_cdec),
                           res_avgstorvol = as.character(res_avgstorvol),
                           res_percavgstorvol = as.character(res_percavgstorvol),
                           res_outflow = as.character(res_outflow),
                           res_inflow = as.character(res_inflow),
                           res_storvol_prevyear = as.character(res_storvol_prevyear) )
                           
as_tibble(cdec_res_table)     

cdec_res_table <- cdec_res_table %>%
                         mutate(
                           res_cap_cdec = as.double(res_cap_cdec), 
                           res_storelev_cdec = as.double(res_storelev_cdec),
                           res_storvol_cdec = as.double(res_storvol_cdec),
                           res_storvoldlychnge_cdec = as.double(res_storvoldlychnge_cdec),
                           res_percentcap_cdec = as.double(res_percentcap_cdec),
                           res_avgstorvol = as.double(res_avgstorvol),
                           res_percavgstorvol = as.double(res_percavgstorvol),
                           res_outflow = as.double(res_outflow),
                           res_inflow = as.double(res_inflow),
                           res_storvol_prevyear = as.double(res_storvol_prevyear) )
                           
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
}                        
                           
#cdec_res_table %>% transmute(res_name_cdec, res_id_cdec) %>% write_csv("cdec_res_table_addXYs.csv")                           
                          
cdec_res_coords <- read_csv("cdec_reservoir_coords.csv") %>% select(-res_name_cdec)

cdec_res_table <- left_join(cdec_res_table, cdec_res_coords)
cdec_res_table <- st_as_sf(cdec_res_table, coords = c("lon_middleres_wgs84", "lat_middleres_wgs84"), crs = 4326) 
