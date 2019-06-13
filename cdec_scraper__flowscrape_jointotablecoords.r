library(tidyverse)

{
cdec_mainrttable <- "http://cdec.water.ca.gov/dynamicapp/getAll?sens_num=20"
cdec_mainrttable <- readHTMLTable(cdec_mainrttable)
cdec_mainrttable <- data.frame(cdec_mainrttable)

cdec_mainrttable  <- cdec_mainrttable %>% rename("name_cdec" = !!names(.[1]),
                                                 "id_cdec" = !!names(.[2]),
                                                 "gelev_ft_cdec" = !!names(.[3]),
                                                 "most_recent_cdec" = !!names(.[4]),
                                                 "cfs_cdec" = !!names(.[5])  )
cdec_mainrttable <- cdec_mainrttable %>% na.omit()                         
cdec_mainrttable <- cdec_mainrttable[!grepl("Station Name", cdec_mainrttable$name_cdec),] #remove any row with Station Name
#as_tibble(cdec_mainrttable)
cdec_mainrttable$elev_ft <- gsub("'", "", cdec_mainrttable$gelev_ft_cdec)
cdec_mainrttable$cfs <- gsub(",", "", cdec_mainrttable$cfs)
#as_tibble(cdec_mainrttable)
cdec_mainrttable$cfs <- gsub("CFS", "", cdec_mainrttable$cfs)
#as_tibble(cdec_mainrttable)
cdec_mainrttable <- cdec_mainrttable %>% mutate(gelev_ft_cdec = as.double(elev_ft), cfs = as.double(cfs),
                                              most_recent_cdec = mdy_hm(most_recent_cdec))
as_tibble(cdec_mainrttable)

cdec_meta <- read_csv("cdec_meta_5.22.19.csv") 

}

#### join coordinates to flow data (and rename/drop some columns)#####

cdec <- inner_join(cdec_meta, cdec_mainrttable, by = "id_cdec") %>%
       transmute(id_cdec, basin_cdec, group_cdec, lat_cdec , lon_cdec , nearby_cty_cdec, gage_oper_cdec, data_collect_cdec, county_cdec, 
                  name_cdec = name_cdec.x, gelev_ft_cdec = gelev_ft_cdec.x, most_recent_cdec, cfs_cdec = cfs)
as_tibble(cdec)

#cdec <- cdec %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "identity")

#}
as_tibble(cdec)
rm(cdec_mainrttable, cdec_meta)
cdec <- distinct(cdec)

#maptypes = c("Stamen.TonerLite",
#             "Stamen.Terrain",
#             "Stamen.TopOSMRelief",
#             "Esri.WorldTopoMap" ,
#             "Esri.WorldPhysical",
#             "OpenTopoMap" )
#
#lopt = labelOptions(noHide = TRUE,
#                    direction = 'top',
#                    textOnly = TRUE)
#
#m <- mapview(cdec["cfs"], #burst = TRUE, hide = TRUE, 
#             col.regions = viridisLite::viridis, 
#            # at = seq(0, 15000, 1000),
#             cex = "cfs",
#             alpha.regions = 0.3,
#             map.types = maptypes,
#             legend = TRUE,
#             popup  = popupTable(cdec, zcol = c("id_cdec", "basin", "cfs",  "most_recent", "name_cdec", "elev_ft",
#                                                 "nearby_cty", "county", "gage_oper_cdec")),
#             layer.name = "cfs") 
#
#
#m <-  addStaticLabels(m, data = cdec, label = cdec$cfs, labelOptions = lopt)
#
#m
#m@map
#rm(cdec_mainrttable)







































