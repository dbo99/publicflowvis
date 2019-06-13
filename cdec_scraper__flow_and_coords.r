#rm(list = ls()) 
#setwd("~/Documents/feathyuba/cdecgage")


{
cdec_mainrttable <- "http://cdec.water.ca.gov/dynamicapp/getAll?sens_num=20"
cdec_mainrttable <- readHTMLTable(cdec_mainrttable)
cdec_mainrttable <- data.frame(cdec_mainrttable)

cdec_mainrttable  <- cdec_mainrttable %>% rename("cdec_name" = !!names(.[1]),
                                                 "cdec_code" = !!names(.[2]),
                                                 "elev_ft" = !!names(.[3]),
                                                 "most_recent" = !!names(.[4]),
                                                 "cfs" = !!names(.[5])  )
cdec_mainrttable <- cdec_mainrttable %>% na.omit()                         
cdec_mainrttable <- cdec_mainrttable[!grepl("Station Name", cdec_mainrttable$cdec_name),] #remove any row with Station Name
#as_tibble(cdec_mainrttable)
cdec_mainrttable$elev_ft <- gsub("'", "", cdec_mainrttable$elev_ft)
cdec_mainrttable$cfs <- gsub(",", "", cdec_mainrttable$cfs)
#as_tibble(cdec_mainrttable)
cdec_mainrttable$cfs <- gsub("CFS", "", cdec_mainrttable$cfs)
#as_tibble(cdec_mainrttable)
cdec_mainrttable <- cdec_mainrttable %>% mutate(elev_ft = as.double(elev_ft), cfs = as.double(cfs),
                                              most_recent = mdy_hm(most_recent))
as_tibble(cdec_mainrttable)

dwrwebliststations <- data.frame(cdec_mainrttable$cdec_code) %>% rename("cdec_code" = !!names(.[1]))
dwrwebliststations <- dwrwebliststations %>% mutate(url = paste0("http://cdec.water.ca.gov/dynamicapp/staMeta?station_id=", cdec_code) )

cdec <- data.frame(    cdec_code = character(),
                       basin = character(),
                       group = numeric(), 
                       lat = character(),
                       lon = character(), 
                       nearby_cty  = character(),
                       gage_oper = character(),
                       data_collect = character(),
                       cdec_code = character(),
                       source = character(),
                       stringsAsFactors = FALSE) 

cdec_i <- data.frame(  cdec_code = character(),
                       basin = character(),
                       group = numeric(), 
                       lat = character(),
                       lon = character(), 
                       nearby_cty  = character(),
                       gage_oper = character(),
                       data_collect = character(),
                       cdec_code = character(),
                       source = character(),
                       stringsAsFactors = FALSE) 
#dwrwebliststations <- dwrwebliststations %>% filter(cdec_code != "ALN") %>% 
#                                             filter(cdec_code != "BAS") %>%   #remove when ALN page is back up
#                                             filter(cdec_code != "BAT") %>%
#                                             filter(cdec_code != "BCK") %>%
#                                             filter(cdec_code != "BCD") %>%
#                                             filter(cdec_code != "BCC") %>%
#                                             filter(cdec_code != "BDF") %>%
#                                             filter(cdec_code != "BDV") %>%
#                                             filter(cdec_code != "BDT") %>%
#                                             filter(cdec_code != "AMW") %>%
#                                             filter(cdec_code != "MBM") %>%
#                                             filter(cdec_code != "MBH") %>%
#                                             filter(cdec_code != "AMK") 
}

for (i in 1:length(dwrwebliststations$cdec_code)) { 
  
  x1 <- dwrwebliststations$url[i]
  x1 <- readHTMLTable(x1)
  x1 <- x1[1]
  x1 <- data.frame(x1)
  
  x1_a <- x1[,c(1,2)]
  x1_b <- x1[,c(3,4)]
  
  x1_a <- transpose(x1_a)
  colnames(x1_a) <- x1_a[1, ] # the first row will be the header
  x1_a <- x1_a[-1, ]          # removing the first row.
  
  x1_b <- transpose(x1_b)
  colnames(x1_b) <- x1_b[1, ] # the first row will be the header
  x1_b <- x1_b[-1, ]          # removing the first row.
  
  cdec_i <- cbind(x1_a, x1_b) 
  cdec_i <- cdec_i %>% mutate(cdec_code = dwrwebliststations$cdec_code[i], source = "cdec_web")
  
  cdec <-rbind(cdec, cdec_i) 
  message(dwrwebliststations$cdec_code[i])
  #done
}
#as_tibble(cdec)

cdec <- cdec %>% transmute(cdec_code, basin = `River Basin`, group  = `Hydrologic Area`, 
                 lat = as.double(substr(Latitude,1,nchar(Latitude)-2)), #removes last two special digits (ie degree symbol, another)
                 lon = as.double(substr(Longitude,1,nchar(Longitude)-2)), #removes last two special digits (ie degree symbol, another)
                 nearby_cty = `Nearby City`, gage_oper = Operator,
                 data_collect = `Data Collection`, county = County)


as_tibble(cdec)

rm(x1, x1_a, x1_b, cdec_i, dwrwebliststations)

#### join coordinates to flow data #####

cdec <- inner_join(cdec, cdec_mainrttable, by = "cdec_code")
as_tibble(cdec)


cdec <- cdec %>% mutate(basin = tolower(basin), basin  = sapply(basin, CapStr),
                       nearby_cty = tolower(nearby_cty),  nearby_cty  = sapply(nearby_cty, CapStr),
                       group = tolower(group),  group = sapply(group, CapStr),
                       cdec_name = tolower(cdec_name),  cdec_name = sapply(cdec_name, CapStr),
                       data_collect = tolower(data_collect), data_collect = sapply(data_collect, CapStr),
                       county = tolower(county),  county = sapply(county, CapStr)) 

cdec$gage_oper <- gsub("US Geological Survey","USGS", cdec$gage_oper)
cdec$gage_oper <- gsub("CA Dept of Water Resources","DWR", cdec$gage_oper)
cdec$gage_oper <- gsub("Pacific Gas & Electric","PG&E", cdec$gage_oper)
cdec$gage_oper <- gsub("US Army Corps of Engineers","USACE", cdec$gage_oper)
cdec$gage_oper <- gsub("US Bureau of Reclamation","USBR", cdec$gage_oper)
cdec$gage_oper <- gsub("Sacramento Municipal Utility District","SMUD", cdec$gage_oper)
cdec$gage_oper <- gsub("Office","", cdec$gage_oper)
cdec$gage_oper <- gsub("Placer County Water Agency","PCWA", cdec$gage_oper)
cdec$gage_oper <- gsub("and","&", cdec$gage_oper)

as_tibble(cdec)
cdec_csv <- cdec %>% select(-most_recent, -cfs)
as_tibble(cdec_csv)
#write_csv(cdec_csv, "cdec_meta.csv")
as_tibble(cdec_csv)

cdec <- cdec %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "identity")
as_tibble(cdec)



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
#             popup  = popupTable(cdec, zcol = c("cdec_code", "basin", "cfs",  "most_recent", "cdec_name", "elev_ft",
#                                                 "nearby_cty", "county", "gage_oper")),
#             layer.name = "cfs") 
#
#
#m <-  addStaticLabels(m, data = cdec, label = cdec$cfs, labelOptions = lopt)
#
#m
#m@map
#rm(cdec_mainrttable)







































