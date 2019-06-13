rm(list = ls()) 
source("libs.r")
source("fun_defs.r")

{
c1 <- "https://waterwatch.usgs.gov/index.php?id=real&sid=w__table_flow&r=ca"
c1  <- read_html(c1,"#fr :nth-child(1)")
c1 <- html_table(c1, header = TRUE, trim = TRUE, fill = TRUE, dec = ".")
c1 <- c1[[5]]
#as_tibble(c1)
c1 <- c1 %>% transmute(id_usgs = as.double(USGSstationID), name_usgs = tolower(`USGS station name`), 
                       name_usgs = sapply(name_usgs, CapStr),
                       stage_ft_usgs = `Most recentstage(ft)`,
                       #kcfs = round(as.numeric(`Most recentflow(cfs)`)/1000,1 ), 
                       cfs = as.numeric(gsub(",", "", `Most recentflow(cfs)`)),
                       #as.numeric(`Most recentflow(cfs)`), 
                       most_recent_usgs = ymd_hms(`Most recentlocaldate/time`),
                       # hist_pkflow_usgs = as.numeric(`Historicalpeaks(cfs)`),
                       hist_pkflow_usgs = as.numeric(gsub(",", "", `Historicalpeaks(cfs)`)), 
                       nwsfloodstg_ft_usgs = `NWSfloodstage(ft)`,
                       nwsid = NWSstationID)
#as_tibble(c1)
}
## get nevada real time data
{
n1 <- "https://waterwatch.usgs.gov/index.php?id=real&sid=w__table_flow&r=nv"
n1  <- read_html(n1,"#fr :nth-child(1)")
n1 <- html_table(n1, header = TRUE, trim = TRUE, fill = TRUE, dec = ".")
n1 <- n1[[5]]
#as_tibble(n1)
n1 <- n1 %>% transmute(id_usgs = as.double(USGSstationID), name_usgs = tolower(`USGS station name`), 
                       name_usgs = sapply(name_usgs, CapStr),
                       stage_ft_usgs = `Most recentstage(ft)`,
                       cfs = as.numeric(gsub(",", "", `Most recentflow(cfs)`)), 
                       most_recent_usgs = ymd_hms(`Most recentlocaldate/time`),
                       # hist_pkflow_usgs = as.numeric(`Historicalpeaks(cfs)`),
                       hist_pkflow_usgs = as.numeric(gsub(",", "", `Historicalpeaks(cfs)`)), 
                       nwsfloodstg_ft_usgs = `NWSfloodstage(ft)`,
                       nwsid = NWSstationID)
#as_tibble(n1)
}
## get oregon real time data

{
o1 <- "https://waterwatch.usgs.gov/index.php?id=real&sid=w__table_flow&r=or"
o1  <- read_html(o1,"#fr :nth-child(1)")
o1 <- html_table(o1, header = TRUE, trim = TRUE, fill = TRUE, dec = ".")
o1 <- o1[[5]]
#as_tibble(o1)
o1 <- o1 %>% transmute(id_usgs = as.double(USGSstationID), name_usgs = tolower(`USGS station name`), 
                       name_usgs = sapply(name_usgs, CapStr),
                       stage_ft_usgs = `Most recentstage(ft)`,
                       cfs = as.numeric(gsub(",", "", `Most recentflow(cfs)`)), 
                       most_recent_usgs = ymd_hms(`Most recentlocaldate/time`),
                       # hist_pkflow_usgs = as.numeric(`Historicalpeaks(cfs)`),
                       hist_pkflow_usgs = as.numeric(gsub(",", "", `Historicalpeaks(cfs)`)), 
                       nwsfloodstg_ft_usgs = `NWSfloodstage(ft)`,
                       nwsid = NWSstationID) 
#as_tibble(o1)
}
usgs_rt <- rbind(c1,n1, o1)
rm(c1,n1,o1)
as_tibble(usgs_rt)

usgs_coords <- read_csv("usgs_may19_coords.csv") #from downloaded shp - https://waterwatch.usgs.gov/index.php?id=wwds_shp (arcmap conversion tool>table to excel, save as .csv)
as_tibble(usgs_coords)
usgs_coords <- usgs_coords %>% filter(ST == "ca" | ST == "or" | ST == "nv") %>% 
                               transmute(id_usgs = STAID, name = STANAME, state_usgs = ST, lat_usgs = POINT_Y,  lon_usgs = POINT_X)  %>% 
                               mutate(id_usgs = as.double(id_usgs))
                               #filter(lat <)
as_tibble(usgs_coords)                                                                                     

#usgs_coords_ca <- usgs_coords %>% filter(state == "ca")
#usgs_coords_nv <- usgs_coords %>% filter(state == "nv")
#usgs_coords_or <- usgs_coords %>% filter(state == "or")

as_tibble(usgs_rt)
as_tibble(usgs_coords)

usgs <- right_join(usgs_coords, usgs_rt,  by = "id_usgs") %>% select(-"name") %>% mutate(cfs_usgs = cfs) %>% select(-cfs)
usgs <- distinct(usgs)
usgs <- usgs %>% select(-nwsid)

as_tibble(usgs)
as_tibble(usgs_rt)
as_tibble(usgs_coords)
rm(usgs_coords, usgs_rt)

#cnrfc_gages <- read_csv("db33river_gage.csv") #%>% filter(off_name != "NA")
#cnrfc_gages <- st_as_sf(cnrfc_gages, coords = c("lon", "lat"), crs = latlonwgs84CRS)


