#{
## read in hydronetwork
#nhd_fyh <- readOGR(".","nhd_feathyuba")
#nhd_fyh_sf <- nhd_fyh %>%  st_as_sf()
#latlonwgs84CRS <- st_crs(nhd_fyh_sf)
### read in cnrfc basin zones
#fyhz <- readOGR(".","feathyuba_zones")
#fyhz_sf <- fyhz %>%  st_as_sf()

{
  
  
  
  maptypes = c(#"Stamen.TonerLite", 
    #"Stamen.Terrain", 
    "Stamen.TopOSMRelief", 
    #"Esri.WorldTopoMap" , 
    "Esri.WorldPhysical",  
    "OpenTopoMap" )# ,
  #"NASAGIBS.ModisTerraSnowCover", 
  #"NASAGIBS.ModisTerraTrueColorCR", 
  #"NASAGIBS.ModisTerraBands367CR")
  
  #grp <- c(    "usgs hydrography",   "0.5 reflectivity","hrrr p_1hr", "hrrr p_2hr",   "hrrr p_4hr", "hrrr p_6hr",
  #             "mrms p_1hr", "mrms p_24hr", "mrms p_48hr", "mrms p_72hr") # "Coarse Geo") # 
  
  grp <- c(    "usgs hydrography",   "0.5 reflectivity") #,"hrrr p_1hr", "hrrr p_2hr",   "hrrr p_4hr", "hrrr p_6hr",
  #"mrms p_1hr", "mrms p_24hr", "mrms p_48hr", "mrms p_72hr") # "Coarse Geo") # 
  
  
  lopt <- labelOptions(noHide = TRUE,
                       direction = 'top',
                       textOnly = TRUE)
  
  
  m <- mapview(cdec_usgs_cnrfc["cfs_most_recent"], #burst = TRUE, hide = TRUE, 
               col.regions = viridisLite::viridis, 
               #at = seq(0, cfs_max, cfs_interval),
               cex = cdec_usgs_cnrfc$group_to_visualize,
               alpha.regions = 0.3,
               map.types = maptypes,
               legend = TRUE,
               #popup = popupTable(cdec_usgs_cnrfc),
               popup = popupTable(rfc_fcast_pnt, zcol = c("cfs_most_recent",
                                                          "name_usgs",
                                                          "name_cdec",
                                                          "name_cnrfc",
                                                          "cfs_usgs",   
                                                          "most_recent_usgs",
                                                          "cfs_cdec",   
                                                          "most_recent_cdec",
                                                          "hist_pkflow_usgs",
                                                          "gelev_ft_cdec",
                                                          "gelev_ft_cnrfc",
                                                          "stage_ft_usgs",
                                                          "nwsfloodstg_ft_usgs",
                                                          "gage_oper_cdec",
                                                          "group_cdec" ,
                                                          "basin_cdec" ,   
                                                          "county_cdec",
                                                          "nearby_cty_cdec",
                                                          
                                                          "data_collect_cdec",
                                                          "gtype_nws",
                                                          "id_cdec",
                                                          "id_usgs",
                                                          "id_nws",
                                                          "lat_cdec",
                                                          "lon_cdec",
                                                          "lat_usgs",
                                                          "lon_usgs",
                                                          "lat_cnrfc",
                                                          "lon_cnrfc",
                                                          
                                                          "offname_cnrfc",
                                                          "rfc_pnt_type",
                                                          "state_usgs")),
               
               
               layer.name = "cfs")  
  
  
  m@map = m@map %>% 
    
    addTiles() %>%
    setView(-119.6, 38.05, zoom = 5)  %>%   
    
    addWMSTiles(group= grp[1], baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", layers = "0",
                options = WMSTileOptions(format = "image/png", transparent = TRUE), attribution = "USGS") %>% 
    
    addWMSTiles( group = grp[2],baseUrl = 
                   "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi", 
                 layers = "nexrad-n0r-900913",
                 options = WMSTileOptions(format = "image/png", transparent = TRUE),
                 attribution = "Weather data  2012 IEM Nexrad")     %>%
    mapview:::mapViewLayersControl(names = grp) %>% #hideGroup(grp[1]) #%>% 
    hideGroup(grp[2])# %>%
  
}
#m <- m %>% addStaticLabels(m, data = cdec_usgs_cnrfc, label = cdec_usgs_cnrfc$cfs_most_recent, labelOptions = lopt)
m
saveWidget(m@map, "777.html", selfcontained = TRUE)

