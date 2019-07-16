#setwd("~/R/proj/nohrsc/shiny/final/firstpublish")
#Sys.setenv(TZ="America/Los_Angeles")
#setwd("~/R/proj/nohrsc/shiny")
#setwd("~/Documents/shiny_nohrsc/final")
#setwd("~/R/proj/nohrsc/shiny/final")

#source("agency_rt_assim_way_june5a.r")
source("res_control_2.r")
#source("cdec_res_table.r")

#rfc_fcast_pnt <- cdec_usgs_cnrfc %>% filter(rfc_pnt_type == "River Forecast")

ui <- dashboardPage(
  dashboardHeader(title = "moneybags doc"),
  dashboardSidebar(  
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."
    ),
    sliderInput("cex", "Flow Marker Size (cfs)",
                min = 0, max = 50, value = 3, step = 0.1
    ),
    
    sliderInput("cex", "Res Marker Size (various)",
                min = 0, max = 50, value = 3, step = 0.1
    ),
    selectizeInput(
      #sliderInput(
      "res_param", "Res Parameter", choices = colnames(df_w), 
      selected = c("storage_dailychange_af_instant"), 
      multiple = FALSE)
  ),
  
  
  
  
  dashboardBody(
    
    tags$style(type = "text/css", "#mapplot {height: calc(100vh - 80px) !important;}"),
    leafletOutput("mapplot"),
    useShinyjs()
  )
)




########################
#### server.r
########################

date <- Sys.Date()

server <- function(input, output) {
  addClass(selector = "body", class = "sidebar-collapse")
  options(shiny.maxRequestSize=225*1024^2) 
  
 # selectedinput <- paste0(input$res_param)
  
#  df_w <- reactive ({
#    df_w() %>% mutate(selectedinput = paste0(unique(input$selectedinput)))
#  })
#  
  
  output$mapplot <- renderLeaflet({
    
    
    maptypes = c(
      "Stamen.TopOSMRelief", 
      "Stamen.TerrainBackground",
      "NASAGIBS.ModisTerraTrueColorCR",
      #"ESRI.WorldImagery",
      "Esri.WorldPhysical",  
      "OpenTopoMap" )
    
    
    
    grp <- c(    "usgs hydrography",   "0.5 reflectivity") #,"hrrr p_1hr", "hrrr p_2hr",   "hrrr p_4hr", "hrrr p_6hr",
    #"mrms p_1hr", "mrms p_24hr", "mrms p_48hr", "mrms p_72hr") # "Coarse Geo") # 
    
    
    lopt <- labelOptions(noHide = TRUE,
                         direction = 'top',
                         textOnly = TRUE)
    pal = mapviewPalette("mapviewSpectralColors")   
    
  #  m <- mapview(cdec_usgs_cnrfc["cfs_most_recent"], #burst = TRUE, hide = TRUE, 
  #               col.regions = viridisLite::viridis, 
  #               cex = cdec_usgs_cnrfc$group_to_visualize,
  #               alpha.regions = 0.3,
  #               map.types = maptypes,
  #               popup = popupTable(cdec_usgs_cnrfc, zcol = c("cfs_most_recent",
  #                                                          "name_usgs",
  #                                                          "name_cdec",
  #                                                          "name_cnrfc",
  #                                                          "cfs_usgs",   
  #                                                          "most_recent_usgs",
  #                                                          "cfs_cdec",   
  #                                                          "most_recent_cdec",
  #                                                          "hist_pkflow_usgs",
  #                                                          "gelev_ft_cdec",
  #                                                          "gelev_ft_cnrfc",
  #                                                          "stage_ft_usgs",
  #                                                          "nwsfloodstg_ft_usgs",
  #                                                          "gage_oper_cdec",
  #                                                          "group_cdec" ,
  #                                                          "basin_cdec" ,   
  #                                                          "county_cdec",
  #                                                          "nearby_cty_cdec",
  #                                                          
  #                                                          "data_collect_cdec",
  #                                                          "gtype_nws",
  #                                                          "id_cdec",
  #                                                          "id_usgs",
  #                                                          "id_nws",
  #                                                          "lat_cdec",
  #                                                          "lon_cdec",
  #                                                          "lat_usgs",
  #                                                          "lon_usgs",
  #                                                          "lat_cnrfc",
  #                                                          "lon_cnrfc",
  #                                                          
  #                                                          "offname_cnrfc",
  #                                                          "rfc_pnt_type",
  #                                                          "state_usgs")),
  #               
  #               layer.name = "cfs, proportional symbols") +
  #    
  #    mapview(rfc_fcast_pnt["cfs_most_recent"], 
  #            col.regions = viridisLite::viridis, 
  #            cex = rfc_fcast_pnt$group_to_visualize,
  #            color = "red",
  #            alpha.regions = 0.0,
  #            map.types = maptypes,
  #            legend = FALSE,
  #            popup = popupTable(rfc_fcast_pnt, zcol = c("cfs_most_recent",
  #                                                       "name_usgs",
  #                                                       "name_cdec",
  #                                                       "name_cnrfc",
  #                                                       "cfs_usgs",   
  #                                                       "most_recent_usgs",
  #                                                       "cfs_cdec",   
  #                                                       "most_recent_cdec",
  #                                                       "hist_pkflow_usgs",
  #                                                       "gelev_ft_cdec",
  #                                                       "gelev_ft_cnrfc",
  #                                                       "stage_ft_usgs",
  #                                                       "nwsfloodstg_ft_usgs",
  #                                                       "gage_oper_cdec",
  #                                                       "group_cdec" ,
  #                                                       "basin_cdec" ,   
  #                                                       "county_cdec",
  #                                                       "nearby_cty_cdec",
  #                                                       
  #                                                       "data_collect_cdec",
  #                                                       "gtype_nws",
  #                                                       "id_cdec",
  #                                                       "id_usgs",
  #                                                       "id_nws",
  #                                                       "lat_cdec",
  #                                                       "lon_cdec",
  #                                                       "lat_usgs",
  #                                                       "lon_usgs",
  #                                                       "lat_cnrfc",
  #                                                       "lon_cnrfc",
  #                                                       
  #                                                       "offname_cnrfc",
  #                                                       "rfc_pnt_type",
  #                                                       "state_usgs")),
  #            layer.name = "official rfc forecast point (red)")               +
  #    
   
    #m <- mapview(df_w["inflow_latest_af_meandly_cdec_2019-06-24"], #burst = TRUE, hide = TRUE, 
 m <-    mapview(df_w["storage_latest_percentempty_instant_cdec"], #burst = TRUE, hide = TRUE, 
                 #col.regions = RColorBrewer::RdBu, 
                 col.regions = pal(100),
                 #at = seq(min_rt, max_rt+2500, 2500),
                 cex = "storage_latest_percentempty_instant_cdec",  #29
                 #cex = "storage_dailychange_af_instant_usbr_2019-06-23",  #14          
                 alpha.regions = 0.3,
                 map.types = maptypes,
                 legend = TRUE,
                 popup = popupTable(df_w),
                 #popup = popupTable(gage_points, zcol = c("STATION_NM", "usgs_id", "cfs", "stage_ft", "hist_pkflow",
                 #                                         "usgs_nwsfloodstg_ft", "dt")),
                 #lwd = nhd_fyh_sf$Q0001A/1000,
                 layer.name =  "ca_reservoirs") 
      
    m@map = m@map %>% 
      
      addTiles() %>%
      #leaflet(height = "100%") %>%
      setView(-119.6, 38.05, zoom = 5)  %>%   
      
      addWMSTiles(group= grp[1], baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE), attribution = "USGS") %>% 
      
      addWMSTiles( group = grp[2],baseUrl = 
                     "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi", 
                   #"https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
                   layers = "nexrad-n0r-900913",
                   options = WMSTileOptions(format = "image/png", transparent = TRUE),
                   attribution = "Weather data  2012 IEM Nexrad")     %>%
      mapview:::mapViewLayersControl(names = grp) %>% hideGroup(grp[1]) %>% 
      hideGroup(grp[2]) 
    m@map
  #  m@map <- m@map %>% addStaticLabels(m@map, data = cdec_usgs_cnrfc, label = cdec_usgs_cnrfc$cfs_most_recent, labelOptions = lopt) %>% 
  #    setView(-119.6, 38.05, zoom = 6.5) 
  #  m@map
    
    
    
    #mapview:::mapViewLayersControl(names = grp) 
    #m@map
    #m
    
    
    observeEvent(input$button, {
      
      saveWidget(m@map, file= paste0("publicgauges_", date, ".html"), selfcontained = TRUE)
      #mapshot(m@map, url = "mydog.html")
    })
    
    m@map     
    
  }
  )}
shinyApp(ui, server)