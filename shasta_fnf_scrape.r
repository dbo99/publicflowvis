

{
{
#rm(list = ls()) 
  rm(list= ls()[!(ls() %in% c('friant_fnf','newmelones_fnf'))])
  #setwd("~/R/proj/publicflowscrapeapp")
setwd("~/Documents/publicflowvis")
source("libs.r")

file <- "https://www.usbr.gov/mp/cvo/vungvari/shafln.pdf"
rawtext  <- pdf_text(file)
rawtext
month_year  <- str_match(rawtext, "CALIFORNIA\r\n(.*?)FULL")  #https://stackoverflow.com/questions/39086400/extracting-a-string-between-other-two-strings-in-r
if (anyNA(month_year)) {month_year  <- str_match(rawtext, "CALIFORNIA\n(.*?)FULL") }
month_year <- month_year[,2] %>% trimws() %>% as.yearmon() 
month <- month(month_year)
year <- year(month_year)
start <- "\n    1"
end <- "\n  TOTALS"
shasta_fnf <- read.table(text=substring(rawtext, regexpr(start, rawtext), regexpr(end, rawtext)))

rm(start, end, month_year, rawtext)
as_tibble(shasta_fnf)
nrow_shasta_fnf <- nrow(shasta_fnf)

shasta_fnf_mostrecent <- shasta_fnf[nrow_shasta_fnf,]  #most recent row from usbr
shasta_fnf_nextmostrecent <- shasta_fnf[nrow_shasta_fnf - 1,]  #next most recent row from usbr
as_tibble(shasta_fnf_mostrecent)
as_tibble(shasta_fnf_nextmostrecent)
rm(shasta_fnf, nrow_shasta_fnf )

}

{
  ## rename most recent ## 
  shasta_fnf_mostrecent  <- shasta_fnf_mostrecent %>% rename("monthday" = !!names(.[1]),
                                                             "Britton" = !!names(.[2]),
                                                             "McCloudDivRes" = !!names(.[3]),
                                                             "IronCanyon" = !!names(.[4]),
                                                             "Pit6" = !!names(.[5]),
                                                             "Pit7" = !!names(.[6]) ,
                                                             "Shasta_upstream_stor" = !!names(.[7]),
                                                             "Shasta_upstream_dlystorchange" = !!names(.[8]),
                                                             "Shasta_observedinflowchange_meandaily" = !!names(.[9]),
                                                             "Shasta_observedinflow_meandaily" = !!names(.[10]),
                                                             "Shasta_natriver_fnf" = !!names(.[11]),
                                                             "Shasta_natriver_WYaccum" = !!names(.[12]))

as_tibble(shasta_fnf_mostrecent)
}
date_mostrecent<- as.integer(shasta_fnf_mostrecent$monthday)
date_nextmostrecent <- as.integer(date_mostrecent - 1)
date_mostrecent <- paste0(month,"/", date_mostrecent, "/", year) %>% mdy()
date_nextmostrecent <- paste0(month,"/", date_nextmostrecent, "/", year) %>% mdy()

shasta_fnf_mostrecent <- shasta_fnf_mostrecent %>% select(-monthday) 

## rename next most recent ## 
{
  shasta_fnf_nextmostrecent  <- shasta_fnf_nextmostrecent %>% rename("monthday" = !!names(.[1]),
                                                                     "Britton" = !!names(.[2]),
                                                                     "McCloudDivRes" = !!names(.[3]),
                                                                     "IronCanyon" = !!names(.[4]),
                                                                     "Pit6" = !!names(.[5]),
                                                                     "Pit7" = !!names(.[6]) ,
                                                                     "Shasta_upstream_stor" = !!names(.[7]),
                                                                     "Shasta_upstream_dlystorchange" = !!names(.[8]),
                                                                     "Shasta_observedinflowchange_meandaily" = !!names(.[9]),
                                                                     "Shasta_observedinflow_meandaily" = !!names(.[10]),
                                                                     "Shasta_natriver_fnf" = !!names(.[11]),
                                                                     "Shasta_natriver_WYaccum" = !!names(.[12])) %>% select(-monthday) 
}


as_tibble(shasta_fnf_nextmostrecent)
as_tibble(shasta_fnf_mostrecent)
###### convert mostrecent's to long format, add attributes######
#{
  shasta_fnf_mostrecent_t <- shasta_fnf_mostrecent %>% gather(key = "res", value = "value")
  nws_id <- c("BTOC1", "MCZC1", "ICYC1", "PTXC1", "PTZC1", "SHDC1", "SHDC1", "SHDC1", "SHDC1", 
              "SHDC1", "SHDC1")

  usbr_web_param <- c("storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant", 
                      "storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant", "upstrstorage_usbrcvo_latest_instant",
                      "stor_usbrcvo_dlychnge_todelete", "meancfs_usbrcvo_dlychnge_todelete", 
                      "inflow_usbrcvo_latest_meandly",
                      "fullnaturalflow_usbrcvo_latest_meandly", "fullnaturalflow_usbrcvo_latest_wyaccum")
  
  unit <-  c("af", "af", "af", "af", "af", "af", "af", "af","cfs", "cfs", "taf")
  as_tibble(shasta_fnf_mostrecent_t)
  shasta_fnf_mostrecent <- cbind(shasta_fnf_mostrecent_t, nws_id, usbr_web_param, unit)
  rm(shasta_fnf_mostrecent_t, unit, usbr_web_param, nws_id)
  as_tibble(shasta_fnf_mostrecent)
  
  shasta_fnf_mostrecent$value <- gsub(",", "", shasta_fnf_mostrecent$value )
 # shasta_fnf_mostrecent$value <- gsub("+", "", shasta_fnf_mostrecent$value ) #doesn't work, but below's as.numeric() seems to
  as_tibble(shasta_fnf_mostrecent)
  shasta_fnf_mostrecent <- shasta_fnf_mostrecent %>% mutate(value = as.numeric(value)) 
  as_tibble(shasta_fnf_mostrecent)
#}

{
  shasta_fnf_mostrecent <- shasta_fnf_mostrecent %>% 
    filter(usbr_web_param != "stor_usbrcvo_dlychnge_todelete") %>% filter(usbr_web_param != "meancfs_usbrcvo_dlychnge_todelete")
  as_tibble(shasta_fnf_mostrecent)
}
###### convert mostrecent's to long format, add attributes######

{
  shasta_fnf_nextmostrecent_t <- shasta_fnf_nextmostrecent %>% gather(key = "res", value = "value")
  nws_id <- c("BTOC1", "MCZC1", "ICYC1", "PTXC1", "PTZC1", "SHDC1", "SHDC1", "SHDC1", "SHDC1", 
              "SHDC1", "SHDC1")
  usbr_web_param <- c("storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant", 
                      "storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant", "upstrstorage_usbrcvo_nextlatest_instant",
                      "stor_usbrcvo_dlychnge_todelete", "meancfs_usbrcvo_dlychnge_todelete", 
                      "inflow_usbrcvo_nextlatest_meandly",
                      "fullnaturalflow_usbrcvo_nextlatest_meandly", "fullnaturalflow_usbrcvo_nextlatest_wyaccum")
  
  unit <-  c("af", "af", "af", "af", "af", "af", "af", "af","cfs", "cfs", "taf")
  shasta_fnf_nextmostrecent <- cbind(shasta_fnf_nextmostrecent_t, nws_id, usbr_web_param, unit)
  rm(shasta_fnf_nextmostrecent_t, unit, usbr_web_param, nws_id)
  as_tibble(shasta_fnf_nextmostrecent)
  
  shasta_fnf_nextmostrecent$value <- gsub(",", "", shasta_fnf_nextmostrecent$value )
  shasta_fnf_nextmostrecent$value <- gsub("+", "", shasta_fnf_nextmostrecent$value ) #doesn't work, but below's as.numeric() seems to
  as_tibble(shasta_fnf_nextmostrecent)
  shasta_fnf_nextmostrecent <- shasta_fnf_nextmostrecent %>% mutate(value = as.numeric(value)) 
  as_tibble(shasta_fnf_nextmostrecent)
  shasta_fnf_nextmostrecent <- shasta_fnf_nextmostrecent %>%
    filter(usbr_web_param != "stor_usbrcvo_dlychnge_todelete") %>% filter(usbr_web_param != "meancfs_usbrcvo_dlychnge_todelete")
  as_tibble(shasta_fnf_nextmostrecent)
}
as_tibble(shasta_fnf_mostrecent)
as_tibble(shasta_fnf_nextmostrecent)


{
  shasta_fnf_dlychnge <- inner_join(shasta_fnf_mostrecent, shasta_fnf_nextmostrecent, by = "res") 
  as_tibble(shasta_fnf_dlychnge)
  shasta_fnf_dlychnge <- shasta_fnf_dlychnge %>% mutate(value = value.x - value.y)
  as_tibble(shasta_fnf_dlychnge)
  shasta_fnf_dlychnge <- shasta_fnf_dlychnge %>% transmute(res, value, nws_id = nws_id.x, usbr_web_param = usbr_web_param.y, unit = unit.x)
  shasta_fnf_dlychnge <- shasta_fnf_dlychnge %>% mutate(date = date_mostrecent)
  shasta_fnf_dlychnge$usbr_web_param <- gsub("nextlatest", "dailychange", shasta_fnf_dlychnge$usbr_web_param ) 
  
  
  as_tibble(shasta_fnf_dlychnge)
  
  ## add date column
  
  shasta_fnf_mostrecent <- shasta_fnf_mostrecent %>% mutate(date = date_mostrecent)
  shasta_fnf_nextmostrecent <- shasta_fnf_nextmostrecent %>% mutate(date = date_nextmostrecent)
  shasta_fnf_dlychnge <- shasta_fnf_dlychnge %>% mutate(date = date_mostrecent)
  
  shasta_fnf  <- rbind(shasta_fnf_mostrecent, shasta_fnf_nextmostrecent, shasta_fnf_dlychnge)
  #shasta_fnf_dlychnge$res <- gsub("Shasta_stor", "Shasta_stor", shasta_fnf_dlychnge$res )
  rm(shasta_fnf_mostrecent, shasta_fnf_nextmostrecent, shasta_fnf_dlychnge)
  as_tibble(shasta_fnf)
  
  shasta_fnf <- shasta_fnf %>% mutate(value = round(value, 0))
  
}

shasta_fnf$res<- gsub("Shasta_upstream_stor", "Shasta", shasta_fnf$res ) 
shasta_fnf$res<- gsub("Shasta_observedinflow_meandaily", "Shasta", shasta_fnf$res ) 
shasta_fnf$res<- gsub("Shasta_natriver_fnf", "Shasta", shasta_fnf$res ) 
shasta_fnf$res<- gsub("Shasta_natriver_WYaccum", "Shasta", shasta_fnf$res ) 
shasta_fnf$res<- gsub("McCloudDivRes", "McCloud", shasta_fnf$res ) 
#shasta_fnf$res<- gsub("IronCanyon", "IronCanyon", shasta_fnf$res ) 


shasta_fnf <- shasta_fnf %>%  separate(usbr_web_param, into = c("param_source_timeseq", "meastype"), sep="_(?=[^_]+$)") 
#https://stackoverflow.com/questions/50518137/separate-a-column-into-2-columns-at-the-last-underscore-in-r
shasta_fnf <- shasta_fnf %>%  separate(param_source_timeseq, into = c("param_source", "timeseq"), sep="_(?=[^_]+$)")
shasta_fnf <- shasta_fnf %>%  separate(param_source, into = c("param", "source"), sep="_(?=[^_]+$)")

as_tibble(shasta_fnf)

## simply 3 units to 2
shasta_fnf <- shasta_fnf %>% mutate(value = ifelse(unit == "taf", value * 1000 ,value)) %>% mutate(unit = ifelse(unit == "taf", "af", as.character(unit)))

as_tibble(shasta_fnf)


## add rows for res capacities
{
  ## capacities (from various sources online, including wiki) ##
  res <- c("Britton", "McCloud", "IronCanyon", "Pit6", "Pit7", "Shasta") #Shasta's storage itself not published on fnf site
  numres <- length(res)
  value <- c(34600, 35200, 24300, 15700, 34100)  #af
  combined <- sum(value)
  value <- c(value, combined)
  nws_id <- c("BTOC1", "MCZC1", "ICYC1", "PTXC1", "PTZC1", "SHDC1")
  param <- c(rep("totalcapacity", numres - 1), "upstrtotalcapacity")
  source <- rep(NA, numres)
  timeseq <- rep(NA, numres)
  meastype <- rep("instant", numres )
  unit <- rep("af", numres)
  date <- rep(NA, numres)
  res_cap <- data.frame(cbind(res, value, nws_id, param, source, timeseq, meastype, unit, date)) #%>% mutate(capacity = as.double(capacity))
}

shasta_fnf <- rbind(shasta_fnf, res_cap) %>% mutate(value = as.character(value), value = as.double(value))
rm(res_cap)


## new df to join to main df for percent capacity calc
{
  ## capacities (from various sources online, including wiki) ##
  res <- c("Britton", "McCloud", "IronCanyon", "Pit6", "Pit7") #Shasta's storage itself not published on fnf site
  #numres <- length(res)
  capacity <- c(34600, 35200, 24300, 15700, 34100)  #note named capacity, not value - removed below
  nws_id <- c("BTOC1", "MCZC1", "ICYC1", "PTXC1", "PTZC1")
  res_cap <- data.frame(cbind(res, capacity, nws_id)) #%>% mutate(capacity = as.double(capacity))
}



## latest percent cap
shasta_fnf_percap_latest <- shasta_fnf %>% filter(param == "storage") %>% filter( timeseq == "latest") 
shasta_fnf_percap_latest <- right_join(shasta_fnf_percap_latest, res_cap) %>% mutate(capacity = as.character(capacity), 
                                                                                     capacity = as.double(capacity))
shasta_fnf_percap_latest <- shasta_fnf_percap_latest %>% mutate(value = value/capacity * 100) %>%
                            mutate(value = round(value, 1), unit = "percentfull") %>% select(-capacity)

shasta_fnf_percap_latest_upstr <- shasta_fnf %>% filter(param == "upstrstorage") %>% filter( timeseq == "latest") %>% 
                                  mutate(capacity = sum(capacity), value = value/capacity * 100) %>%
                                   mutate(value = round(value, 1), unit = "percentfull") %>% select(-capacity)

shasta_fnf_percap_latest_upstr_val <- shasta_fnf_percap_latest_upstr$value

## next latest percent cap
shasta_fnf_percap_nextlatest <- shasta_fnf %>% filter(param == "storage") %>% filter( timeseq == "nextlatest") 
shasta_fnf_percap_nextlatest <- right_join(shasta_fnf_percap_nextlatest, res_cap) %>% mutate(capacity = as.character(capacity), 
                                                                                     capacity = as.double(capacity))
shasta_fnf_percap_nextlatest <- shasta_fnf_percap_nextlatest %>% mutate(value = value/capacity * 100) %>%
                            mutate(value = round(value, 1), unit = "percentfull") %>% select(-capacity)

shasta_fnf_percap_nextlatest_upstr <- shasta_fnf %>% filter(param == "upstrstorage") %>% filter( timeseq == "nextlatest") %>% 
  mutate(capacity = sum(capacity), value = value/capacity * 100) %>%
  mutate(value = round(value, 1), unit = "percentfull") %>% select(-capacity)

shasta_fnf_percap_nextlatest_upstr_val <- shasta_fnf_percap_nextlatest_upstr$value

shasta_fnf_percap_nextlatest_upstr_valchange <- shasta_fnf_percap_latest_upstr_val - shasta_fnf_percap_nextlatest_upstr_val
shasta_fnf_percap_nextlatest_upstr_valchange <- shasta_fnf_percap_nextlatest_upstr %>%
                                              mutate(value = shasta_fnf_percap_nextlatest_upstr_valchange, timeseq = "dailychange")
## rbind
shasta_fnf<- rbind(shasta_fnf, shasta_fnf_percap_latest, shasta_fnf_percap_nextlatest, shasta_fnf_percap_latest_upstr, shasta_fnf_percap_nextlatest_upstr,
                   shasta_fnf_percap_nextlatest_upstr_valchange)

## create percent empty and inactive capacity remaining

shasta_fnf_percentempty <- shasta_fnf %>% filter(unit == "percentfull", timeseq != "dailychange") %>% mutate(value = 100 - value, unit = "percentempty")
shasta_fnf <- rbind(shasta_fnf, shasta_fnf_percentempty)


## create volume remaining 
shasta_fnf_totcap <- shasta_fnf %>% filter(param == "totalcapacity" | param == "upstrtotalcapacity") %>% select(value, nws_id, param) 

shasta_fnf_stor_latest <- shasta_fnf %>% filter(param == "storage" | param == "upstrstorage", timeseq == "latest", unit == "af")
shasta_remainingcap_latest <- left_join(shasta_fnf_stor_latest, shasta_fnf_totcap, by = "nws_id" , "param") %>% 
                   mutate(value = value.y - value.x, param = "remainingcapacity") %>% select(-value.x, -value.y, -param.x, -param.y)


shasta_fnf_stor_nextlatest <- shasta_fnf %>% filter(param == "storage" | param == "upstrstorage", timeseq == "nextlatest", unit == "af")
shasta_remainingcap_nextlatest <- left_join(shasta_fnf_stor_nextlatest, shasta_fnf_totcap, by = "nws_id" , "param") %>% 
  mutate(value = value.y - value.x, param = "remainingcapacity") %>% select(-value.x, -value.y, -param.x, -param.y)
                                                                                                         
shasta_fnf <- rbind(shasta_fnf, shasta_remainingcap_latest, shasta_remainingcap_nextlatest )

rm(list= ls()[!(ls() %in% c('friant_fnf','newmelones_fnf', 'shasta_fnf'))])
as_tibble(shasta_fnf)
}
