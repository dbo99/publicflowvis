


{
#rm(list = ls()) 
  rm(list= ls()[!(ls() %in% mainkeepers)])
#  rm(list=setdiff(ls(), "shasta_fnf", "newmelones_fnf", "newmelones_fnf"))
#setwd("~/R/proj/publicflowscrapeapp")
setwd("~/Documents/publicflowvis")
source("libs.r")

file <- "https://www.usbr.gov/mp/cvo/vungvari/nmlfln.pdf"
rawtext  <- pdf_text(file)
#rawtext
month_year  <- str_match(rawtext, "CALIFORNIA\r\n(.*?)FULL")  #https://stackoverflow.com/questions/39086400/extracting-a-string-between-other-two-strings-in-r
if (anyNA(month_year)) {month_year  <- str_match(rawtext, "CALIFORNIA\n(.*?)FULL") }
month_year <- month_year[,2] %>% trimws() %>% as.yearmon() 
month <- month(month_year)
year <- year(month_year)
start <- "\n    1"
end <- "\n TOTALS"  #beware - newmelones and Shasta have more white space in between `n` & `TOTALS`
newmelones_fnf <- read.table(text=substring(rawtext, regexpr(start, rawtext), regexpr(end, rawtext)))

rm(start, end, month_year, rawtext)
as_tibble(newmelones_fnf)
nrow_newmelones_fnf <- nrow(newmelones_fnf)

newmelones_fnf_mostrecent <- newmelones_fnf[nrow_newmelones_fnf,]  #most recent row from usbr
newmelones_fnf_nextmostrecent <- newmelones_fnf[nrow_newmelones_fnf - 1,]  #next most recent row from usbr
as_tibble(newmelones_fnf_mostrecent)
as_tibble(newmelones_fnf_nextmostrecent)
rm(newmelones_fnf, nrow_newmelones_fnf )



{
  ## rename most recent ## 
  newmelones_fnf_mostrecent  <- newmelones_fnf_mostrecent %>% rename("monthday" = !!names(.[1]),
                                                             "Donnell" = !!names(.[2]),
                                                             "Beardsley" = !!names(.[3]),
                                                             "newmelones_upstream_stor" = !!names(.[4]),
                                                             "newmelones_upstream_dlystorchange" = !!names(.[5]),
                                                             "newmelones_observedinflowchange_meandaily" = !!names(.[6]),
                                                             "newmelones_observedinflow_meandaily" = !!names(.[7]),
                                                             "newmelones_natriver_fnf" = !!names(.[8]),
                                                             "newmelones_natriver_wyaccum" = !!names(.[9])  ) 
  
  as_tibble(newmelones_fnf_mostrecent)
}
date_mostrecent<- as.integer(newmelones_fnf_mostrecent$monthday)
date_nextmostrecent <- as.integer(date_mostrecent - 1)
date_mostrecent <- paste0(month,"/", date_mostrecent, "/", year) %>% mdy()
date_nextmostrecent <- paste0(month,"/", date_nextmostrecent, "/", year) %>% mdy()

newmelones_fnf_mostrecent <- newmelones_fnf_mostrecent %>% select(-monthday) 

## rename next most recent ## 
{
  newmelones_fnf_nextmostrecent  <- newmelones_fnf_nextmostrecent %>% rename("monthday" = !!names(.[1]),
                                                                             "Donnell" = !!names(.[2]),
                                                                             "Beardsley" = !!names(.[3]),
                                                                             "newmelones_upstream_stor" = !!names(.[4]),
                                                                             "newmelones_upstream_dlystorchange" = !!names(.[5]),
                                                                             "newmelones_observedinflowchange_meandaily" = !!names(.[6]),
                                                                             "newmelones_observedinflow_meandaily" = !!names(.[7]),
                                                                             "newmelones_natriver_fnf" = !!names(.[8]),
                                                                             "newmelones_natriver_wyaccum" = !!names(.[9])) %>% select(-monthday) 
}


as_tibble(newmelones_fnf_nextmostrecent)
as_tibble(newmelones_fnf_mostrecent)
###### convert mostrecent's to long format, add attributes######
#{
newmelones_fnf_mostrecent_t <- newmelones_fnf_mostrecent %>% gather(key = "res", value = "value")
nwsid <- c("dllc1", "besc1",  "nmsc1", "nmsc1", "nmsc1", "nmsc1", "nmsc1", "nmsc1")

usbr_web_param <- c("storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant", "upstrstorage_usbrcvo_latest_instant", 
                    "stor_usbrcvo_dlychnge_todelete",  "meancfs_usbrcvo_dlychnge_todelete", 
                    "inflow_usbrcvo_latest_meandly",
                    "fullnaturalflow_usbrcvo_latest_meandly", "fullnaturalflow_usbrcvo_latest_wyaccum")

unit <-  c("af", "af", "af", "af", "cfs","cfs", "cfs", "taf")
as_tibble(newmelones_fnf_mostrecent_t)
newmelones_fnf_mostrecent <- cbind(newmelones_fnf_mostrecent_t, nwsid,  unit, usbr_web_param)
rm(newmelones_fnf_mostrecent_t, unit, nwsid, usbr_web_param)
as_tibble(newmelones_fnf_mostrecent)

newmelones_fnf_mostrecent$value <- gsub(",", "", newmelones_fnf_mostrecent$value )
# newmelones_fnf_mostrecent$value <- gsub("+", "", newmelones_fnf_mostrecent$value ) #doesn't work, but below's as.numeric() seems to
as_tibble(newmelones_fnf_mostrecent)
newmelones_fnf_mostrecent <- newmelones_fnf_mostrecent %>% mutate(value = as.numeric(value)) 
as_tibble(newmelones_fnf_mostrecent)
#}

{
  newmelones_fnf_mostrecent <- newmelones_fnf_mostrecent %>% 
    #filter(usbr_web_param != usbr_web_param[4]) %>% filter(usbr_web_param != usbr_web_param[5])
    filter(usbr_web_param != "stor_usbrcvo_dlychnge_todelete") %>% filter(usbr_web_param != "meancfs_usbrcvo_dlychnge_todelete")
  as_tibble(newmelones_fnf_mostrecent)
}
###### convert mostrecent's to long format, add attributes######

{
  newmelones_fnf_nextmostrecent_t <- newmelones_fnf_nextmostrecent %>% gather(key = "res", value = "value")
  nwsid <- c("dllc1", "besc1",  "nmsc1", "nmsc1", "nmsc1", "nmsc1", "nmsc1", "nmsc1")
  
  usbr_web_param <- c("storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant", "upstrstorage_usbrcvo_nextlatest_instant", 
                      "stor_usbrcvo_dlychnge_todelete",  "meancfs_usbrcvo_dlychnge_todelete", 
                      "inflow_usbrcvo_nextlatest_meandly",
                      "fullnaturalflow_usbrcvo_nextlatest_meandly", "fullnaturalflow_usbrcvo_nextlatest_wyaccum")
  
  unit <-  c("af", "af", "af", "af", "cfs","cfs", "cfs", "taf")
  newmelones_fnf_nextmostrecent <- cbind(newmelones_fnf_nextmostrecent_t, nwsid, usbr_web_param, unit)
  rm(newmelones_fnf_nextmostrecent_t, unit, nwsid)#usbr_web_param
  as_tibble(newmelones_fnf_nextmostrecent)
  
  newmelones_fnf_nextmostrecent$value <- gsub(",", "", newmelones_fnf_nextmostrecent$value )
  newmelones_fnf_nextmostrecent$value <- gsub("+", "", newmelones_fnf_nextmostrecent$value ) #doesn't work, but below's as.numeric() seems to
  as_tibble(newmelones_fnf_nextmostrecent)
  newmelones_fnf_nextmostrecent <- newmelones_fnf_nextmostrecent %>% mutate(value = as.numeric(value)) 
  as_tibble(newmelones_fnf_nextmostrecent)
  newmelones_fnf_nextmostrecent <- newmelones_fnf_nextmostrecent %>%
  filter(usbr_web_param != "stor_usbrcvo_dlychnge_todelete") %>% filter(usbr_web_param != "meancfs_usbrcvo_dlychnge_todelete")
  #filter(usbr_web_param != usbr_web_param[4]) %>% filter(usbr_web_param != usbr_web_param[5])
  as_tibble(newmelones_fnf_nextmostrecent)
}
as_tibble(newmelones_fnf_mostrecent)
as_tibble(newmelones_fnf_nextmostrecent)


{
  newmelones_fnf_dlychnge <- inner_join(newmelones_fnf_mostrecent, newmelones_fnf_nextmostrecent, by = "res") 
  as_tibble(newmelones_fnf_dlychnge)
  newmelones_fnf_dlychnge <- newmelones_fnf_dlychnge %>% mutate(value = value.x - value.y)
  as_tibble(newmelones_fnf_dlychnge)
  newmelones_fnf_dlychnge <- newmelones_fnf_dlychnge %>% transmute(res, value, nwsid = nwsid.x, usbr_web_param = usbr_web_param.y, unit = unit.x)
  newmelones_fnf_dlychnge <- newmelones_fnf_dlychnge %>% mutate(date = date_mostrecent)
  newmelones_fnf_dlychnge$usbr_web_param <- gsub("nextlatest", "dailychange", newmelones_fnf_dlychnge$usbr_web_param ) 
  
  
  as_tibble(newmelones_fnf_dlychnge)
  
  ## add date column
  
  newmelones_fnf_mostrecent <- newmelones_fnf_mostrecent %>% mutate(date = date_mostrecent)
  newmelones_fnf_nextmostrecent <- newmelones_fnf_nextmostrecent %>% mutate(date = date_nextmostrecent)
  newmelones_fnf_dlychnge <- newmelones_fnf_dlychnge %>% mutate(date = date_mostrecent)
  
  newmelones_fnf  <- rbind(newmelones_fnf_mostrecent, newmelones_fnf_nextmostrecent, newmelones_fnf_dlychnge)
  newmelones_fnf_dlychnge$res <- gsub("newmelones_stor", "newmelones_stor", newmelones_fnf_dlychnge$res )
  rm(newmelones_fnf_mostrecent, newmelones_fnf_nextmostrecent, newmelones_fnf_dlychnge)

  as_tibble(newmelones_fnf)
  newmelones_fnf <- newmelones_fnf %>% mutate(value = round(value, 0))
  
  

  
  
}

newmelones_fnf$res<- gsub("newmelones_upstream_stor", "NewMelones", newmelones_fnf$res ) 
newmelones_fnf$res<- gsub("newmelones_observedinflow_meandaily", "NewMelones", newmelones_fnf$res ) 
newmelones_fnf$res<- gsub("newmelones_natriver_fnf", "NewMelones", newmelones_fnf$res ) 
newmelones_fnf$res<- gsub("newmelones_natriver_wyaccum", "NewMelones", newmelones_fnf$res ) 


newmelones_fnf <- newmelones_fnf %>%  separate(usbr_web_param, into = c("param_source_timeseq", "meastype"), sep="_(?=[^_]+$)") 
#https://stackoverflow.com/questions/50518137/separate-a-column-into-2-columns-at-the-last-underscore-in-r
newmelones_fnf <- newmelones_fnf %>%  separate(param_source_timeseq, into = c("param_source", "timeseq"), sep="_(?=[^_]+$)")
newmelones_fnf <- newmelones_fnf %>%  separate(param_source, into = c("param", "source"), sep="_(?=[^_]+$)")

as_tibble(newmelones_fnf)






## simplify 3 units to 2
newmelones_fnf <- newmelones_fnf %>% mutate(value = ifelse(unit == "taf", value * 1000 ,value)) %>% mutate(unit = ifelse(unit == "taf", "af", as.character(unit)))
as_tibble(newmelones_fnf)
#}

## add rows for res capacities
{
  #  ## capacities (from various sources online, including wiki) ##
  res <- c("Donnell", "Beardsley", "NewMelones")
  numres <- length(res)
  value <- c(56.83, 97.8) * 1000  #af
  combined <- sum(value)
  value <- c(value, combined)
  nwsid <- c("dllc1", "besc1", "nmsc1")
  param <- c(rep("totalcapacity", numres - 1), "upstrtotalcapacity")
  source <- rep(NA, numres)
  timeseq <- rep(NA, numres)
  meastype <- rep("instant", numres )
  unit <- rep("af", numres)
  date <- rep(NA, numres)
  res_cap <- data.frame(cbind(res, value, nwsid, param, source, timeseq, meastype, unit, date)) #%>% mutate(capacity = as.double(capacity))
}
#
newmelones_fnf <- rbind(newmelones_fnf, res_cap) %>% mutate(value = as.character(value), value = as.double(value))
rm(res_cap)
#
#
### new df to join to main df for percent capacity calc
{
  #  ## capacities (from various sources online, including wiki) ##
  res <- c("Donnell", "Beardsley")
  capacity <- c(56.83, 97.8) * 1000 #note named capacity, not value - removed below
  nwsid <-  nwsid <- c("dllc1", "besc1")
  res_cap <- data.frame(cbind(res, capacity, nwsid)) 
}


## latest percent cap
newmelones_fnf_percap_latest <- newmelones_fnf %>% filter(param == "storage") %>% filter( timeseq == "latest") 
newmelones_fnf_percap_latest <- right_join(newmelones_fnf_percap_latest, res_cap) %>% mutate(capacity = as.character(capacity), 
                                                                                     capacity = as.double(capacity))
newmelones_fnf_percap_latest <- newmelones_fnf_percap_latest %>% mutate(value = value/capacity * 100) %>%
  mutate(value = round(value, 1), unit = "percentfull") %>% select(-capacity) 

newmelones_fnf_percap_latest_upstr <- newmelones_fnf %>% filter(param == "upstrstorage") %>% filter( timeseq == "latest") %>% 
  mutate(capacity = sum(capacity), value = value/capacity * 100) %>%
  mutate(value = round(value, 1), unit = "percentfull") %>% select(-capacity) %>% mutate(date = date_mostrecent)

newmelones_fnf_percap_latest_upstr_val <- newmelones_fnf_percap_latest_upstr$value

## next latest percent cap
newmelones_fnf_percap_nextlatest <- newmelones_fnf %>% filter(param == "storage") %>% filter( timeseq == "nextlatest") 
newmelones_fnf_percap_nextlatest <- right_join(newmelones_fnf_percap_nextlatest, res_cap) %>% mutate(capacity = as.character(capacity), 
                                                                                             capacity = as.double(capacity))
newmelones_fnf_percap_nextlatest <- newmelones_fnf_percap_nextlatest %>% mutate(value = value/capacity * 100) %>%
  mutate(value = round(value, 1), unit = "percentfull") %>% select(-capacity)

newmelones_fnf_percap_nextlatest_upstr <- newmelones_fnf %>% filter(param == "upstrstorage") %>% filter( timeseq == "nextlatest") %>% 
  mutate(capacity = sum(capacity), value = value/capacity * 100) %>%
  mutate(value = round(value, 1), unit = "percentfull") %>% select(-capacity) %>% mutate(date = date_mostrecent)

newmelones_fnf_percap_nextlatest_upstr_val <- newmelones_fnf_percap_nextlatest_upstr$value

newmelones_fnf_percap_nextlatest_upstr_valchange <- newmelones_fnf_percap_latest_upstr_val - newmelones_fnf_percap_nextlatest_upstr_val
newmelones_fnf_percap_nextlatest_upstr_valchange <- newmelones_fnf_percap_nextlatest_upstr %>%
  mutate(value = newmelones_fnf_percap_nextlatest_upstr_valchange, timeseq = "dailychange")
## rbind
newmelones_fnf<- rbind(newmelones_fnf, newmelones_fnf_percap_latest, newmelones_fnf_percap_nextlatest, newmelones_fnf_percap_latest_upstr, newmelones_fnf_percap_nextlatest_upstr,
                   newmelones_fnf_percap_nextlatest_upstr_valchange)

## create percent empty and capacity remaining

newmelones_fnf_percentempty <- newmelones_fnf %>% filter(unit == "percentfull", timeseq != "dailychange") %>% mutate(value = 100 - value, unit = "percentempty")
newmelones_fnf <- rbind(newmelones_fnf, newmelones_fnf_percentempty)


## create volume remaining 
newmelones_fnf_totcap <- newmelones_fnf %>% filter(param == "totalcapacity" | param == "upstrtotalcapacity") %>% select(value, nwsid, param) 

newmelones_fnf_stor_latest <- newmelones_fnf %>% filter(param == "storage" | param == "upstrstorage", timeseq == "latest", unit == "af")
newmelones_remainingcap_latest <- left_join(newmelones_fnf_stor_latest, newmelones_fnf_totcap, by = "nwsid" , "param") %>% 
  mutate(value = value.y - value.x, param = ifelse(param == "upstrstorage", "remainingupstrcapacity", "remainingcapacity")) %>% select(-value.x, -value.y, -param.x, -param.y)


newmelones_fnf_stor_nextlatest <- newmelones_fnf %>% filter(param == "storage" | param == "upstrstorage", timeseq == "nextlatest", unit == "af")
newmelones_remainingcap_nextlatest <- left_join(newmelones_fnf_stor_nextlatest, newmelones_fnf_totcap, by = "nwsid" , "param") %>% 
  mutate(value = value.y - value.x, param = ifelse(param.x == "upstrstorage", "remainingupstrcapacity", "remainingcapacity")) %>% select(-value.x, -value.y, -param.x, -param.y)

newmelones_fnf <- rbind(newmelones_fnf, newmelones_remainingcap_latest, newmelones_remainingcap_nextlatest )

## percentfull dailychange

newmelones_fnf_perc_change_latest <- newmelones_fnf %>% filter(param == "storage", timeseq == "latest", unit == "percentfull") %>% select(value, nwsid, param) 
newmelones_fnf_perc_change_nextlatest <- newmelones_fnf %>% filter(param == "storage", timeseq == "nextlatest", unit == "percentfull") 
newmelones_fnf_perc_change <- left_join(newmelones_fnf_perc_change_latest, newmelones_fnf_perc_change_nextlatest,  by = "nwsid" , "param") %>%
  mutate(value = round((value.x - value.y)/value.x * 100,1) , timeseq = "dailychange", param = "storage") %>% 
  select(-param.x, -param.y, -value.x, -value.y) %>% mutate(unit = "percentchange") %>% mutate(date = date_mostrecent)

newmelones_fnf <- rbind(newmelones_fnf, newmelones_fnf_perc_change)



## add inflow volume class from cfs

newmelones_fnf_inflowdlyaf <- newmelones_fnf %>% filter(param == "inflow", meastype == "meandly", unit == "cfs") %>% mutate(value = value * 1.98347, unit = "af")
newmelones_fnf <- rbind(newmelones_fnf, newmelones_fnf_inflowdlyaf) 


## stamp columns with usbr 
newmelones_fnf <- data.frame(newmelones_fnf) %>% 
  mutate(value = unlist(value)) %>% 
  filter(timeseq != "nextlatest") %>% 
  select(-source) %>% rename_all(paste0, "_usbr") %>% 
  mutate(nwsid = nwsid_usbr) %>% select(-nwsid_usbr)  

rm(list= ls()[!(ls() %in% mainkeepers)])
as_tibble(newmelones_fnf)


}

  
#}
