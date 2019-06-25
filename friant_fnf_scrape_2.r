


{
#rm(list = ls()) 
  rm(list= ls()[!(ls() %in% mainkeepers)])
#setwd("~/R/proj/publicflowscrapeapp")
setwd("~/Documents/publicflowvis")
source("libs.r")





file <- "https://www.usbr.gov/mp/cvo/vungvari/milfln.pdf"
rawtext  <- pdf_text(file)
#rawtext
month_year  <- str_match(rawtext, "CALIFORNIA\r\n(.*?)FULL")  #https://stackoverflow.com/questions/39086400/extracting-a-string-between-other-two-strings-in-r
if (anyNA(month_year)) {month_year  <- str_match(rawtext, "CALIFORNIA\n(.*?)FULL") }


month_year <- month_year[,2] %>% trimws() %>% as.yearmon() 

month <- month(month_year)
year <- year(month_year)
start <- "\n    1"
end <- "\n  TOTALS"
friant_fnf <- read.table(text=substring(rawtext, regexpr(start, rawtext), regexpr(end, rawtext)))
rm(start, end, month_year, rawtext)
as_tibble(friant_fnf)
nrow_friant_fnf <- nrow(friant_fnf)

friant_fnf_mostrecent <- friant_fnf[nrow_friant_fnf,]  #most recent row from usbr
friant_fnf_nextmostrecent <- friant_fnf[nrow_friant_fnf - 1,]  #next most recent row from usbr
as_tibble(friant_fnf_mostrecent)
as_tibble(friant_fnf_nextmostrecent)
rm(friant_fnf, nrow_friant_fnf )

#}

{
## rename most recent ## 
friant_fnf_mostrecent  <- friant_fnf_mostrecent %>% rename("monthday" = !!names(.[1]),
                                                           "Edison" = !!names(.[2]),
                                                           "Florence" = !!names(.[3]),
                                                           "Huntington" = !!names(.[4]),
                                                           "Shaver" = !!names(.[5]),
                                                           "MammothPool" = !!names(.[6]) ,
                                                           "Redinger" = !!names(.[7]),
                                                           "CraneValley" = !!names(.[8]),
                                                           "Kerckhoff" = !!names(.[9]),
                                                           "Millerton_upstream_stor" = !!names(.[10])  ,                                          
                                                           "Millerton_upstream_dlystorchange" = !!names(.[11]),
                                                           "Millerton_observedinflowchange_meandaily" = !!names(.[12]),
                                                           "Millerton_observedinflow_meandaily" = !!names(.[13]),
                                                           "Millerton_natriver_fnf" = !!names(.[14]),
                                                           "Millerton_natriver_wyaccum" = !!names(.[15])) 
}
as_tibble(friant_fnf_mostrecent)
date_mostrecent<- as.integer(friant_fnf_mostrecent$monthday)
date_nextmostrecent <- as.integer(date_mostrecent - 1)
date_mostrecent <- paste0(month,"/", date_mostrecent, "/", year) %>% mdy()
date_nextmostrecent <- paste0(month,"/", date_nextmostrecent, "/", year) %>% mdy()

friant_fnf_mostrecent <- friant_fnf_mostrecent %>% select(-monthday) 

## rename next most recent ## 
{
friant_fnf_nextmostrecent  <- friant_fnf_nextmostrecent %>% rename("monthday" = !!names(.[1]),
                                                                   "Edison" = !!names(.[2]),
                                                                   "Florence" = !!names(.[3]),
                                                                   "Huntington" = !!names(.[4]),
                                                                   "Shaver" = !!names(.[5]),
                                                                   "MammothPool" = !!names(.[6]) ,
                                                                   "Redinger" = !!names(.[7]),
                                                                   "CraneValley" = !!names(.[8]),
                                                                   "Kerckhoff" = !!names(.[9]),
                                                                   "Millerton_upstream_stor" = !!names(.[10])  ,                                          
                                                                   "Millerton_upstream_dlystorchange" = !!names(.[11]),
                                                                   "Millerton_observedinflowchange_meandaily" = !!names(.[12]),
                                                                   "Millerton_observedinflow_meandaily" = !!names(.[13]),
                                                                   "Millerton_natriver_fnf" = !!names(.[14]),
                                                                   "Millerton_natriver_wyaccum" = !!names(.[15])) %>% select(-monthday)
}


as_tibble(friant_fnf_nextmostrecent)
as_tibble(friant_fnf_mostrecent)
###### convert mostrecent's to long format, add attributes######
{
friant_fnf_mostrecent_t <- friant_fnf_mostrecent %>% gather(key = "res", value = "value")
nwsid <- c("taec1", "flec1", "hntc1", "savc1", "mplc1", "rgrc1", "basc1", "krhc1", "frac1", 
                    "frac1", "frac1", "frac1", "frac1", "frac1")
usbr_web_param <- c("storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant", 
                    "storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant",
                    "storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant", "upstrstorage_usbrcvo_latest_instant", 
                    "stor_usbrcvo_dlychnge_todelete", "meancfs_usbrcvo_dlychnge_todelete", 
                    "inflow_usbrcvo_latest_meandly", "fullnaturalflow_usbrcvo_latest_meandly", 
                    "fullnaturalflow_usbrcvo_latest_wyaccum")

unit <- c("af", "af", "af", "af", "af", "af", "af", "af", "af", "af", "cfs", "cfs", "cfs", "taf")

friant_fnf_mostrecent <- cbind(friant_fnf_mostrecent_t, nwsid, usbr_web_param, unit)
rm(friant_fnf_mostrecent_t, unit, usbr_web_param, nwsid)
as_tibble(friant_fnf_mostrecent)

friant_fnf_mostrecent$value <- gsub(",", "", friant_fnf_mostrecent$value )
friant_fnf_mostrecent$value <- gsub("+", "", friant_fnf_mostrecent$value ) #doesn't work, but below's as.numeric() seems to
as_tibble(friant_fnf_mostrecent)
friant_fnf_mostrecent <- friant_fnf_mostrecent %>% mutate(value = as.numeric(value)) 
as_tibble(friant_fnf_mostrecent)
}

{
friant_fnf_mostrecent <- friant_fnf_mostrecent %>% 
                        filter(usbr_web_param != "stor_usbrcvo_dlychnge_todelete") %>% filter(usbr_web_param != "meancfs_usbrcvo_dlychnge_todelete")
as_tibble(friant_fnf_mostrecent)
}
###### convert mostrecent's to long format, add attributes######

{
friant_fnf_nextmostrecent_t <- friant_fnf_nextmostrecent %>% gather(key = "res", value = "value")
nwsid <- c("taec1", "flec1", "hntc1", "savc1", "mplc1", "rgrc1", "basc1", "krhc1", "frac1", 
            "frac1", "frac1", "frac1", "frac1", "frac1")
usbr_web_param <- c("storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant", 
                    "storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant",
                    "storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant", "upstrstorage_usbrcvo_nextlatest_instant", 
                    "stor_usbrcvo_dlychnge_todelete", "meancfs_usbrcvo_dlychnge_todelete", 
                    "inflow_usbrcvo_nextlatest_meandly",  "fullnaturalflow_usbrcvo_nextlatest_meandly", 
                    "fullnaturalflow_usbrcvo_nextlatest_wyaccum")

unit <- c("af", "af", "af", "af", "af", "af", "af", "af", "af", "af", "cfs", "cfs", "cfs", "taf")

friant_fnf_nextmostrecent <- cbind(friant_fnf_nextmostrecent_t, nwsid, usbr_web_param, unit)
rm(friant_fnf_nextmostrecent_t, unit, usbr_web_param, nwsid)
as_tibble(friant_fnf_nextmostrecent)

friant_fnf_nextmostrecent$value <- gsub(",", "", friant_fnf_nextmostrecent$value )
#friant_fnf_nextmostrecent$value <- gsub("+", "", friant_fnf_nextmostrecent$value ) #doesn't work, but below's as.numeric() seems to
as_tibble(friant_fnf_nextmostrecent)
friant_fnf_nextmostrecent <- friant_fnf_nextmostrecent %>% mutate(value = as.numeric(value)) 
as_tibble(friant_fnf_nextmostrecent)
friant_fnf_nextmostrecent <- friant_fnf_nextmostrecent %>%
                             filter(usbr_web_param != "stor_usbrcvo_dlychnge_todelete") %>% filter(usbr_web_param != "meancfs_usbrcvo_dlychnge_todelete")
as_tibble(friant_fnf_nextmostrecent)
}
as_tibble(friant_fnf_mostrecent)
as_tibble(friant_fnf_nextmostrecent)


{
friant_fnf_dlychnge <- inner_join(friant_fnf_mostrecent, friant_fnf_nextmostrecent, by = "res") 
as_tibble(friant_fnf_dlychnge)
friant_fnf_dlychnge <- friant_fnf_dlychnge %>% mutate(value = value.x - value.y)
as_tibble(friant_fnf_dlychnge)
friant_fnf_dlychnge <- friant_fnf_dlychnge %>% transmute(res, value, nwsid = nwsid.x, usbr_web_param = usbr_web_param.y, unit = unit.x)
friant_fnf_dlychnge <- friant_fnf_dlychnge %>% mutate(date = date_mostrecent)
friant_fnf_dlychnge$usbr_web_param <- gsub("nextlatest", "dailychange", friant_fnf_dlychnge$usbr_web_param ) 
                                  

as_tibble(friant_fnf_dlychnge)

## add date column

friant_fnf_mostrecent <- friant_fnf_mostrecent %>% mutate(date = date_mostrecent)
friant_fnf_nextmostrecent <- friant_fnf_nextmostrecent %>% mutate(date = date_nextmostrecent)
friant_fnf_dlychnge <- friant_fnf_dlychnge %>% mutate(date = date_mostrecent)

friant_fnf  <- rbind(friant_fnf_mostrecent, friant_fnf_nextmostrecent, friant_fnf_dlychnge)
#friant_fnf_dlychnge$res <- gsub("Millerton_upstream_stor", "Millerton_upstream_stor", friant_fnf_dlychnge$res )
rm(friant_fnf_mostrecent, friant_fnf_nextmostrecent, friant_fnf_dlychnge)
as_tibble(friant_fnf)

friant_fnf <- friant_fnf %>% mutate(value = round(value, 0))

friant_fnf$res<- gsub("Millerton_upstream_stor", "Millerton", friant_fnf$res ) 
friant_fnf$res<- gsub("Millerton_observedinflow_meandaily", "Millerton", friant_fnf$res ) 
friant_fnf$res<- gsub("Millerton_natriver_fnf", "Millerton", friant_fnf$res ) 
friant_fnf$res<- gsub("Millerton_natriver_wyaccum", "Millerton", friant_fnf$res ) 


friant_fnf <- friant_fnf %>%  separate(usbr_web_param, into = c("param_source_timeseq", "meastype"), sep="_(?=[^_]+$)") 
#https://stackoverflow.com/questions/50518137/separate-a-column-into-2-columns-at-the-last-underscore-in-r
friant_fnf <- friant_fnf %>%  separate(param_source_timeseq, into = c("param_source", "timeseq"), sep="_(?=[^_]+$)")
friant_fnf <- friant_fnf %>%  separate(param_source, into = c("param", "source"), sep="_(?=[^_]+$)")

as_tibble(friant_fnf)

}

## 3 units to 2
friant_fnf <- friant_fnf %>% mutate(value = ifelse(unit == "taf", value * 1000 ,value)) %>% mutate(unit = ifelse(unit == "taf", "af", as.character(unit)))

as_tibble(friant_fnf)


## add rows for res capacities
{
#  ## capacities (from various sources online, including wiki) ##
res <- c("Edison", "Florence", "Huntington", "Shaver", "MammothPool", "Redinger", "CraneValley", "Kerckhoff", "Millerton")
  numres <- length(res)
  value <- c(125.0, 64.6, 88.834, 135.283, 123.0, 35.0, 45.4, 4.252) * 1000  #af
  combined <- sum(value)
  value <- c(value, combined)
  nwsid <- c("taec1", "flec1", "hntc1", "savc1", "mplc1", "rgrc1", "basc1", "krhc1", "frac1")
  param <- c(rep("totalcapacity", numres - 1), "upstrtotalcapacity")
  source <- rep(NA, numres)
  timeseq <- rep(NA, numres)
  meastype <- rep("instant", numres )
  unit <- rep("af", numres)
  date <- rep(NA, numres)
  res_cap <- data.frame(cbind(res, value, nwsid, param, source, timeseq, meastype, unit, date)) #%>% mutate(capacity = as.double(capacity))
}
#
friant_fnf <- rbind(friant_fnf, res_cap) %>% mutate(value = as.character(value), value = as.double(value))
rm(res_cap)
#
#
### new df to join to main df for percent capacity calc
{
#  ## capacities (from various sources online, including wiki) ##
 res <- c("Edison", "Florence", "Huntington", "Shaver", "MammothPool", "Redinger", "CraneValley", "Kerckhoff") #friant's storage itself not published on fnf site
 capacity <- c(125.0, 64.6, 88.834, 135.283, 123.0, 35.0, 45.4, 4.252) * 1000 #note named capacity, not value - removed below
 nwsid <- c("taec1", "flec1", "hntc1", "savc1", "mplc1", "rgrc1", "basc1", "krhc1")
 res_cap <- data.frame(cbind(res, capacity, nwsid)) 
}

## latest percent cap
friant_fnf_percap_latest <- friant_fnf %>% filter(param == "storage") %>% filter( timeseq == "latest") 
friant_fnf_percap_latest <- right_join(friant_fnf_percap_latest, res_cap) %>% mutate(capacity = as.character(capacity), 
                                                                                     capacity = as.double(capacity))
friant_fnf_percap_latest <- friant_fnf_percap_latest %>% mutate(value = value/capacity * 100) %>%
  mutate(value = round(value, 1), unit = "percentfull") %>% select(-capacity)

friant_fnf_percap_latest_upstr <- friant_fnf %>% filter(param == "upstrstorage") %>% filter( timeseq == "latest") %>% 
  mutate(capacity = sum(capacity), value = value/capacity * 100) %>%
  mutate(value = round(value, 1), unit = "percentfull") %>% select(-capacity) %>% mutate(date = date_mostrecent)

friant_fnf_percap_latest_upstr_val <- friant_fnf_percap_latest_upstr$value

## next latest percent cap
friant_fnf_percap_nextlatest <- friant_fnf %>% filter(param == "storage") %>% filter( timeseq == "nextlatest") 
friant_fnf_percap_nextlatest <- right_join(friant_fnf_percap_nextlatest, res_cap) %>% mutate(capacity = as.character(capacity), 
                                                                                             capacity = as.double(capacity))
friant_fnf_percap_nextlatest <- friant_fnf_percap_nextlatest %>% mutate(value = value/capacity * 100) %>%
  mutate(value = round(value, 1), unit = "percentfull") %>% select(-capacity)

friant_fnf_percap_nextlatest_upstr <- friant_fnf %>% filter(param == "upstrstorage") %>% filter( timeseq == "nextlatest") %>% 
  mutate(capacity = sum(capacity), value = value/capacity * 100) %>%
  mutate(value = round(value, 1), unit = "percentfull") %>% select(-capacity) %>% mutate(date = date_mostrecent)

friant_fnf_percap_nextlatest_upstr_val <- friant_fnf_percap_nextlatest_upstr$value

friant_fnf_percap_nextlatest_upstr_valchange <- friant_fnf_percap_latest_upstr_val - friant_fnf_percap_nextlatest_upstr_val
friant_fnf_percap_nextlatest_upstr_valchange <- friant_fnf_percap_nextlatest_upstr %>%
  mutate(value = friant_fnf_percap_nextlatest_upstr_valchange, timeseq = "dailychange")
## rbind
friant_fnf<- rbind(friant_fnf, friant_fnf_percap_latest, friant_fnf_percap_nextlatest, friant_fnf_percap_latest_upstr, friant_fnf_percap_nextlatest_upstr,
                   friant_fnf_percap_nextlatest_upstr_valchange)

## create percent empty and capacity remaining

friant_fnf_percentempty <- friant_fnf %>% filter(unit == "percentfull", timeseq != "dailychange") %>% mutate(value = 100 - value, unit = "percentempty")
friant_fnf <- rbind(friant_fnf, friant_fnf_percentempty)


## create volume remaining 
friant_fnf_totcap <- friant_fnf %>% filter(param == "totalcapacity" | param == "upstrtotalcapacity") %>% select(value, nwsid, param) 

friant_fnf_stor_latest <- friant_fnf %>% filter(param == "storage" | param == "upstrstorage", timeseq == "latest", unit == "af")
friant_remainingcap_latest <- left_join(friant_fnf_stor_latest, friant_fnf_totcap, by = "nwsid" , "param") %>% 
  mutate(value = value.y - value.x, param = ifelse(param == "upstrstorage", "remainingupstrcapacity", "remainingcapacity")) %>% select(-value.x, -value.y, -param.x, -param.y)


friant_fnf_stor_nextlatest <- friant_fnf %>% filter(param == "storage" | param == "upstrstorage", timeseq == "nextlatest", unit == "af")
friant_remainingcap_nextlatest <- left_join(friant_fnf_stor_nextlatest, friant_fnf_totcap, by = "nwsid" , "param") %>% 
  mutate(value = value.y - value.x, param = ifelse(param.x == "upstrstorage", "remainingupstrcapacity", "remainingcapacity")) %>% select(-value.x, -value.y, -param.x, -param.y)

friant_fnf <- rbind(friant_fnf, friant_remainingcap_latest, friant_remainingcap_nextlatest )

## percentfull dailychange

friant_fnf_perc_change_latest <- friant_fnf %>% filter(param == "storage", timeseq == "latest", unit == "percentfull") %>% select(value, nwsid, param) 
friant_fnf_perc_change_nextlatest <- friant_fnf %>% filter(param == "storage", timeseq == "nextlatest", unit == "percentfull") 
friant_fnf_perc_change <- left_join(friant_fnf_perc_change_latest, friant_fnf_perc_change_nextlatest,  by = "nwsid" , "param") %>%
                        mutate(value = round((value.x - value.y)/value.x * 100,1) , timeseq = "dailychange", param = "storage") %>% 
                        select(-param.x, -param.y, -value.x, -value.y) %>% mutate(unit = "percentchange") %>% mutate(date = date_mostrecent)

friant_fnf <- rbind(friant_fnf, friant_fnf_perc_change)

## add inflow volume class from cfs

friant_fnf_inflowdlyaf <- friant_fnf %>% filter(param == "inflow", meastype == "meandly", unit == "cfs") %>% mutate(value = value * 1.98347, unit = "af")
friant_fnf <- rbind(friant_fnf, friant_fnf_inflowdlyaf) 


friant_fnf <- data.frame(friant_fnf) %>% mutate(value = unlist(value)) %>% filter(timeseq != "nextlatest") %>% select(-source) %>% rename_all(paste0, "_usbr") %>% 
                                        mutate(nwsid = nwsid_usbr) %>% select(-nwsid_usbr)  

rm(list= ls()[!(ls() %in% mainkeepers)])
as_tibble(friant_fnf)



}
