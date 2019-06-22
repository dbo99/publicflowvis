


{
#rm(list = ls()) 
  rm(list=setdiff(ls(),  "shasta_fnf"))
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
nws_id <- c("TAEC1", "FLEC1", "HNTC1", "SAVC1", "MPLC1", "RGRC1", "BASC1", "KRHC1", "FRAC1", 
                    "FRAC1", "FRAC1", "FRAC1", "FRAC1", "FRAC1")
usbr_web_param <- c("storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant", 
                    "storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant",
                    "storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant", "upstrstorage_usbrcvo_latest_instant", 
                    "stor_usbrcvo_dlychnge_todelete", "meancfs_usbrcvo_dlychnge_todelete", 
                    "inflow_usbrcvo_latest_meandly", "fullnaturalflow_usbrcvo_latest_meandly", 
                    "fullnaturalflow_usbrcvo_latest_wyaccum")

unit <- c("af", "af", "af", "af", "af", "af", "af", "af", "af", "af", "cfs", "cfs", "cfs", "taf")

friant_fnf_mostrecent <- cbind(friant_fnf_mostrecent_t, nws_id, usbr_web_param, unit)
rm(friant_fnf_mostrecent_t, unit, usbr_web_param, nws_id)
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
nws_id <- c("TAEC1", "FLEC1", "HNTC1", "SAVC1", "MPLC1", "RGRC1", "BASC1", "KRHC1", "FRAC1", 
            "FRAC1", "FRAC1", "FRAC1", "FRAC1", "FRAC1")
usbr_web_param <- c("storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant", 
                    "storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant",
                    "storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant", "upstrstorage_usbrcvo_nextlatest_instant", 
                    "stor_usbrcvo_dlychnge_todelete", "meancfs_usbrcvo_dlychnge_todelete", 
                    "inflow_usbrcvo_nextlatest_meandly",  "fullnaturalflow_usbrcvo_nextlatest_meandly", 
                    "fullnaturalflow_usbrcvo_nextlatest_wyaccum")

unit <- c("af", "af", "af", "af", "af", "af", "af", "af", "af", "af", "cfs", "cfs", "cfs", "taf")

friant_fnf_nextmostrecent <- cbind(friant_fnf_nextmostrecent_t, nws_id, usbr_web_param, unit)
rm(friant_fnf_nextmostrecent_t, unit, usbr_web_param, nws_id)
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
friant_fnf_dlychnge <- friant_fnf_dlychnge %>% transmute(res, value, nws_id = nws_id.x, usbr_web_param = usbr_web_param.y, unit = unit.x)
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



## simply 3 units to 2
friant_fnf <- friant_fnf %>% mutate(value = ifelse(unit == "taf", value * 1000 ,value)) %>% mutate(unit = ifelse(unit == "taf", "af", as.character(unit)))

as_tibble(friant_fnf)
}





## capacities (from various sources online, including wiki) ##
{
  res_id_nws <- c("TAEC1", "FLEC1", "HNTC1", "SAVC1", "MPLC1", "RGRC1", "BASC1", "KRHC1")
  res_cap <- c(125.0, 64.6, 88.834, 135.283, 123.0, 35.0, 45.4, 4.252)
  res_cap <- data.frame(res_id_nws, res_cap)
  rm(res_id_nws)
  as_tibble(res_cap)
}


