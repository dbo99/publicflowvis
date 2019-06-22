


{
rm(list = ls()) 
#  rm(list=setdiff(ls(), "shasta_fnf", "friant_fnf", "newmelones_fnf"))
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
end <- "\n TOTALS"  #Friant and Shasta have more white space in between `n` and `TOTALS`
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
nws_id <- c("DLLC1", "BESC1",  "NMSC1", "NMSC1", "NMSC1", "NMSC1", "NMSC1", "NMSC1")

usbr_web_param <- c("storage_usbrcvo_latest_instant", "storage_usbrcvo_latest_instant", "upstrstorage_usbrcvo_latest_instant", 
                    "stor_usbrcvo_dlychnge_todelete",  "meancfs_usbrcvo_dlychnge_todelete", 
                    "inflow_usbrcvo_latest_meandly",
                    "fullnaturalflow_usbrcvo_latest_meandly", "fullnaturalflow_usbrcvo_latest_wyaccum")

unit <-  c("af", "af", "af", "af", "cfs","cfs", "cfs", "taf")
as_tibble(newmelones_fnf_mostrecent_t)
newmelones_fnf_mostrecent <- cbind(newmelones_fnf_mostrecent_t, nws_id,  unit, usbr_web_param)
rm(newmelones_fnf_mostrecent_t, unit, nws_id, usbr_web_param)
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
  nws_id <- c("DLLC1", "BESC1",  "NMSC1", "NMSC1", "NMSC1", "NMSC1", "NMSC1", "NMSC1")
  
  usbr_web_param <- c("storage_usbrcvo_nextlatest_instant", "storage_usbrcvo_nextlatest_instant", "upstrstorage_usbrcvo_nextlatest_instant", 
                      "stor_usbrcvo_dlychnge_todelete",  "meancfs_usbrcvo_dlychnge_todelete", 
                      "inflow_usbrcvo_nextlatest_meandly",
                      "fullnaturalflow_usbrcvo_nextlatest_meandly", "fullnaturalflow_usbrcvo_nextlatest_wyaccum")
  
  unit <-  c("af", "af", "af", "af", "cfs","cfs", "cfs", "taf")
  newmelones_fnf_nextmostrecent <- cbind(newmelones_fnf_nextmostrecent_t, nws_id, usbr_web_param, unit)
  rm(newmelones_fnf_nextmostrecent_t, unit, nws_id)#usbr_web_param
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
  newmelones_fnf_dlychnge <- newmelones_fnf_dlychnge %>% transmute(res, value, nws_id = nws_id.x, usbr_web_param = usbr_web_param.y, unit = unit.x)
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


}



## simplify 3 units to 2
newmelones_fnf <- newmelones_fnf %>% mutate(value = ifelse(unit == "taf", value * 1000 ,value)) %>% mutate(unit = ifelse(unit == "taf", "af", as.character(unit)))
as_tibble(newmelones_fnf)
#}


## capacities (from various sources online, including wiki) ##
{
  res_id_nws <- c("DLLC1", "BESC1", "NMSC1")
  res_cap <- c(56.83, 97.8, 2400)  #taf
  res_cap <- data.frame(res_id_nws, res_cap)
  rm(res_id_nws)
  as_tibble(res_cap)
}

