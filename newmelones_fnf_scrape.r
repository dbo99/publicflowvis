


{
rm(list = ls()) 
setwd("~/R/proj/publicflowscrapeapp")
source("libs.r")

file <- "https://www.usbr.gov/mp/cvo/vungvari/nmlfln.pdf"
rawtext  <- pdf_text(file)
#rawtext
month_year  <- str_match(rawtext, "CALIFORNIA\r\n(.*?)FULL")  #https://stackoverflow.com/questions/39086400/extracting-a-string-between-other-two-strings-in-r
month_year <- month_year[,2] %>% trimws() %>% as.yearmon() 
month <- month(month_year)
year <- year(month_year)
start <- "\n    1"
end <- "\n TOTALS"  #Friant and Shasta have more white space in between `n` and `TOTALS`
NewMelones_fnf <- read.table(text=substring(rawtext, regexpr(start, rawtext), regexpr(end, rawtext)))

rm(start, end, month_year, rawtext)
as_tibble(NewMelones_fnf)
nrow_NewMelones_fnf <- nrow(NewMelones_fnf)

NewMelones_fnf_mostrecent <- NewMelones_fnf[nrow_NewMelones_fnf,]  #most recent row from usbr
NewMelones_fnf_nextmostrecent <- NewMelones_fnf[nrow_NewMelones_fnf - 1,]  #next most recent row from usbr
as_tibble(NewMelones_fnf_mostrecent)
as_tibble(NewMelones_fnf_nextmostrecent)
rm(NewMelones_fnf, nrow_NewMelones_fnf )



{
  ## rename most recent ## 
  NewMelones_fnf_mostrecent  <- NewMelones_fnf_mostrecent %>% rename("monthday" = !!names(.[1]),
                                                             "Donnell" = !!names(.[2]),
                                                             "Beardsley" = !!names(.[3]),
                                                             "NewMelones_upstream_stor" = !!names(.[4]),
                                                             "NewMelones_upstream_dlystorchange" = !!names(.[5]),
                                                             "NewMelones_observedinflowchange_meandaily" = !!names(.[6]),
                                                             "NewMelones_observedinflow_meandaily" = !!names(.[7]),
                                                             "NewMelones_natriver_fnf" = !!names(.[8]),
                                                             "NewMelones_natriver_WYaccum" = !!names(.[9])  ) 
  
  as_tibble(NewMelones_fnf_mostrecent)
}
date_mostrecent<- as.integer(NewMelones_fnf_mostrecent$monthday)
date_nextmostrecent <- as.integer(date_mostrecent - 1)
date_mostrecent <- paste0(month,"/", date_mostrecent, "/", year) %>% mdy()
date_nextmostrecent <- paste0(month,"/", date_nextmostrecent, "/", year) %>% mdy()

NewMelones_fnf_mostrecent <- NewMelones_fnf_mostrecent %>% select(-monthday) 

## rename next most recent ## 
{
  NewMelones_fnf_nextmostrecent  <- NewMelones_fnf_nextmostrecent %>% rename("monthday" = !!names(.[1]),
                                                                             "Donnell" = !!names(.[2]),
                                                                             "Beardsley" = !!names(.[3]),
                                                                             "NewMelones_upstream_stor" = !!names(.[4]),
                                                                             "NewMelones_upstream_dlystorchange" = !!names(.[5]),
                                                                             "NewMelones_observedinflowchange_meandaily" = !!names(.[6]),
                                                                             "NewMelones_observedinflow_meandaily" = !!names(.[7]),
                                                                             "NewMelones_natriver_fnf" = !!names(.[8]),
                                                                             "NewMelones_natriver_WYaccum" = !!names(.[9])) %>% select(-monthday) 
}


as_tibble(NewMelones_fnf_nextmostrecent)
as_tibble(NewMelones_fnf_mostrecent)
###### convert mostrecent's to long format, add attributes######
#{
NewMelones_fnf_mostrecent_t <- NewMelones_fnf_mostrecent %>% gather(key = "res", value = "value")
nws_id <- c("DLLC1", "BESC1",  "NMSC1", "NMSC1", "NMSC1", "NMSC1", "NMSC1", "NMSC1")

usbr_web_param <- c("stor_cvopublished_latest", "stor_cvopublished_latest", "stor_cvopublished_latest", 
                    "stor_cvopublished_dlychnge_todelete",  "meancfs_cvopublished_dlychnge_todelete", 
                    "meandlyinflowobs_cvopublished_latest",
                    "meanfnf_cvopublished_latest", "wyaccum_cvopublished_latest")

unit <-  c("af", "af", "af", "af", "cfs","cfs", "cfs", "taf")
as_tibble(NewMelones_fnf_mostrecent_t)
NewMelones_fnf_mostrecent <- cbind(NewMelones_fnf_mostrecent_t, nws_id, usbr_web_param, unit)
rm(NewMelones_fnf_mostrecent_t, unit, usbr_web_param, nws_id)
as_tibble(NewMelones_fnf_mostrecent)

NewMelones_fnf_mostrecent$value <- gsub(",", "", NewMelones_fnf_mostrecent$value )
# NewMelones_fnf_mostrecent$value <- gsub("+", "", NewMelones_fnf_mostrecent$value ) #doesn't work, but below's as.numeric() seems to
as_tibble(NewMelones_fnf_mostrecent)
NewMelones_fnf_mostrecent <- NewMelones_fnf_mostrecent %>% mutate(value = as.numeric(value)) 
as_tibble(NewMelones_fnf_mostrecent)
#}

{
  NewMelones_fnf_mostrecent <- NewMelones_fnf_mostrecent %>% 
    filter(usbr_web_param != "stor_cvopublished_dlychnge_todelete") %>% filter(usbr_web_param != "meancfs_cvopublished_dlychnge_todelete")
  as_tibble(NewMelones_fnf_mostrecent)
}
###### convert mostrecent's to long format, add attributes######

{
  NewMelones_fnf_nextmostrecent_t <- NewMelones_fnf_nextmostrecent %>% gather(key = "res", value = "value")
  nws_id <- c("DLLC1", "BESC1",  "NMSC1", "NMSC1", "NMSC1", "NMSC1", "NMSC1", "NMSC1")
  
  usbr_web_param <- c("stor_cvopublished_nextlatest", "stor_cvopublished_nextlatest", "stor_cvopublished_nextlatest", 
                      "stor_cvopublished_dlychnge_todelete",  "meancfs_cvopublished_dlychnge_todelete", 
                      "meandlyinflowobs_cvopublished_nextlatest",
                      "meanfnf_cvopublished_nextlatest", "wyaccum_cvopublished_nextlatest")
  
  unit <-  c("af", "af", "af", "af", "cfs","cfs", "cfs", "taf")
  NewMelones_fnf_nextmostrecent <- cbind(NewMelones_fnf_nextmostrecent_t, nws_id, usbr_web_param, unit)
  rm(NewMelones_fnf_nextmostrecent_t, unit, usbr_web_param, nws_id)
  as_tibble(NewMelones_fnf_nextmostrecent)
  
  NewMelones_fnf_nextmostrecent$value <- gsub(",", "", NewMelones_fnf_nextmostrecent$value )
  NewMelones_fnf_nextmostrecent$value <- gsub("+", "", NewMelones_fnf_nextmostrecent$value ) #doesn't work, but below's as.numeric() seems to
  as_tibble(NewMelones_fnf_nextmostrecent)
  NewMelones_fnf_nextmostrecent <- NewMelones_fnf_nextmostrecent %>% mutate(value = as.numeric(value)) 
  as_tibble(NewMelones_fnf_nextmostrecent)
  NewMelones_fnf_nextmostrecent <- NewMelones_fnf_nextmostrecent %>%
    filter(usbr_web_param != "stor_cvopublished_dlychnge_todelete") %>% filter(usbr_web_param != "meancfs_cvopublished_dlychnge_todelete")
  as_tibble(NewMelones_fnf_nextmostrecent)
}
as_tibble(NewMelones_fnf_mostrecent)
as_tibble(NewMelones_fnf_nextmostrecent)


{
  NewMelones_fnf_dlychnge <- inner_join(NewMelones_fnf_mostrecent, NewMelones_fnf_nextmostrecent, by = "res") 
  as_tibble(NewMelones_fnf_dlychnge)
  NewMelones_fnf_dlychnge <- NewMelones_fnf_dlychnge %>% mutate(value = value.x - value.y)
  as_tibble(NewMelones_fnf_dlychnge)
  NewMelones_fnf_dlychnge <- NewMelones_fnf_dlychnge %>% transmute(res, value, nws_id = nws_id.x, usbr_web_param = usbr_web_param.y, unit = unit.x)
  NewMelones_fnf_dlychnge <- NewMelones_fnf_dlychnge %>% mutate(date = date_mostrecent)
  NewMelones_fnf_dlychnge$usbr_web_param <- gsub("nextlatest", "dailychange", NewMelones_fnf_dlychnge$usbr_web_param ) 
  
  
  as_tibble(NewMelones_fnf_dlychnge)
  
  ## add date column
  
  NewMelones_fnf_mostrecent <- NewMelones_fnf_mostrecent %>% mutate(date = date_mostrecent)
  NewMelones_fnf_nextmostrecent <- NewMelones_fnf_nextmostrecent %>% mutate(date = date_nextmostrecent)
  NewMelones_fnf_dlychnge <- NewMelones_fnf_dlychnge %>% mutate(date = date_mostrecent)
  
  NewMelones_fnf  <- rbind(NewMelones_fnf_mostrecent, NewMelones_fnf_nextmostrecent, NewMelones_fnf_dlychnge)
  NewMelones_fnf_dlychnge$res <- gsub("NewMelones_stor", "NewMelones_stor", NewMelones_fnf_dlychnge$res )
  rm(NewMelones_fnf_mostrecent, NewMelones_fnf_nextmostrecent, NewMelones_fnf_dlychnge)
  as_tibble(NewMelones_fnf)
  
  
  
  ## capacities (from various sources online, including wiki) ##
  
  
}

{
  res_id_nws <- c("DLLC1", "BESC1", "NMSC1")
  res_cap <- c(56.83, 97.8, 2400)  #taf
  res_cap <- data.frame(res_id_nws, res_cap)
  rm(res_id_nws)
  as_tibble(res_cap)
}


}
