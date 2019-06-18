


{
rm(list = ls()) 
setwd("~/R/proj/publicflowscrapeapp")
source("libs.r")

file <- "https://www.usbr.gov/mp/cvo/vungvari/shafln.pdf"
rawtext  <- pdf_text(file)
rawtext
month_year  <- str_match(rawtext, "CALIFORNIA\r\n(.*?)FULL")  #https://stackoverflow.com/questions/39086400/extracting-a-string-between-other-two-strings-in-r
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
                                                             "upstream_stor" = !!names(.[7]),
                                                             "upstream_dlystorchange" = !!names(.[8]),
                                                             "Shasta_observedinflowchange_meandaily" = !!names(.[9]),
                                                             "Shasta_observedinflow_meandaily" = !!names(.[10]),
                                                             "Shasta_natriver_fnf" = !!names(.[11])) %>% select(-V12) 

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
                                                                     "upstream_stor" = !!names(.[7]),
                                                                     "upstream_dlystorchange" = !!names(.[8]),
                                                                     "Shasta_observedinflowchange_meandaily" = !!names(.[9]),
                                                                     "Shasta_observedinflow_meandaily" = !!names(.[10]),
                                                                     "Shasta_natriver_fnf" = !!names(.[11])) %>% select(-V12, -monthday) 
}


as_tibble(shasta_fnf_nextmostrecent)
as_tibble(shasta_fnf_mostrecent)
###### convert mostrecent's to long format, add attributes######
#{
  shasta_fnf_mostrecent_t <- shasta_fnf_mostrecent %>% gather(key = "res", value = "value")
  nws_id <- c("BTOC1", "MCZC1", "ICYC1", "PTXC1", "PTZC1", "SHDC1", "SHDC1", "SHDC1", "SHDC1", 
              "SHDC1")
  usbr_web_param <- c("stor_latest_cvopublished", "stor_latest_cvopublished", "stor_latest_cvopublished", 
                      "stor_latest_cvopublished", "stor_latest_cvopublished", "stor_latest_cvopublished",
                      "stor_dlychnge", "meancfs_dlychnge", 
                      "meancfs_dlyinflowobs_latest_cvopublished",
                      "meancfs_fnf_latest_cvopublished")
  
  unit <-  c("taf", "taf", "taf", "taf", "taf", "taf", "taf", "taf","cfs", "cfs")
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
    filter(usbr_web_param != "stor_dlychnge") %>% filter(usbr_web_param != "meancfs_dlychnge")
  as_tibble(shasta_fnf_mostrecent)
}
###### convert mostrecent's to long format, add attributes######

{
  shasta_fnf_nextmostrecent_t <- shasta_fnf_nextmostrecent %>% gather(key = "res", value = "value")
  nws_id <- c("BTOC1", "MCZC1", "ICYC1", "PTXC1", "PTZC1", "SHDC1", "SHDC1", "SHDC1", "SHDC1", 
              "SHDC1")
  usbr_web_param <- c("stor_latest_cvopublished", "stor_latest_cvopublished", "stor_latest_cvopublished", 
                      "stor_latest_cvopublished", "stor_latest_cvopublished", "stor_latest_cvopublished",
                      "stor_dlychnge", "meancfs_dlychnge", 
                      "meancfs_dlyinflowobs_latest_cvopublished",
                      "meancfs_fnf_latest_cvopublished")
  
  unit <-  c("taf", "taf", "taf", "taf", "taf", "taf", "taf", "taf","cfs", "cfs")
  shasta_fnf_nextmostrecent <- cbind(shasta_fnf_nextmostrecent_t, nws_id, usbr_web_param, unit)
  rm(shasta_fnf_nextmostrecent_t, unit, usbr_web_param, nws_id)
  as_tibble(shasta_fnf_nextmostrecent)
  
  shasta_fnf_nextmostrecent$value <- gsub(",", "", shasta_fnf_nextmostrecent$value )
  shasta_fnf_nextmostrecent$value <- gsub("+", "", shasta_fnf_nextmostrecent$value ) #doesn't work, but below's as.numeric() seems to
  as_tibble(shasta_fnf_nextmostrecent)
  shasta_fnf_nextmostrecent <- shasta_fnf_nextmostrecent %>% mutate(value = as.numeric(value)) 
  as_tibble(shasta_fnf_nextmostrecent)
  shasta_fnf_nextmostrecent <- shasta_fnf_nextmostrecent %>%
    filter(usbr_web_param != "stor_dlychnge") %>% filter(usbr_web_param != "meancfs_dlychnge")
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
  shasta_fnf_dlychnge$res <- gsub("Shasta_stor", "Shasta_stor", shasta_fnf_dlychnge$res )
  rm(shasta_fnf_mostrecent, shasta_fnf_nextmostrecent, shasta_fnf_dlychnge)
  as_tibble(shasta_fnf)
  
  
  
  ## capacities (from various sources online, including wiki) ##
  
  
}

{
  res_id_nws <- c("BTOC1", "MCZC1", "ICYC1", "PTXC1", "PTZC1", "SHDC1")
  res_cap <- c(34.6, 35.2, 24.3, 15.7, 34.1, 4552)  #taf
  res_cap <- data.frame(res_id_nws, res_cap)
  rm(res_id_nws)
  as_tibble(res_cap)
}



