rm(list = ls()) 

#{

{
library(pdftools)
file <- "https://www.usbr.gov/mp/cvo/vungvari/milfln.pdf"
rawtext  <- pdf_text(file)

start <- "\r\n    1"
end <- "\r\n  TOTALS"
friant_fnf <- read.table(text=substring(rawtext, regexpr(start, rawtext), regexpr(end, rawtext)))
rm(start, end)
as_tibble(friant_fnf)
nrow_friant_fnf <- nrow(friant_fnf)

friant_fnf_mostrecent <- friant_fnf[nrow_friant_fnf,]  #most recent row from usbr
friant_fnf_nextmostrecent <- friant_fnf[nrow_friant_fnf - 1,]  #next most recent row from usbr
as_tibble(friant_fnf_mostrecent)
as_tibble(friant_fnf_nextmostrecent)
rm(friant_fnf)

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
                                             "Millerton_stor_usbr" = !!names(.[10])  ,                                          
                                             "Millerton_dlystorchange_usbr" = !!names(.[11]),
                                             "Millerton_dlyinflowchange_usbr" = !!names(.[12]),
                                             "Millerton_observedinflow_cfs" = !!names(.[13]),
                                             "Millerton_natriver_fnf" = !!names(.[14])) %>% select(-monthday, -V15)
as_tibble(friant_fnf_mostrecent)

## rename next most recent ## 

friant_fnf_nextmostrecent  <- friant_fnf_nextmostrecent %>% rename("monthday" = !!names(.[1]),
                                                                   "Edison" = !!names(.[2]),
                                                                   "Florence" = !!names(.[3]),
                                                                   "Huntington" = !!names(.[4]),
                                                                   "Shaver" = !!names(.[5]),
                                                                   "MammothPool" = !!names(.[6]) ,
                                                                   "Redinger" = !!names(.[7]),
                                                                   "CraneValley" = !!names(.[8]),
                                                                   "Kerckhoff" = !!names(.[9]),
                                                                   "Millerton_stor_usbr" = !!names(.[10])  ,                                          
                                                                   "Millerton_dlystorchange_usbr" = !!names(.[11]),
                                                                   "Millerton_dlyinflowchange_usbr" = !!names(.[12]),
                                                                   "Millerton_observedinflow_cfs" = !!names(.[13]),
                                                                   "Millerton_natriver_fnf" = !!names(.[14])) %>% select(-monthday, -V15)
as_tibble(friant_fnf_mostrecent)
}
###### convert mostrecent's to long format, add attributes######
{
friant_fnf_mostrecent_t <- friant_fnf_mostrecent %>% gather(key = "res", value = "value")
nws_id <- c("TAEC1", "FLEC1", "HNTC1", "SAVC1", "MPLC1", "RGRC1", "BASC1", "KRHC1", "FRAC1", 
                    "FRAC1", "FRAC1", "FRAC1", "FRAC1")
usbr_web_param <- c("stor_latest_cvopublished", "stor_latest_cvopublished", "stor_latest_cvopublished", "stor_latest_cvopublished", "stor_latest_cvopublished", "stor_latest_cvopublished",
           "stor_latest_cvopublished", "stor_latest_cvopublished", "stor_latest_cvopublished", "stor_dlychnge", "meancfs_dlychnge", "meancfs_dlyinflowobs_latest_cvopublished",
           "meancfs_fnf_latest_cvopublished")

unit <- c("taf", "taf", "taf", "taf", "taf", "taf",
          "taf", "taf", "taf", "taf", "cfs", "cfs",
          "cfs")

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
                        filter(usbr_web_param != "stor_dlychnge") %>% filter(usbr_web_param != "meancfs_dlychnge")
as_tibble(friant_fnf_mostrecent)
}
###### convert mostrecent's to long format, add attributes######

{
friant_fnf_nextmostrecent_t <- friant_fnf_nextmostrecent %>% gather(key = "res", value = "value")
nws_id <- c("TAEC1", "FLEC1", "HNTC1", "SAVC1", "MPLC1", "RGRC1", "BASC1", "KRHC1", "FRAC1", 
            "FRAC1", "FRAC1", "FRAC1", "FRAC1")
usbr_web_param <- c("stor_nextlatest_cvopublished", "stor_nextlatest_cvopublished", "stor_nextlatest_cvopublished", "stor_nextlatest_cvopublished", "stor_nextlatest_cvopublished", "stor_nextlatest_cvopublished",
           "stor_nextlatest_cvopublished", "stor_nextlatest_cvopublished", "stor_nextlatest_cvopublished", "stor_dlychnge", "meancfs_dlychnge", "meancfs_dlyinflowobs_nextlatest_cvopublished",
           "meancfs_fnf_nextlatest_cvopublished")

unit <- c("taf", "taf", "taf", "taf", "taf", "taf",
          "taf", "taf", "taf", "taf", "cfs", "cfs",
          "cfs")
friant_fnf_nextmostrecent <- cbind(friant_fnf_nextmostrecent_t, nws_id, usbr_web_param, unit)
rm(friant_fnf_nextmostrecent_t, unit, usbr_web_param, nws_id)
as_tibble(friant_fnf_nextmostrecent)

friant_fnf_nextmostrecent$value <- gsub(",", "", friant_fnf_nextmostrecent$value )
friant_fnf_nextmostrecent$value <- gsub("+", "", friant_fnf_nextmostrecent$value ) #doesn't work, but below's as.numeric() seems to
as_tibble(friant_fnf_nextmostrecent)
friant_fnf_nextmostrecent <- friant_fnf_nextmostrecent %>% mutate(value = as.numeric(value)) 
as_tibble(friant_fnf_nextmostrecent)
friant_fnf_nextmostrecent <- friant_fnf_nextmostrecent %>%
                             filter(usbr_web_param != "stor_dlychnge") %>% filter(usbr_web_param != "meancfs_dlychnge")
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
friant_fnf_dlychnge$usbr_web_param <- gsub("nextlatest", "dailychange", friant_fnf_dlychnge$usbr_web_param )

as_tibble(friant_fnf_dlychnge)


friant_fnf  <- rbind(friant_fnf_mostrecent, friant_fnf_nextmostrecent, friant_fnf_dlychnge)
friant_fnf_dlychnge$res <- gsub("Millerton_stor_usbr", "Millerton_stor", friant_fnf_dlychnge$res )
friant_fnf_dlychnge$res <- gsub("Millerton_observedinflow_cfs", "Millerton_inflow", friant_fnf_dlychnge$res )
rm(friant_fnf_mostrecent, friant_fnf_nextmostrecent, friant_fnf_dlychnge)
as_tibble(friant_fnf)



## capacities (from various sources online, including wiki) ##


}

res_id_nws <- c("TAEC1", "FLEC1", "HNTC1", "SAVC1", "MPLC1", "RGRC1", "BASC1", "KRHC1")
res_cap <- c(125.0, 64.6, 88.834, 135.283, 123.0, 35.0, 45.4, 4.252)
res_cap <- data.frame(res_id_nws, res_cap)
rm(res_id_nws)
as_tibble(res_cap)
#}
