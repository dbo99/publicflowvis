

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

## rename most recent ## 
friant_fnf_mostrecent  <- friant_fnf_mostrecent %>% rename("monthday" = !!names(.[1]),
                                              "edison_stor_usbr" = !!names(.[2]),
                                             "florence_stor_usbr" = !!names(.[3]),
                                             "huntington_stor_usbr" = !!names(.[4]),
                                             "shaver_stor_usbr" = !!names(.[5]),
                                             "mammothpool_stor_usbr" = !!names(.[6]) ,
                                             "redinger_stor_usbr" = !!names(.[7]),
                                             "cranevalley_stor_usbr" = !!names(.[8]),
                                             "kerckhoff_stor_usbr" = !!names(.[9]),
                                             "friant_stor_usbr" = !!names(.[10])  ,                                          
                                             "friant_dlystorchange_usbr" = !!names(.[11]),
                                             "friant_dlyinflowchange_usbr" = !!names(.[12]),
                                             "friant_observedinflow_cfs_usbr" = !!names(.[13]),
                                             "friant_natriver_fnf" = !!names(.[14])) %>% select(-monthday, -V15)
as_tibble(friant_fnf_mostrecent)

## rename next most recent ## 

friant_fnf_nextmostrecent  <- friant_fnf_nextmostrecent %>% rename("monthday" = !!names(.[1]),
                                                           "edison_stor_usbr" = !!names(.[2]),
                                                           "florence_stor_usbr" = !!names(.[3]),
                                                           "huntington_stor_usbr" = !!names(.[4]),
                                                           "shaver_stor_usbr" = !!names(.[5]),
                                                           "mammothpool_stor_usbr" = !!names(.[6]) ,
                                                           "redinger_stor_usbr" = !!names(.[7]),
                                                           "cranevalley_stor_usbr" = !!names(.[8]),
                                                           "kerckhoff_stor_usbr" = !!names(.[9]),
                                                           "friant_stor_usbr" = !!names(.[10])  ,                                          
                                                           "friant_dlystorchange_usbr" = !!names(.[11]),
                                                           "friant_dlyinflowchange_usbr" = !!names(.[12]),
                                                           "friant_observedinflow_cfs_usbr" = !!names(.[13]),
                                                           "friant_natriver_fnf" = !!names(.[14])) %>% select(-monthday, -V15)
as_tibble(friant_fnf_nextmostrecent)

###### convert mostrecent's to long format, add attributes######

friant_fnf_mostrecent_t <- friant_fnf_mostrecent %>% gather(key = "res", value = "value")
nws_id <- c("TAEC1", "FLEC1", "HNTC1", "SAVC1", "MPLC1", "RGRC1", "BASC1", "KRHC1", "FRAC1", 
                    "FRAC1", "FRAC1", "FRAC1", "FRAC1")
param <- c("stor_current", "stor_current", "stor_current", "stor_current", "stor_current", "stor_current",
           "stor_current", "stor_current", "stor_current", "stor_dlychnge", "meancfs_dlychnge", "meancfs_dlyinflowobs",
           "meancfs_fnf")

unit <- c("taf", "taf", "taf", "taf", "taf", "taf",
          "taf", "taf", "taf", "taf", "cfs", "cfs",
          "cfs")

friant_fnf_mostrecent <- cbind(friant_fnf_mostrecent_t, nws_id, param, unit)
as_tibble(friant_fnf_mostrecent)

friant_fnf_mostrecent$value <- gsub(",", "", friant_fnf_mostrecent$value )
friant_fnf_mostrecent$value <- gsub("+", "", friant_fnf_mostrecent$value ) #doesn't work, but below's as.numeric() seems to
as_tibble(friant_fnf_mostrecent)
friant_fnf_mostrecent <- friant_fnf_mostrecent %>% mutate(value = as.numeric(value))
as_tibble(friant_fnf_mostrecent)

###### convert mostrecent's to long format, add attributes######

friant_fnf_nextmostrecent_t <- friant_fnf_nextmostrecent %>% gather(key = "res", value = "value")
nws_id <- c("TAEC1", "FLEC1", "HNTC1", "SAVC1", "MPLC1", "RGRC1", "BASC1", "KRHC1", "FRAC1", 
            "FRAC1", "FRAC1", "FRAC1", "FRAC1")
param <- c("stor_current", "stor_current", "stor_current", "stor_current", "stor_current", "stor_current",
           "stor_current", "stor_current", "stor_current", "stor_dlychnge", "meancfs_dlychnge", "meancfs_dlyinflowobs",
           "meancfs_fnf")

unit <- c("taf", "taf", "taf", "taf", "taf", "taf",
          "taf", "taf", "taf", "taf", "cfs", "cfs",
          "cfs")
friant_fnf_nextmostrecent <- cbind(friant_fnf_nextmostrecent_t, nws_id, param, unit)
as_tibble(friant_fnf_nextmostrecent)

friant_fnf_nextmostrecent$value <- gsub(",", "", friant_fnf_nextmostrecent$value )
friant_fnf_nextmostrecent$value <- gsub("+", "", friant_fnf_nextmostrecent$value ) #doesn't work, but below's as.numeric() seems to
as_tibble(friant_fnf_nextmostrecent)
friant_fnf_nextmostrecent <- friant_fnf_nextmostrecent %>% mutate(value = as.numeric(value))
as_tibble(friant_fnf_nextmostrecent)


}
as_tibble(friant_fnf_mostrecent)
as_tibble(friant_fnf_nextmostrecent)

friant_fnf_nextmostrecent <- friant_fnf_nextmostrecent %>% mutate(prev_value = value) %>% 
                             transmute(res, prev_value)

as_tibble(friant_fnf_nextmostrecent)

df_diff <- left_join(friant_fnf_mostrecent, friant_fnf_nextmostrecent, by = "res")
as_tibble(df_diff)
df_diff <- df_diff %>% mutate(diff = value - prev_value)
as_tibble(df_diff)
## capacities (from various sources online, including wiki) ##

res_id_nws <- c("TAEC1", "FLEC1", "HNTC1", "SAVC1", "MPLC1", "RGRC1", "BASC1", "KRHC1")
res_cap <- c(125.0, 64.6, 88.834, 135.283, 123.0, 35.0, 45.4, 4.252)
res_cap <- data.frame(res_id_nws, res_cap)

bhnc1 <- c(151716)  #area

