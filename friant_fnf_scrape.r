

{
library(pdftools)
file <- "https://www.usbr.gov/mp/cvo/vungvari/milfln.pdf"
#file <- "https://www.usbr.gov/mp/cvo/vungvari/trndop.pdf"
rawtext  <- pdf_text(file)

start <- "\r\n    1"
end <- "\r\n  TOTALS"
friant_fnf <- read.table(text=substring(rawtext, regexpr(start, rawtext), regexpr(end, rawtext)))
as_tibble(friant_fnf)
nrow_friant_fnf <- nrow(friant_fnf)

friant_fnf <- friant_fnf [nrow_friant_fnf,]
as_tibble(friant_fnf)
friant_fnf  <- friant_fnf %>% rename("monthday" = !!names(.[1]),
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
                                             "friant_dlycfschange_usbr" = !!names(.[12]),
                                             "friant_observedinflow_cfs_usbr" = !!names(.[13]),
                                             "friant_natriver_fnf" = !!names(.[14])) %>% select(-monthday, -V15)
as_tibble(friant_fnf)


}

friant_fnf_t <- friant_fnf %>% gather(key = "res", value = "value")


## capacities (from various sources online, including wiki) ##

res_id_nws <- c("TAEC1", "FLEC1", "HNTC1", "SAVC1", "MPLC1", "RGRC1", "BASC1", "KRHC1")
res_cap <- c(125.0, 64.6, 88.834, 135.283, 123.0, 35.0, 45.4, 4.252)
res_cap <- data.frame(res_id_nws, res_cap)



