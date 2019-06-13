

{
library(pdftools)
file <- "https://www.usbr.gov/mp/cvo/vungvari/milfln.pdf"
rawtext  <- pdf_text(file)

start <- "\r\n    1"
end <- "\r\n  TOTALS"
friant_fnf <- read.table(text=substring(rawtext, regexpr(start, rawtext), regexpr(end, rawtext)))
as_tibble(friant_fnf)
length_friant_fnf <- nrow(friant_fnf)

friant_fnf <- df[length_friant_fnf,]
as_tibble(friant_fnf)
friant_fnf  <- friant_fnf %>%         rename("monthday" = !!names(.[1]),
                                              "edison_lake_taf_usbr" = !!names(.[2]),
                                             "florence_lake_taf_usbr" = !!names(.[3]),
                                             "huntington_lake_taf_usbr" = !!names(.[4]),
                                             "shaver_lake_taf_usbr" = !!names(.[5]),
                                             "mammoth_pool_taf_usbr" = !!names(.[6]) ,
                                             "redinger_lake_taf_usbr" = !!names(.[7]),
                                             "crane_valley_taf_usbr" = !!names(.[8]),
                                             "kerckhoff_lake_taf_usbr" = !!names(.[9]),
                                             "friant_taf_usbr" = !!names(.[10])  ,                                          
                                             "friant_change_taf_usbr" = !!names(.[11]),
                                             "friant_change_cfs_usbr" = !!names(.[12]),
                                             "friant_observedinflow_cfs_usbr" = !!names(.[13]),
                                             
                                             "friant_natural_river_fnf" = !!names(.[14])) %>% select(-monthday)
as_tibble(friant_fnf)

}
