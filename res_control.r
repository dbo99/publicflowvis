

rm(list = ls()) 
{
mainkeepers <- c("shasta_fnf", "friant_fnf", "newmelones_fnf", "pineflat_fnf", "cdec_res_table", "mainkeepers", "usbr")
rm(list= ls()[!(ls() %in% mainkeepers)])
setwd("~/Documents/publicflowvis")
source("libs.r")
source("fun_defs.r")
}

#### full natural flow ####
# usbr
source("shasta_fnf_scrape.r")
source("friant_fnf_scrape.r")
source("newmelones_fnf_scrape.r")
# corps
source("pineflat_fnf_scrape.r")

usbr <- rbind(shasta_fnf, friant_fnf, newmelones_fnf) 

as_tibble(usbr)


corps <- pineflat_fnf 
as_tibble(corps)


###  reservoir dailies ###
# cdec
source("cdec_res_table_scrape.r")

as_tibble(cdec_res_table)




df <- rbind(shasta_fnf, pineflat_fnf, newmelones_fnf, friant_fnf, cdec_res_table)

