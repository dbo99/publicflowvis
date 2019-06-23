


{
#rm(list = ls()) 
rm(list= ls()[!(ls() %in% mainkeepers)])
#setwd("~/R/proj/publicflowscrapeapp")
setwd("~/Documents/publicflowvis")
source("libs.r")



{
res <- c("PineFlat")
coe_coe_text <- "http://www.spk-wc.usace.army.mil/fcgi-bin/midnight.py?days=0&report=COE"
coe_coe_text <- html_text(html_node(read_html(coe_coe_text),".h2 , pre"))
value <- "Pine Flat Natural Flow (.*?) Below N. F. nr Trimmer"
value <- regmatches(coe_coe_text,regexec(value, coe_coe_text))
value <- value[[1]][2]
value <- gsub("cfs):", "", value)
value <- gsub("\n", "", value)
value <- gsub("\\(", "", value ) %>% trimws() %>% as.numeric()
# https://stackoverflow.com/questions/9449466/remove-parenthesis-from-string
nwsid <- c("pftc1")
param <- c("fullnaturalflow")
source <- c("corps")
timeseq <- c("latest")  #latest
meastype <- c("meandly")
unit <- ("cfs")
#date <- "Data Ending 2400 hours (.*?) Report Generated"
#date <- regmatches(coe_coe_text,regexec(date, coe_coe_text))
#date <- date[[1]][2]
#date <- gsub("\n", "", date) %>% trimws()
#date <- dmy(date) %>% as.Date()                   ### gets previous data ending date

date <- "Report Generated (.*?) @"
date <- regmatches(coe_coe_text,regexec(date, coe_coe_text)) 
date <- date[[1]][2] 
date <- dmy(date) %>% as.Date()   #gets report date


pineflat_fnf_0 <- cbind(res, value, nwsid, param, source, timeseq, meastype, unit) %>% data.frame() %>% mutate(date = date)
#pineflat_fnf <- data.frame(pineflat_fnf) %>% mutate(date = as.Date(date))
as_tibble(pineflat_fnf_0)
}

{
coe_coe_text <- "http://www.spk-wc.usace.army.mil/fcgi-bin/midnight.py?days=1&report=COE"  #note `days = 1` (ago)
coe_coe_text <- html_text(html_node(read_html(coe_coe_text),".h2 , pre"))
value <- "Pine Flat Natural Flow (.*?) Below N. F. nr Trimmer"
value <- regmatches(coe_coe_text,regexec(value, coe_coe_text))
value <- value[[1]][2]
value <- gsub("cfs):", "", value)
value <- gsub("\n", "", value)
value <- gsub("\\(", "", value ) %>% trimws() %>% as.numeric()
nwsid <- c("pftc1")
param <- c("fullnaturalflow")
source <- c("corps")
timeseq <- c("nextlatest")  #nextlatest
meastype <- c("meandly")
unit <- ("cfs")
date <- "Report Generated (.*?) @"
date <- regmatches(coe_coe_text,regexec(date, coe_coe_text)) 
date <- date[[1]][2] 
date <- dmy(date) %>% as.Date()   #gets report date

pineflat_fnf_1 <- cbind(res, value, nwsid, param, source, timeseq, meastype, unit) %>% data.frame() %>% mutate(date = date)

pineflat_fnf <- rbind(pineflat_fnf_0, pineflat_fnf_1) %>% mutate(value = as.character(value), value = as.double(value))
as_tibble(pineflat_fnf)

latestval <- pineflat_fnf %>% filter(timeseq == "latest") %>% select(value)
nextlatestval <- pineflat_fnf %>% filter(timeseq == "nextlatest") %>% select(value)
dailychange <- latestval - nextlatestval

pineflat_fnf_change <- pineflat_fnf %>% filter(timeseq == "latest") %>% mutate(timeseq = "dailychange")
pineflat_fnf_change$value <- dailychange

pineflat_fnf <- rbind(pineflat_fnf, pineflat_fnf_change)
pineflat_fnf <- data.frame(pineflat_fnf) %>% mutate(value = unlist(value)) %>% select(-source) %>% rename_all(paste0, "_corps") %>% 
  mutate(nwsid = nwsid_corps) %>% select(-nwsid_corps)

rm(list= ls()[!(ls() %in% mainkeepers)])
as_tibble(pineflat_fnf)
}

}
