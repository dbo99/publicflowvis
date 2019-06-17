
library(pdftools)
file <- "https://www.usbr.gov/mp/cvo/vungvari/shafln.pdf"
rawtext  <- pdf_text(file)
rawtext
start <- "\r\n    1"
end <- "\r\n  TOTALS"
shasta_fnf <- read.table(text=substring(rawtext, regexpr(start, rawtext), regexpr(end, rawtext)))
