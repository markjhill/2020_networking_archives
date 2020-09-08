#create DFMs of corpora

setwd("code/work/")
options(stringsAsFactors = FALSE)
'%!in%' <- function(x,y)!('%in%'(x,y))

#more than plots - get sankey data.
library(quanteda)
library(stringr)

latin_stopwords <- read.csv("../../data/raw/latin.stopwords.clean", stringsAsFactors = FALSE)
extra_stopwords <- c("shall", "may", "one", "us", "hath", "yet", "upon", "yet", "now", "said", "unto", "thy", "doe", "first", "must",
                     "much", "made", "many", "make", "also", "without", "can", "thou", "like", "can", "though", "therefore", "without",
                     "two", "things", "might", "way", "say", "day", "let", "well", "things", "take", "owne", "doth", "i.e", "tis", "page removed",
                     #THIS IS IMPORTANT!
                     "non-latin", "alphabet", "saith", "thus", "|", "¦", "vol", "year", "printed", "published")


#corp_files <- list.files("../../data/work/netowrking_archives/corpora", full.names = TRUE)
corp_files <- list.files("../../data/work/netowrking_archives/corpora_6_years", full.names = TRUE)

for(i_dfm in 616:length(corp_files)) {
  era <- str_extract(corp_files[i_dfm], "\\d\\d\\d\\d-\\d\\d\\d\\d.+.rds")
  era <- gsub(".rds", "", era)
  cat("\nDFM ", i_dfm, "of", length(corp_files), "-", era)
  temp_corp <- readRDS(corp_files[i_dfm])
  
  
  temp_corp <- tokens(temp_corp, remove_punct = TRUE, remove_numbers = TRUE)
  temp_corp <- tokens_select(temp_corp, pattern = c(stopwords('en'), "amp", extra_stopwords, letters, latin_stopwords$a), selection = 'remove')
  temp_dfm <-  dfm(temp_corp, remove_punct = TRUE, verbose = TRUE)
  
  rm(temp_corp)
  
  #saveRDS(temp_dfm, file = paste0("../../data/work/netowrking_archives/dfm/dfm_", era, ".rds"))
  saveRDS(temp_dfm, file = paste0("../../data/work/netowrking_archives/dfm_6_years/dfm_", era, ".rds"))
  rm(temp_dfm)
  gc()
}
