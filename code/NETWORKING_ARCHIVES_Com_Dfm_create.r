#com dfm create

setwd("code/work/")
options(stringsAsFactors = FALSE)
'%!in%' <- function(x,y)!('%in%'(x,y))

#more than plots - get sankey data.
library(quanteda)
library(stringr)

# latin_stopwords <- read.csv("../../data/raw/latin.stopwords.clean", stringsAsFactors = FALSE)
# extra_stopwords <- c("shall", "may", "one", "us", "hath", "yet", "upon", "yet", "now", "said", "unto", "thy", "doe", "first", "must",
#                      "much", "made", "many", "make", "also", "without", "can", "thou", "like", "can", "though", "therefore", "without",
#                      "two", "things", "might", "way", "say", "day", "let", "well", "things", "take", "owne", "doth", "i.e", "tis", "page removed",
#                      #THIS IS IMPORTANT!
#                      "non-latin", "alphabet", "saith", "thus", "|", "¦", "vol", "year", "printed", "published")


#corp_files <- list.files("../../data/work/netowrking_archives/corpora", full.names = TRUE)
#corp_files <- list.files("../../data/work/netowrking_archives/corpora_6_years", full.names = TRUE)
corp_files <- list.files("../../data/work/netowrking_archives/corpora_titles", full.names = TRUE)

eras <- unique(str_extract(corp_files, "\\d\\d\\d\\d-\\d\\d\\d\\d"))


for(i_era_dfm in 1:length(eras)) {
  cat("\n", eras[i_era_dfm], "\n")
  temp_files <- corp_files[grep(eras[i_era_dfm], corp_files)]
  
  #load and combine corps
  for(i_corp in 1:length(temp_files)) {
    temp_corp <- readRDS(temp_files[i_corp])
    
    com_num <- gsub(paste0("../../data/work/netowrking_archives/corpora_titles/corp_", eras[i_era_dfm], "_"), "", temp_files[i_corp])
    com_num <- gsub(".rds", "", com_num)
    
    if(i_corp == 1) {
      docvars(temp_corp, "Community") <- paste0("Community_", com_num)
      docnames(temp_corp) <- paste0(com_num, "_", docnames(temp_corp))
      temp_com_corp <- temp_corp
    } else {
      docvars(temp_corp, "Community") <- paste0("Community_", com_num)
      docnames(temp_corp) <- paste0(com_num, "_", docnames(temp_corp))
      temp_com_corp <- temp_com_corp + temp_corp
    }
    
  }
  rm(temp_corp)
  
  total_era_dfm <- dfm(temp_com_corp, remove_punct = TRUE, verbose = TRUE)
  #saveRDS(total_era_dfm, file = paste0("../../data/work/netowrking_archives/com_dfm/com_dfm_", eras[i_era_dfm], ".rds"))
  saveRDS(total_era_dfm, file = paste0("../../data/work/netowrking_archives/com_dfm_titles/com_dfm_", eras[i_era_dfm], ".rds"))
  
  rm(temp_com_corp)
  rm(total_era_dfm)
  gc()
}

