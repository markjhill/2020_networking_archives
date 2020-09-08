#Pull top feautres

#setwd("code/work/")
options(stringsAsFactors = FALSE)
'%!in%' <- function(x,y)!('%in%'(x,y))

library(quanteda)
library(stringr)
library(ggplot2)

#For 10 year chunks
#dfm_files <- list.files("../../data/work/netowrking_archives/dfm/", full.names = TRUE)
dfm_files <- list.files("../ESTC_SNA_data_creation/data/work/netowrking_archives/com_dfm_titles/", full.names = TRUE)
#dfm_files <- dfm_files[-c(1:159)]
#dfm_files <- dfm_files[-c(267:length(dfm_files))] #update this if I can get the last few -1730

eras <- unique(str_extract(dfm_files, "\\d\\d\\d\\d-\\d\\d\\d\\d"))

#stop words
latin_stopwords <- read.csv("../ESTC_SNA_data_creation/data/raw/latin.stopwords.clean", stringsAsFactors = FALSE)
extra_stopwords <- c("shall", "may", "one", "us", "hath", "yet", "upon", "yet", "now", "said", "unto", "thy", "doe", "first", "must",
                     "much", "made", "many", "make", "also", "without", "can", "thou", "like", "can", "though", "therefore", "without",
                     "two", "things", "might", "way", "say", "day", "let", "well", "things", "take", "owne", "doth", "i.e", "tis", "page removed",
                     #THIS IS IMPORTANT!
                     "non-latin", "alphabet", "saith", "thus", "|", "¦", "⁻", "vol", "year", "printed", "published",
                     "na", "amp", "ye", "anno", "translated", "vpon", "vnto", "yeere", "haue", "three", "second", "thereof", "yeare", "written", "mr",
                     "wherein", "written", "esq", "author", "part", "dr", "volumes", "1800", "volume", "mrs", "m.d", "d.d")

#Get top features per era




#Get keyness per community, export png of each community, and save CSV for each era

for(i_era in 1:length(eras)) {
  cat("\r", eras[i_era])
  
  temp_files <- dfm_files[grep(eras[i_era], dfm_files)]
  temp_dir <- paste0("outputs/keyness/", eras[i_era], "/")
  
  dir.create(temp_dir, showWarnings = FALSE)
  temp_dfm <- readRDS(temp_files)
  temp_dfm <- dfm_select(temp_dfm, pattern = c(stopwords("english"), stopwords("french"), latin_stopwords$a, extra_stopwords), selection = "remove", valuetype = "fixed")
  com_num <- unique(temp_dfm@docvars$Community)
  
  #One era only has one community (1506-1515), so need to skip it
  if(length(com_num) == 1) { next }
  
  temp_df_key_plus <- data.frame(matrix(data = NA, nrow = 100, ncol = length(com_num)))
  temp_df_key_minus <- data.frame(matrix(data = NA, nrow = 100, ncol = length(com_num)))
  
  for (i_com_keyness in 1:length(com_num)) {
    tstat_key <- textstat_keyness(temp_dfm, 
                                  target = temp_dfm@docvars$Community == paste0("Community_", i_com_keyness))
    temp_plot <- textplot_keyness(tstat_key, n = 20)
    ggsave(paste0(temp_dir, "community_", i_com_keyness, ".png"), temp_plot)
    
    temp_df_key_plus[,i_com_keyness] <- tstat_key$feature[1:100]
    temp_df_key_minus[,i_com_keyness] <- tstat_key$feature[nrow(tstat_key):(nrow(tstat_key)-99)]
    
    write.csv(temp_df_key_plus, file = paste0("outputs/keyness/tstat_", eras[i_era], "_plus.csv"))
    write.csv(temp_df_key_minus, file = paste0("outputs/keyness/tstat_", eras[i_era], "_minus.csv"))
  }
}


  