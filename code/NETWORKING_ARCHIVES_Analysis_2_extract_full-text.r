#Extract full text

setwd("code/")

options(stringsAsFactors = FALSE)
'%!in%' <- function(x,y)!('%in%'(x,y))

library(stringr)
library(neo4r)
library(igraph)
library(ggplot2)
library(ggalluvial)
library(dplyr)
library(purrr)
library(igraph)
library(stringr)
library(RColorBrewer)
library(neo4r)
library(ggplot2)
library(ggalluvial)
library(tibble)
library(PGRdup)
library(quanteda)


con <- neo4j_api$new(
  url = "http://localhost:7474",
  user = "neo4j", 
  password = "1234"
)

latin_stopwords <- read.csv("../../ESTC_SNA_data_creation/data/raw/latin.stopwords.clean", stringsAsFactors = FALSE)
extra_stopwords <- c("shall", "may", "one", "us", "hath", "yet", "upon", "yet", "now", "said", "unto", "thy", "doe", "first", "must",
                     "much", "made", "many", "make", "also", "without", "can", "thou", "like", "can", "though", "therefore", "without",
                     "two", "things", "might", "way", "say", "day", "let", "well", "things", "take", "owne", "doth", "i.e", "tis", "page removed",
                     #THIS IS IMPORTANT!
                     "non-latin", "alphabet", "saith", "thus", "|", "¦", "⁻", "vol", "year", "printed", "published",
                     "na", "amp", "ye", "anno", "translated", "vpon", "vnto", "yeere", "haue", "three", "second", "thereof", "yeare", "written", "mr",
                     "wherein", "written", "esq", "author", "part", "dr", "volumes", "1800", "volume", "mrs", "m.d", "d.d", "b.d", "▪")

#era

era <- "1646-1655"
era <- "1651-1660"
era <- "1776-1785"
era <- "1761-1770"
era <- "1796-1805"

#community

comms <- c(1,2,9)
comms <- c(20)

#Load and create DFMs

temp_dir <- paste0("../../ESTC_SNA_data_creation/data/work/netowrking_archives/community_data/", era, "/")
temp_files <- list.files(temp_dir, full.names = TRUE)
actors <- readRDS(paste0(temp_dir, "actors.rds"))
actors <- actors[which(actors$community %in% comms),]
temp_files <- paste0("../../ESTC_SNA_data_creation/data/work/netowrking_archives/community_data/", era, "/comm_", comms, ".rds")

for(i in 1:length(temp_files)) {
  
  cat("\nCommunity:", i, "\n")
  
  temp_file_name <- paste0("../../ESTC_SNA_data_creation/data/work/netowrking_archives/full_text_comms/dfm_", era, "_", comms[i], ".rds")
  

  temp_comm <- readRDS(temp_files[i])
  assign(paste0("comm_", era, "_", comms[i]), temp_comm)
  
  if(file.exists(temp_file_name)) {
    temp_dfm <- readRDS(temp_file_name)
    assign(paste0("dfm_", era, "_", comms[i]), temp_dfm)
    cat("\nDFM exists, skippig to next\n")
    next
  }
  
  temp_estcs <- temp_comm$estc_ids
  temp_estcs_filepath <- paste0("../../eebo_ecco_full_text/", temp_estcs, ".txt")
  temp_estcs <- temp_estcs[which(file.exists(temp_estcs_filepath) == TRUE)]
  temp_estcs_filepath <- temp_estcs_filepath[which(file.exists(temp_estcs_filepath) == TRUE)]
  
  #build corpora
  temp_df <- temp_comm[,-c(4)]
  
  temp_df$text <- paste0(temp_df$title_1, temp_df$title_2, sep = " ")
  
  temp_df$ft <- FALSE
  temp_df$ft[which(temp_df$estc_ids %in% temp_estcs)] <- TRUE
  
  temp_work_ids <- temp_df$work_id[which(duplicated(temp_df$work_id))]
  temp_work_ids <- unique(temp_work_ids)
  if(any(is.na(temp_work_ids))) {
    temp_work_ids <- temp_work_ids[-c(which(is.na(temp_work_ids)))]
  }
  
  for(i_work_check in 1:length(temp_work_ids)) {
    #for(i_work_check in 1:6) {
    cat("\r", i_work_check)
    
    temp_locs <- which(temp_df$work_id == temp_work_ids[i_work_check])
    temp_ft_locs <- temp_locs[which(temp_df$ft[temp_locs] == TRUE)]
    if(length(temp_ft_locs) == 0) {
      temp_df <- temp_df[-c(temp_locs[-(which.max(nchar(temp_df$text[temp_locs])))]),]
    } else {
      temp_locs <- temp_locs[-c(which(temp_locs == temp_ft_locs[1]))]
      temp_df <- temp_df[-c(temp_locs),]
    }
    if(nrow(temp_df) == 0) {
      cat("ERROR", i_work_check)
      break
    }
  }
  
  if(length(which(temp_estcs %!in% temp_df$estc_ids)) > 0) {
    to_remove <- which(temp_estcs %!in% temp_df$estc_ids)
    temp_estcs <- temp_estcs[-c(to_remove)]
    temp_estcs_filepath <- temp_estcs_filepath[-c(to_remove)]
  }
  
  for(i_full_text in 1:length(temp_estcs)) {
    cat("\r", i_full_text)
    temp_df$text[which(temp_df$estc_ids == temp_estcs[i_full_text])] <- readr::read_file(temp_estcs_filepath[i_full_text])
  }
  temp_corp <- corpus(temp_df, docid_field = "estc_ids", text_field = "text")
  
  temp_dfm <- dfm(temp_corp, verbose = TRUE, remove_punct = TRUE)
  temp_dfm <- dfm_select(temp_dfm, pattern = c(stopwords("english"), stopwords("french"), stopwords("italian"), stopwords("spanish"), latin_stopwords$a, extra_stopwords), selection = "remove", valuetype = "fixed")
  temp_dfm <- dfm_select(temp_dfm, pattern = c("\\d\\d\\d\\d\\d", "\\d\\d\\d\\d", "\\d\\d\\d", "\\d\\d", "\\d"), selection = "remove", valuetype = "regex")
  temp_dfm <- dfm_trim(temp_dfm, min_termfreq = 10, min_docfreq = 2)
  
  saveRDS(temp_dfm, file = temp_file_name)
  
  assign(paste0("dfm_", era, "_", comms[i]), temp_dfm)
  
  rm(temp_df)
  rm(temp_dfm)
  rm(temp_comm)
  rm(temp_corp)
  rm(temp_estcs)
  rm(temp_estcs_filepath)
  gc()
  
}

#Analyse DFMs

topfeatures(`dfm_1646-1655_1`, n =25)
topfeatures(`dfm_1646-1655_10`, n =25)
topfeatures(`dfm_1646-1655_12`, n =25)
topfeatures(`dfm_1646-1655_2`, n =25)
topfeatures(`dfm_1646-1655_9`, n =25)

topfeatures(`dfm_1646-1655_1`, n =25)
topfeatures(`dfm_1646-1655_10`, n =25)
topfeatures(`dfm_1646-1655_12`, n =25)
topfeatures(`dfm_1646-1655_2`, n =25)
topfeatures(`dfm_1646-1655_9`, n =25)


#check simil

keywords <- c("quakers")
era <- "1651-1660"

#community

#comms <- c(2,9,1,10,12)
comms <- c(1,10,12,5)

keywords <- c("wealth")

#for(i in 1:length(temp_files)) {
for(i in c(1,4)) {
  temp_dfm <- get(paste0("dfm_", era, "_", comms[i]))
  temp_dfm[, c(keywords)]
  tstat_temp <- textstat_simil(temp_dfm,  temp_dfm[, c(keywords)], method = "cosine", margin = "features")
  print(paste0("dfm_", era, "_", comms[i]))
  print(as.list(tstat_temp, n = 20))
  rm(temp_dfm)
  rm(tstat_temp)
}




write.csv(`comm_1646-1655_9`, file = "../outputs_for_presentation/1646-1655_9.csv")
