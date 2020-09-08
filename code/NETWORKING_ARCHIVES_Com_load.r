#NETWORKING ARCHIVES - Load eras to compare

setwd("code/work/")
options(stringsAsFactors = FALSE)
'%!in%' <- function(x,y)!('%in%'(x,y))

#more than plots - get sankey data.
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
library(neo4r)

con <- neo4j_api$new(
  url = "http://localhost:7474",
  user = "neo4j", 
  password = "1234"
)


#########
#Get files

#which yers to include
start_year <- 1499
finish_year <- 1801

#files <- list.files("../../data/work/netowrking_archives/community_data", recursive = TRUE, full.names = TRUE)
files <- list.files("../../data/work/netowrking_archives/community_data_6_years/", recursive = TRUE, full.names = TRUE)
file_years <- str_extract(files, "\\d\\d\\d\\d")
files <- files[c(intersect(which(as.numeric(file_years) >= start_year), which(as.numeric(file_years) < finish_year)))]
rm(file_years)
rm(start_year)
rm(finish_year)

#load all

year_groups <- unique(str_extract(files, "\\d\\d\\d\\d-\\d\\d\\d\\d"))

for(i_year_groups in 1:length(year_groups)) {
  cat("\r", i_year_groups)
  temp_files <- files[c(grep(year_groups[i_year_groups], files))]
  
  #actor
  temp_actors <- readRDS(temp_files[grep("actors.rds", temp_files)])
  assign(paste0("actors_", year_groups[i_year_groups]), temp_actors)
  rm(temp_actors)
  
  #coms
  temp_files <- temp_files[-c(grep("actors.rds", temp_files))]
  for(i_com in 1:length(temp_files)) {
    temp_com <- readRDS(temp_files[i_com])
    assign(paste0("com_", year_groups[i_year_groups], "_", i_com), temp_com)
    rm(temp_com)
  }
  rm(temp_files)
}

#####################
#create corpora per community 

#######
#THINGS TO CONSIDER
# Published for the first time in that era? This can be done by checking work id against first instance.
# Published multiple times? This can be checked with the work field.
#######

existing_coms <- objects()
existing_coms <- existing_coms[c(grep("com_\\d\\d\\d\\d", existing_coms))]

full_text_files <- list.files("../../../eebo_ecco_full_text", full.names = TRUE)
full_text_ids <- str_sub(full_text_files, 30)
full_text_ids <- gsub(".txt", "", full_text_ids)

#estc_year <- read.csv("../../data/raw/estc_ids_and_years.csv", stringsAsFactors = FALSE)

for(i_com_corp in 1:length(existing_coms)) {
  cat("\nCommunity", i_com_corp, "of", length(existing_coms), "\n")
  temp_com <- get(existing_coms[i_com_corp])
  
  #get current era to prevent discounting works shared betweenc communities
    era <- str_extract(existing_coms[i_com_corp], "\\d\\d\\d\\d-\\d\\d\\d\\d")
    start_year <- str_extract(era, "\\d\\d\\d\\d")
    #end_year <- gsub(paste0(start_year, "-"), "", era)
    #year_groups <- unique(str_extract(files, "\\d\\d\\d\\d-\\d\\d\\d\\d"))
  rm(era)
  ###################################
  #These steps remove works published prior to era
  
  #First those which can be seen in the data itself (rather than pinging the database)
  #Find existing dupes and remove all but first instance - if multiple same year, take title with most characters
  dupes <- intersect(which(!is.na(temp_com$work_id)), which(duplicated(temp_com$work_id)))
  dupes <- unique(temp_com$work_id[dupes])
  for(i_dupes in 1:length(dupes)) {
    temp_dupes_df <- temp_com[which(temp_com$work_id == dupes[i_dupes]),]
    bad_estcs <- temp_dupes_df$estc_ids[which(temp_dupes_df$pub_year > min(temp_dupes_df$pub_year))]
    #check to see there are different years (if 0 all are the same year)
    if(length(bad_estcs) != 0) {
      #remove the already found bad ones
      temp_com <- temp_com[-c(which(temp_com$estc_ids %in% bad_estcs)),]
      #get only the ones which are the first year
      temp_dupes_df <- temp_dupes_df[which(temp_dupes_df$pub_year == min(temp_dupes_df$pub_year)),]
      
      #if only one, keep it and move on
      if(nrow(temp_dupes_df == 1)) {
        rm(temp_dupes_df)
        rm(bad_estcs)
        next
      }
    }
    #for workspublished in the same year (either from start, or after above purge)
    good_estc <- temp_dupes_df$estc_ids[which.max(nchar(paste0(temp_dupes_df$title_1, temp_dupes_df$title_2)))]
    bad_estcs <- temp_dupes_df$estc_ids[which(temp_dupes_df$estc_ids %!in% good_estc[1])]
    temp_com <- temp_com[-c(which(temp_com$estc_ids %in% bad_estcs)),]
    
    rm(temp_dupes_df)
    rm(bad_estcs)
    rm(good_estc)
  }
  
  #Next, look to previous eras
  #check database for instances of IDs existing outside com
  temp_estc_ids <- temp_com$estc_ids
  to_remove <- c()
  for (i_check_work in 1:length(temp_estc_ids)) {
    temp_work_matches <- paste0("MATCH (d1:Document {estc_id: '", temp_estc_ids[i_check_work], "'})-[:WORK_OF]-(w:Work)-[:WORK_OF]-(d2:Document)
    WHERE d2.pub_year > 1500 AND d2.pub_year <", start_year, "
    RETURN d1.estc_id, d1.pub_year, d2.estc_id, d2.pub_year") %>%
      call_neo4j(con)
    #if there are no results go to next (not part of a work)
    if (length(temp_work_matches) == 0) { next }
    #if the one being searched is published the first year skip (the previous stage should mean there is only one published that year in the data still)
    if(unique(temp_work_matches$d1.pub_year[[1]]) == min(temp_work_matches$d2.pub_year[[1]])) { 
      rm(temp_work_matches)
      next 
    } else { 
      #if it wasn't the first published, add it to the list of IDs to be removed
      to_remove <- c(to_remove, temp_estc_ids[i_check_work])
      rm(temp_work_matches)
    }
  }
  rm(temp_estc_ids)
  
  #Remove documents which were published previously (and thus are not indicative of writing by that community)
  temp_com <- temp_com[c(which(temp_com$estc_ids %!in% to_remove)),]
  rm(to_remove)
  
  #####
  #check if a community still exists, if not, move on (next)
  if(nrow(temp_com) == 0) { next }
  
  #Get authors
  temp_com$author <- NA
  for(i_author in 1:nrow(temp_com)) {
    temp_author <- paste0("MATCH (a:Actor)-[:AUTHOR]-(d:Document {estc_id: '", temp_com$estc_ids[i_author], "'})
    RETURN a.name") %>%
      call_neo4j(con)
    if(length(temp_author) > 0) {
      if(length(temp_author$a.name) == 0) { #this is necesasry because some nodes currently have a blank for name
        temp_author <- paste0("MATCH (a:Actor)-[:AUTHOR]-(d:Document {estc_id: '", temp_com$estc_ids[i_author], "'})
                              RETURN a.name_variants") %>%
                              call_neo4j(con)
        if(length(temp_author$a.name_variants) == 0) { #this is necesasry because some nodes currently have a blank for name
          temp_com$author[i_author] <- NA
        } else {
          temp_com$author[i_author] <- paste0(temp_author$a.name_variants[[1]], collapse = ", ")
        }
      } else {
        temp_com$author[i_author] <- paste0(temp_author$a.name[[1]], collapse = ", ")
      }
    }
    rm(temp_author)
  }
  
  #Get full text
  temp_com$full_text <- ""
  temp_estc_ids <- temp_com$estc_ids
  temp_full_text_hits <- which(temp_estc_ids %in% full_text_ids)
  if(length(temp_full_text_hits) > 0) {
    for(i_full_text_pull in 1:length(temp_full_text_hits)) {
      cat("\rPulling text", i_full_text_pull, "of", length(temp_full_text_hits))
      temp_com$full_text[temp_full_text_hits[i_full_text_pull]] <- readr::read_file(full_text_files[which(full_text_ids == temp_estc_ids[temp_full_text_hits[i_full_text_pull]])])
    }
  }
  
  #If a doc without full text, look for other version and pull even if different era - but get first available version
  full_text_to_check <- which(temp_com$full_text == "")
  for (i_full_text_check in 1:length(full_text_to_check)) {
    temp_estc_id <- temp_com$estc_ids[full_text_to_check[i_full_text_check]]
    temp_matching_ids <- paste0("MATCH (d:Document {estc_id: '", temp_estc_id, "'})-[:WORK_OF]->(w:Work)<-[:WORK_OF]-(d2:Document)
                                WHERE d2.full_text = TRUE
                                RETURN d2.estc_id, d2.pub_year
                                ORDER BY d2.pub_year") %>%
                          call_neo4j(con)   
    if(length(temp_matching_ids) > 0) {
      break
      cat("FOUND TO TEST")
    }
  }
  rm(full_text_to_check)
  
  #create corpus
  temp_corp <- corpus(paste0(temp_com$title_1, " ", temp_com$title_2, " ", temp_com$full_text))
  docvars(temp_corp, "pub_year") <- temp_com$pub_year
  docvars(temp_corp, "estc_id") <- temp_com$estc_ids
  docvars(temp_corp, "work_id") <- temp_com$work_id
  
  temp_com_details <- str_sub(existing_coms[i_com_corp], 5)
  assign(paste0("corpus_", temp_com_details), temp_com)
  
  #saveRDS(temp_corp, file = paste0("../../data/work/netowrking_archives/corpora/corp_", str_sub(existing_coms[i_com_corp], 5), ".rds"))
  saveRDS(temp_corp, file = paste0("../../data/work/netowrking_archives/corpora_6_years/corp_", str_sub(existing_coms[i_com_corp], 5), ".rds"))
  
  rm(list = existing_coms[i_com_corp])
  rm(temp_corp)
  rm(temp_com)
  rm(temp_com_details)
  rm(temp_estc_ids)
  rm(temp_full_text_hits)
  gc()
  
}


#####################################
#####################################
#above but titles only
###############################
#####################################


#########
#Get files

#which yers to include
start_year <- 1499
finish_year <- 1801

files <- list.files("../../data/work/netowrking_archives/community_data", recursive = TRUE, full.names = TRUE)
#files <- list.files("../../data/work/netowrking_archives/community_data_6_years/", recursive = TRUE, full.names = TRUE)
file_years <- str_extract(files, "\\d\\d\\d\\d")
files <- files[c(intersect(which(as.numeric(file_years) >= start_year), which(as.numeric(file_years) < finish_year)))]
rm(file_years)
rm(start_year)
rm(finish_year)

#load all

year_groups <- unique(str_extract(files, "\\d\\d\\d\\d-\\d\\d\\d\\d"))

for(i_year_groups in 1:length(year_groups)) {
  cat("\r", i_year_groups)
  temp_files <- files[c(grep(year_groups[i_year_groups], files))]
  
  #actor
  temp_actors <- readRDS(temp_files[grep("actors.rds", temp_files)])
  assign(paste0("actors_", year_groups[i_year_groups]), temp_actors)
  rm(temp_actors)
  
  #coms
  temp_files <- temp_files[-c(grep("actors.rds", temp_files))]
  for(i_com in 1:length(temp_files)) {
    temp_com <- readRDS(temp_files[i_com])
    assign(paste0("com_", year_groups[i_year_groups], "_", i_com), temp_com)
    rm(temp_com)
  }
  rm(temp_files)
}

#####################
#create corpora per community 

#######
#THINGS TO CONSIDER
# Published for the first time in that era? This can be done by checking work id against first instance.
# Published multiple times? This can be checked with the work field.
#######

existing_coms <- objects()
existing_coms <- existing_coms[c(grep("com_\\d\\d\\d\\d", existing_coms))]

# full_text_files <- list.files("../../../eebo_ecco_full_text", full.names = TRUE)
# full_text_ids <- str_sub(full_text_files, 30)
# full_text_ids <- gsub(".txt", "", full_text_ids)

#estc_year <- read.csv("../../data/raw/estc_ids_and_years.csv", stringsAsFactors = FALSE)

for(i_com_corp in 1:length(existing_coms)) {
  cat("\nCommunity", i_com_corp, "of", length(existing_coms), "\n")
  temp_com <- get(existing_coms[i_com_corp])
  
  #get current era to prevent discounting works shared betweenc communities
  era <- str_extract(existing_coms[i_com_corp], "\\d\\d\\d\\d-\\d\\d\\d\\d")
  start_year <- str_extract(era, "\\d\\d\\d\\d")
  #end_year <- gsub(paste0(start_year, "-"), "", era)
  #year_groups <- unique(str_extract(files, "\\d\\d\\d\\d-\\d\\d\\d\\d"))
  rm(era)
  ###################################
  #These steps remove works published prior to era
  
  #First those which can be seen in the data itself (rather than pinging the database)
  #Find existing dupes and remove all but first instance - if multiple same year, take title with most characters
  dupes <- intersect(which(!is.na(temp_com$work_id)), which(duplicated(temp_com$work_id)))
  dupes <- unique(temp_com$work_id[dupes])
  for(i_dupes in 1:length(dupes)) {
    temp_dupes_df <- temp_com[which(temp_com$work_id == dupes[i_dupes]),]
    bad_estcs <- temp_dupes_df$estc_ids[which(temp_dupes_df$pub_year > min(temp_dupes_df$pub_year))]
    #check to see there are different years (if 0 all are the same year)
    if(length(bad_estcs) != 0) {
      #remove the already found bad ones
      temp_com <- temp_com[-c(which(temp_com$estc_ids %in% bad_estcs)),]
      #get only the ones which are the first year
      temp_dupes_df <- temp_dupes_df[which(temp_dupes_df$pub_year == min(temp_dupes_df$pub_year)),]
      
      #if only one, keep it and move on
      if(nrow(temp_dupes_df == 1)) {
        rm(temp_dupes_df)
        rm(bad_estcs)
        next
      }
    }
    #for workspublished in the same year (either from start, or after above purge)
    good_estc <- temp_dupes_df$estc_ids[which.max(nchar(paste0(temp_dupes_df$title_1, temp_dupes_df$title_2)))]
    bad_estcs <- temp_dupes_df$estc_ids[which(temp_dupes_df$estc_ids %!in% good_estc[1])]
    temp_com <- temp_com[-c(which(temp_com$estc_ids %in% bad_estcs)),]
    
    rm(temp_dupes_df)
    rm(bad_estcs)
    rm(good_estc)
  }
  
  #Next, look to previous eras
  #check database for instances of IDs existing outside com
  temp_estc_ids <- temp_com$estc_ids
  to_remove <- c()
  for (i_check_work in 1:length(temp_estc_ids)) {
    temp_work_matches <- paste0("MATCH (d1:Document {estc_id: '", temp_estc_ids[i_check_work], "'})-[:WORK_OF]-(w:Work)-[:WORK_OF]-(d2:Document)
                                WHERE d2.pub_year > 1500 AND d2.pub_year <", start_year, "
                                RETURN d1.estc_id, d1.pub_year, d2.estc_id, d2.pub_year") %>%
      call_neo4j(con)
    #if there are no results go to next (not part of a work)
    if (length(temp_work_matches) == 0) { next }
    #if the one being searched is published the first year skip (the previous stage should mean there is only one published that year in the data still)
    if(unique(temp_work_matches$d1.pub_year[[1]]) == min(temp_work_matches$d2.pub_year[[1]])) { 
      rm(temp_work_matches)
      next 
    } else { 
      #if it wasn't the first published, add it to the list of IDs to be removed
      to_remove <- c(to_remove, temp_estc_ids[i_check_work])
      rm(temp_work_matches)
    }
  }
  rm(temp_estc_ids)
  
  #Remove documents which were published previously (and thus are not indicative of writing by that community)
  temp_com <- temp_com[c(which(temp_com$estc_ids %!in% to_remove)),]
  rm(to_remove)
  
  #####
  #check if a community still exists, if not, move on (next)
  if(nrow(temp_com) == 0) { next }
  
  #Get authors
  temp_com$author <- NA
  for(i_author in 1:nrow(temp_com)) {
    temp_author <- paste0("MATCH (a:Actor)-[:AUTHOR]-(d:Document {estc_id: '", temp_com$estc_ids[i_author], "'})
                          RETURN a.name") %>%
      call_neo4j(con)
    if(length(temp_author) > 0) {
      if(length(temp_author$a.name) == 0) { #this is necesasry because some nodes currently have a blank for name
        temp_author <- paste0("MATCH (a:Actor)-[:AUTHOR]-(d:Document {estc_id: '", temp_com$estc_ids[i_author], "'})
                              RETURN a.name_variants") %>%
          call_neo4j(con)
        if(length(temp_author$a.name_variants) == 0) { #this is necesasry because some nodes currently have a blank for name
          temp_com$author[i_author] <- NA
        } else {
          temp_com$author[i_author] <- paste0(temp_author$a.name_variants[[1]], collapse = ", ")
        }
      } else {
        temp_com$author[i_author] <- paste0(temp_author$a.name[[1]], collapse = ", ")
      }
    }
    rm(temp_author)
  }
  
  # #Get full text
  # temp_com$full_text <- ""
  # temp_estc_ids <- temp_com$estc_ids
  # temp_full_text_hits <- which(temp_estc_ids %in% full_text_ids)
  # if(length(temp_full_text_hits) > 0) {
  #   for(i_full_text_pull in 1:length(temp_full_text_hits)) {
  #     cat("\rPulling text", i_full_text_pull, "of", length(temp_full_text_hits))
  #     temp_com$full_text[temp_full_text_hits[i_full_text_pull]] <- readr::read_file(full_text_files[which(full_text_ids == temp_estc_ids[temp_full_text_hits[i_full_text_pull]])])
  #   }
  # }
  # 
  # #If a doc without full text, look for other version and pull even if different era - but get first available version
  # full_text_to_check <- which(temp_com$full_text == "")
  # for (i_full_text_check in 1:length(full_text_to_check)) {
  #   temp_estc_id <- temp_com$estc_ids[full_text_to_check[i_full_text_check]]
  #   temp_matching_ids <- paste0("MATCH (d:Document {estc_id: '", temp_estc_id, "'})-[:WORK_OF]->(w:Work)<-[:WORK_OF]-(d2:Document)
  #                               WHERE d2.full_text = TRUE
  #                               RETURN d2.estc_id, d2.pub_year
  #                               ORDER BY d2.pub_year") %>%
  #     call_neo4j(con)   
  #   if(length(temp_matching_ids) > 0) {
  #     break
  #     cat("FOUND TO TEST")
  #   }
  # }
  # rm(full_text_to_check)
  
  #create corpus
  #temp_corp <- corpus(paste0(temp_com$title_1, " ", temp_com$title_2, " ", temp_com$full_text))
  temp_corp <- corpus(paste0(temp_com$title_1, " ", temp_com$title_2))
  docvars(temp_corp, "pub_year") <- temp_com$pub_year
  docvars(temp_corp, "estc_id") <- temp_com$estc_ids
  docvars(temp_corp, "work_id") <- temp_com$work_id
  
  temp_com_details <- str_sub(existing_coms[i_com_corp], 5)
  assign(paste0("corpus_", temp_com_details), temp_com)
  
  #saveRDS(temp_corp, file = paste0("../../data/work/netowrking_archives/corpora/corp_", str_sub(existing_coms[i_com_corp], 5), ".rds"))
  saveRDS(temp_corp, file = paste0("../../data/work/netowrking_archives/corpora_titles/corp_", str_sub(existing_coms[i_com_corp], 5), ".rds"))
  
  rm(list = existing_coms[i_com_corp])
  rm(temp_corp)
  rm(temp_com)
  rm(temp_com_details)
  rm(temp_estc_ids)
  #rm(temp_full_text_hits)
  gc()
  
}
