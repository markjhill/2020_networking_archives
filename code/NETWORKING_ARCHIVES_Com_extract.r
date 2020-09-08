#Cambridge com pres

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

con <- neo4j_api$new(
  url = "http://localhost:7474",
  user = "neo4j", 
  password = "1234"
)

get_igraph_from_neo4j <- function(G) {
  
  #remove mutil-birth/death years and collapse arrays
  nodes <- length(G$nodes$properties)
  for (i_node in 1:nodes) {
    G$nodes$properties[[i_node]][which(names(G$nodes$properties[[i_node]]) == "year_birth")] <- 
      unlist(G$nodes$properties[[i_node]][which(names(G$nodes$properties[[i_node]]) == "year_birth")])[[1]]
    G$nodes$properties[[i_node]][which(names(G$nodes$properties[[i_node]]) == "year_death")] <-
      unlist(G$nodes$properties[[i_node]][which(names(G$nodes$properties[[i_node]]) == "year_death")])[[1]]
    
    G$nodes$properties[[i_node]][which(names(G$nodes$properties[[i_node]]) == "tagged_corp")] <- 
      unlist(G$nodes$properties[[i_node]][which(names(G$nodes$properties[[i_node]]) == "tagged_corp")])[[1]]
    
    G$nodes$properties[[i_node]][which(names(G$nodes$properties[[i_node]]) == "year_active_last_estc")] <- 
      unlist(G$nodes$properties[[i_node]][which(names(G$nodes$properties[[i_node]]) == "year_active_last_estc")])[[1]]
    G$nodes$properties[[i_node]][which(names(G$nodes$properties[[i_node]]) == "year_active_first_estc")] <- 
      unlist(G$nodes$properties[[i_node]][which(names(G$nodes$properties[[i_node]]) == "year_active_first_estc")])[[1]]
    
    to_collapse <- which(lapply(G$nodes$properties[[i_node]], length) > 1)
    if(length(to_collapse) == 0) { next }
    G$nodes$properties[[i_node]][to_collapse] <- lapply(G$nodes$properties[[i_node]][to_collapse], paste0, collapse = ", ")
  }
  
  # #collapse arrays
  # nodes <- length(G$nodes$properties)
  # for (i_node in 1:nodes) {
  #   to_collapse <- which(lapply(G$nodes$properties[[i_node]], length) > 1)
  #   if(length(to_collapse) == 0) { next }
  #   G$nodes$properties[[i_node]][to_collapse] <- lapply(G$nodes$properties[[i_node]][to_collapse], paste0, collapse = ", ")
  # }
  
  G$nodes <- G$nodes %>%
    unnest_nodes(what = "properties") %>% 
    # We're extracting the first label of each node, but 
    # this column can also be removed if not needed
    mutate(label = map_chr(label, 1))
  #G$nodes$label <- ifelse(is.na(G$nodes$name), str_trunc(G$nodes$title, 20, "right"), str_trunc(G$nodes$name, 20, "right"))
  
  G$relationships <- G$relationships %>%
    unnest_relationships() %>%
    dplyr::select(startNode, endNode, type, everything())
  
  graph_object <- igraph::graph_from_data_frame(
    d = G$relationships, 
    directed = TRUE, 
    vertices = G$nodes
  )
  
  #Lables and colours
  
  V(graph_object)$color[which(V(graph_object)$label == "Actor")] <- "yellow"
  V(graph_object)$type[which(V(graph_object)$label == "Actor")] <- "Actor"
  V(graph_object)$label[which(V(graph_object)$label == "Actor")] <- names(V(graph_object)[which(V(graph_object)$label == "Actor")])
  
  V(graph_object)$color[which(V(graph_object)$label == "Document")] <- "green"
  V(graph_object)$type[which(V(graph_object)$label == "Document")] <- "Document"
  V(graph_object)$label[which(V(graph_object)$label == "Document")] <- stringr::str_trunc(V(graph_object)$title[which(V(graph_object)$label == "Document")], 20)
  
  V(graph_object)$color[which(V(graph_object)$label == "Work")] <- "orange"
  V(graph_object)$type[which(V(graph_object)$label == "Work")] <- "Work"
  V(graph_object)$label[which(V(graph_object)$label == "Work")] <- stringr::str_trunc(V(graph_object)$work_id[which(V(graph_object)$label == "Work")], 20)
  
  return(graph_object)
}


#number_of_years <- 10
number_of_years <- 6
eras <- seq(1501, 1800, (number_of_years/2)) #divide by 2 to overlap half of data

for(i_era in 1:length(eras)) {
  cat("\nPulling data for: ", eras[i_era],"-",(eras[i_era]+number_of_years-1), "\n")
  rm(total_com_df)
  
  #Get actors
  g <- paste0("MATCH (a1:Actor)-[:BOOKSELLER|PUBLISHER|PRINTER]->(d1:Document)-[:WORK_OF]->(w:Work)<-[:WORK_OF]-(d2:Document)<-[:BOOKSELLER|PUBLISHER|PRINTER]-(a2:Actor)
              WHERE id(a1)<>id(a2) AND d1.pub_year IN range(",eras[i_era],",",(eras[i_era]+number_of_years-1),") AND d2.pub_year IN range(",eras[i_era],",",(eras[i_era]+number_of_years-1),") AND d1.pub_city CONTAINS 'London' AND NOT d1.pub_city CONTAINS 'New London' AND d2.pub_city CONTAINS 'London' AND NOT d2.pub_city CONTAINS 'New London'  
              CALL apoc.create.vRelationship(a1,'JUMP', {}, a2) YIELD rel
              RETURN a1, rel, a2") %>%
    call_neo4j(con, type = "graph")
  
  gg <- get_igraph_from_neo4j(g) %>%
    as.undirected(mode = "each")
  
  #louvain 
  l <- cluster_louvain(gg)
  # #edge betweenness
  # eb <- cluster_edge_betweenness(gg)
  # #fastgreedy - NO MULTIPLE EDGES
  # fg <- simplify(gg) %>%
  #   cluster_fast_greedy()
  # #walktrap 
  # wt <- cluster_walktrap(gg)
  # 
  # sg <- cluster_spinglass(gg)
  # 
  # le <- cluster_leading_eigen()
  # 
  # lp <- cluster_label_prop(gg)
  
  
  for(i_com in 1:length(l)) {
    
    names_df <- data.frame(name = l[[i_com]])
    names_df$actor_id <- V(gg)$actor_id[which(l$membership == i_com)]
    names_df$community <- i_com
    names_df[] <- lapply(names_df, function(x) {gsub('"", ', "", x)})
    
    if(i_com == 1) {
      total_com_df <- names_df
    } else {
      total_com_df <- rbind(total_com_df, names_df)
    }
    
    #save actor and com data
  }
  
  #dir.create(paste0("../../data/work/netowrking_archives/community_data/", eras[i_era], "-", eras[i_era]+number_of_years-1, "/"))
  #saveRDS(total_com_df, paste0("../../data/work/netowrking_archives/community_data/", eras[i_era], "-", eras[i_era]+number_of_years-1, "/actors.rds"))
  
  dir.create(paste0("../../data/work/netowrking_archives/community_data_6_years/", eras[i_era], "-", eras[i_era]+number_of_years-1, "/"))
  saveRDS(total_com_df, paste0("../../data/work/netowrking_archives/community_data_6_years/", eras[i_era], "-", eras[i_era]+number_of_years-1, "/actors.rds"))
  
  #Get ESTC IDs and titles
  for(i_com in 1:length(l)) {
    cat("\rCommunity:", i_com)
    temp_df <- total_com_df[which(total_com_df$community == i_com),]
    estc_ids <- c()
    title_1 <- c()
    title_2 <- c()
    pub_year <- c()
    for(i_actor in 1:nrow(temp_df)) {
      #cat("\rActor: ", i_actor)
      temp_estc_ids <- paste0("MATCH (a:Actor)-[:BOOKSELLER|PUBLISHER|PRINTER]->(d:Document)
                WHERE a.actor_id = '", temp_df$actor_id[i_actor], "' AND d.pub_year IN range(",eras[i_era],",",(eras[i_era]+number_of_years-1),") AND d.pub_city CONTAINS 'London' AND NOT d.pub_city CONTAINS 'New London'
                RETURN DISTINCT d.estc_id, d.title, d.title_remainder, d.pub_year") %>%
        call_neo4j(con)
      
      estc_ids <- c(estc_ids, temp_estc_ids$d.estc_id[[1]])
      title_1 <- c(title_1, temp_estc_ids$d.title[[1]])
      title_2 <- c(title_2, temp_estc_ids$d.title_remainder[[1]])
      pub_year <- c(pub_year, temp_estc_ids$d.pub_year[[1]])
    }
    
    if(any(duplicated(estc_ids)) == TRUE) {
      title_1 <- title_1[-c(which(duplicated(estc_ids)))]
      title_2 <- title_2[-c(which(duplicated(estc_ids)))]
      pub_year <- pub_year[-c(which(duplicated(estc_ids)))]
      estc_ids <- estc_ids[-c(which(duplicated(estc_ids)))]
    }
    
    temp_title_df <- data.frame(estc_ids = estc_ids, title_1 = title_1, title_2 = title_2, pub_year = pub_year)
    
    #get work id
    
    temp_title_df$work_id <- NA
    
    for(i_work in 1:nrow(temp_title_df)) {
      
      temp_work_ids <- paste0("MATCH (d:Document)-[:WORK_OF]->(w:Work)
                WHERE d.estc_id = '", temp_title_df$estc_ids[i_work], "'
                RETURN w.work_id") %>%
        call_neo4j(con)
      if(length(temp_work_ids) > 0) {
        temp_title_df$work_id[i_work] <- temp_work_ids$w.work_id[[1]]
      } else {
        next
      }
    }
    
    #save com title file
    #saveRDS(temp_title_df, paste0("../../data/work/netowrking_archives/community_data/", eras[i_era], "-", eras[i_era]+number_of_years-1, "/comm_", i_com, ".rds"))
    saveRDS(temp_title_df, paste0("../../data/work/netowrking_archives/community_data_6_years/", eras[i_era], "-", eras[i_era]+number_of_years-1, "/comm_", i_com, ".rds"))
    rm(temp_title_df)
  } 
}


# #Getting document texts
# 
# library(rvest)
# library(curl)
# library(httr)
# library(jsonlite)
# library(stringi)
# library(stringr)
# 
# comm_files <- list.files("../../data/work/netowrking_archives/community_data/", recursive = TRUE, full.names = TRUE)
# comm_files <- comm_files[-c(grep("actors", comm_files))]
# #titles_estc <- read.csv("data/work/estc_ids_and_full_titles.csv", stringsAsFactors = FALSE)
# 
# #pulling data from octavo
# # example URL: https://vm0824.kaj.pouta.csc.fi/octavo/ecco/search?query=%3CDOCUMENT%C2%A7ESTCID:T202855%C2%A7DOCUMENT%3E&field=content&field=title
# #https://vm0824.kaj.pouta.csc.fi/octavo/ecco/search?query=%3CDOCUMENT%C2%A7ESTCID:N12659%C2%A7DOCUMENT%3E&field=content&field=title
# 
# #base_url <- "https://vm0824.kaj.pouta.csc.fi/octavo/ecco/search?query=%3CDOCUMENT%C2%A7ESTCID:"
# base_url <- "https://vm0824.kaj.pouta.csc.fi/octavo/eebo/search?query=%3CDOCUMENT%C2%A7ESTCID:"
# estc_id <- "T202855" #must be six digits, if not pad.
# url_tail <- "%C2%A7DOCUMENT%3E&field=content&field=title"
# 
# for (i_fulltext in 178:length(comm_files)) {
#   cat("\n", comm_files[i_fulltext], "\n")
#   
#   temp_data <- readRDS(comm_files[i_fulltext])
#   temp_data$full_text <- NA
#   
#   for(i_text in 1:nrow(temp_data)) {
#     cat("\rText:", i_text)
#     #Get ESTCS
#     estc <- temp_data$estc_ids[i_text]
#     #if(nchar(estc) != 7) { stri_sub(estc, 2, 1) <- paste0(rep(0, 7-nchar(estc)), collapse = "") }
#     current_doc_address <- paste0(base_url, estc, url_tail)
#     current_doc_json <- GET(current_doc_address, authenticate("hume","bacon"))
#     if(current_doc_json$status_code != 200) {
#       cat("\nError - status code\n")
#       break
#     }
#     jsonRespParsed<-content(current_doc_json,as="parsed") 
#     if(jsonRespParsed$results$total == 1) {
#       
#       #temp_com_corp$just_title[i_estcs] <- FALSE
#       temp_data$full_text[i_text] <- jsonRespParsed$results$docs[[1]]$content
# 
#       # paste(titles_estc$title[which(titles_estc$estc_id == paste0("(CU-RivES)", estcs[i_estcs]))], 
#       #       temp_com_corp$doc[i_estcs] <- jsonRespParsed$results$docs[[1]]$content)
#     } else {
#       next
#     }
#   }
#   saveRDS(temp_data, comm_files[i_fulltext])
# }






#for a whole community - I'll try 1655 for quakers first then somethign in the 18th century, but will need to change database.
  
temp_data <- readRDS("data/work/community_tokens/community_fastgreedy_data_1655.csv.rds")
#temp_data <- temp_data[c(6,9),]

for(i_comms in 1:nrow(temp_data)) {
  #Get ESTCS
  estcs <- temp_data$estc_ids[i_comms]
  estcs <- unlist(str_split(estcs, "; "))
  estcs <- gsub("[(]CU-RivES[)]", "", estcs)
  temp_com_df <- data.frame(estc_id = estcs, doc = NA, just_title = NA)
  # This is necessary for ECCO, but not EEBO
  for(i_estcs in 1:length(estcs)) {
    cat("\rCommunity ", i_comms, ". Title: ", i_estcs, " out of ", length(estcs), "               ", sep = "")
    #   if(nchar(estcs[i_estcs]) != 7) { stri_sub(estcs[i_estcs], 2, 1) <- rep(0, 7-nchar(estcs[i_estcs])) }
    # }
    current_doc_address <- paste0(base_url, estcs[i_estcs], url_tail)
    current_doc_json <- GET(current_doc_address, authenticate("hume","bacon"))
    #document <- fromJSON(txt=GET(current_doc_address, authenticate("hume","bacon")))
    jsonRespParsed<-content(current_doc_json,as="parsed") 
    if(current_doc_json$status_code != 200) {
      cat("\nError - status code\n")
      break
    }
    if(jsonRespParsed$results$total == 1) {
      temp_com_df$just_title[i_estcs] <- FALSE
      paste(titles_estc$title[which(titles_estc$estc_id == paste0("(CU-RivES)", estcs[i_estcs]))], 
            temp_com_df$doc[i_estcs] <- jsonRespParsed$results$docs[[1]]$content)
    } else {
      temp_com_df$just_title[i_estcs] <- TRUE
      temp_com_df$doc[i_estcs] <- titles_estc$title[which(titles_estc$estc_id == paste0("(CU-RivES)", estcs[i_estcs]))]
    }
  }
  
  saveRDS(temp_com_df, file = paste0("data/work/dhn_data/1655_com_", i_comms, ".rds"))
}


