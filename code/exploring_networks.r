#exploring era specific DFMs


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


con <- neo4j_api$new(
  url = "http://localhost:7474",
  user = "neo4j", 
  password = "1234"
)

era <- "1761-1770"


latin_stopwords <- read.csv("../../ESTC_SNA_data_creation/data/raw/latin.stopwords.clean", stringsAsFactors = FALSE)
extra_stopwords <- c("shall", "may", "one", "us", "hath", "yet", "upon", "yet", "now", "said", "unto", "thy", "doe", "first", "must",
                     "much", "made", "many", "make", "also", "without", "can", "thou", "like", "can", "though", "therefore", "without",
                     "two", "things", "might", "way", "say", "day", "let", "well", "things", "take", "owne", "doth", "i.e", "tis", "page removed",
                     #THIS IS IMPORTANT!
                     "non-latin", "alphabet", "saith", "thus", "|", "¦", "⁻", "vol", "year", "printed", "published",
                     "na", "amp", "ye", "anno", "translated", "vpon", "vnto", "yeere", "haue", "three", "second", "thereof", "yeare", "written", "mr",
                     "wherein", "written", "esq", "author", "part", "dr", "volumes", "1800", "volume", "mrs", "m.d", "d.d", "b.d")


temp_dfm <- readRDS(paste0("../../ESTC_SNA_data_creation/data/work/netowrking_archives/com_dfm_titles/com_dfm_", era, ".rds"))
temp_dfm <- dfm_select(temp_dfm, pattern = c(stopwords("english"), stopwords("french"), stopwords("italian"), stopwords("spanish"), latin_stopwords$a, extra_stopwords), selection = "remove", valuetype = "fixed")
temp_dfm <- dfm_select(temp_dfm, pattern = c("\\d\\d\\d\\d\\d", "\\d\\d\\d\\d", "\\d\\d\\d", "\\d\\d", "\\d"), selection = "remove", valuetype = "regex")

temp_dir <- paste0("../../ESTC_SNA_data_creation/data/work/netowrking_archives/community_data/", era, "/")
temp_files <- list.files(temp_dir, full.names = TRUE)
actors <- readRDS(paste0(temp_dir, "actors.rds"))
temp_files <- temp_files[-(grep("actor", temp_files))]

# for(i_com in 1:length(temp_files)) {
#   temp_com <- readRDS(temp_files[i_com])
#   current_com <- gsub(paste0("../ESTC_SNA_data_creation/data/work/netowrking_archives/community_data/", era, "//"), "", temp_files[i_com])
#   current_com <- gsub(".rds", "", current_com)
#   #check which com it may be
#   for(i_com_check in 1:length(unique(temp_dfm@docvars$Community)))
#   if(all(temp_dfm@docvars$estc_id[which(temp_dfm@docvars$Community == paste0("Community_", i_com_check))] %in% temp_com$estc_ids)) {
#     #assign(paste0("com_", era, "_", i_com_check), temp_com)
#     cat("\nExtracted data com", current_com, "is", i_com_check, "in DFM")
#   }
# }


temp_actors <- paste0(actors$actor_id[which(actors$community == "2")], collapse = '", "')
temp_estc <- com_1641-`com_1641-1650_2`
start_year <- str_extract(era, "^\\d\\d\\d\\d")
end_year <- str_extract(era, "\\d\\d\\d\\d$")



cat(paste0('MATCH (a1:Actor)-[]-(d:Document)-[]-(a2:Actor)
WHERE a1.actor_id IN ["', temp_actors, '"] AND a2.actor_id IN ["', temp_actors, '"] AND d.pub_city CONTAINS "London" AND d.pub_year IN range(', start_year, ',', end_year, ')
RETURN a1, a2, d'))

temp_estc_id <- paste0(temp_dfm@docvars$estc_id[which(temp_dfm@docvars$Community == com_num[i_authors])], collapse = '", "')
#getting subjects
temp_subjects <- paste0('MATCH (d:Document)
WHERE d.estc_id IN ["', temp_estc_id, '"] 
RETURN d.subjects_600, d.simplified_dd_subject') %>%
  call_neo4j(con)

table(temp_subjects$d.simplified_dd_subject[[1]][which(!is.na(temp_subjects$d.simplified_dd_subject[[1]]))])

table(temp_subjects$d.subjects_600[[1]])

#get must actibe

"MATCH (a1:Actor)-[rel]-(d:Document)
WHERE a1.actor_id IN ["170699710", "sisumptibussocietatisstationarum_0", "49294208", "40794771", "bbti_135440", "bbti_131474", "31881771", "68823052", "bbti_102043", "68526243", "34527193", "bbti_10293", "feglesfieldlondinensium_1", "34342322", "bbti_73285", "39261895", "bbti_48870", "44552282", "32067131", "bbti_32527", "7721377", "bbti_5405", "12361505", "bbti_49856", "34801468", "53710534", "54032690", "bbti_103091", "bbti_102303", "geoleone_0", "66352513", "bbti_19179", "bbti_73955", "bbti_28296", "bbti_979", "iaquesalestry_0", "17107362", "NV17154", "bbti_77317", "NV31833", "9727599", "committeeofestates_1", "bbti_103162", "bbti_103083", "9730731", "NV18581", "bbti_45403", "bbti_51065", "bbti_99633", "bbti_19013", "bbti_22486", "bbti_34307", "bbti_15079", "bbti_44357", "bbti_64335", "2351234", "peterwhaley_0", "bbti_49857", "bbti_102188", "bbti_53025", "bbti_103088", "bbti_102523", "bbti_49672", "bbti_36300", "bbti_102952", "bbti_103053", "bbti_200235", "ioycenorton_0", "bbti_8961", "bbti_48891", "imprimatur_0", "edwardforrest_0", "100968801", "39267981", "bbti_1040", "johndawson_0", "bd_0", "63891780", "bbti_102950", "twprochristophermeridith_0", "bbti_101554", "bbti_16645", "robsomer_0", "bbti_102875", "bbti_44356", "trem_0", "bbti_30529", "bbti_37180", "bbti_26016", "williamwright_1", "bbti_102033", "bbti_135128", "ralphbrocklebanck_0", "bbti_45546", "jamesyoungsumptibusdanielfrerebibliopol_0", "bbti_61169", "bbti_41149", "thovnderhill_0", "tobylangford_0", "iohnlong_0", "311335919", "bbti_70410", "bbti_107602", "andrewcock_0", "bbti_40603", "NV18578", "dwellinginfleetstreet_0", "hwhite_0", "richardbestandjohnplace_0", "rbestandiplace_0", "sthomaseshospitall_0", "bbti_60002", "90694831", "39723643", "mortar_0", "charingcrosse_0", "thomamrobinsonoxonii_1", "kingdomeofscotland_1", "bbti_103221"] AND d.pub_city CONTAINS "London" AND d.pub_year IN range(1641,1650)
RETURN a1.name, count(d)"


"MATCH (a:Actor {name: "Nedham, Marchamont, 1620-1678."})-[]->(d)<-[]-(a2)
RETURN a2.name, COUNT(d)
ORDER by  COUNT(d)"

g <- paste0('MATCH (a1:Actor)-[rel1]-(d:Document)-[rel2]-(a2:Actor)
WHERE a1.actor_id IN ["', temp_actors, '"] AND a2.actor_id IN ["', temp_actors, '"] AND d.pub_city CONTAINS "London" AND d.pub_year IN range(', start_year, ',', end_year, ')
RETURN a1, a2, d, rel1, rel2') %>% 
  call_neo4j(con, type = "graph")

g <- paste0('MATCH (a1:Actor)-[rel1]-(d:Document)-[rel2]-(a2:Actor)
WHERE a1.actor_id IN ["', temp_actors, '"] AND a2.actor_id IN ["', temp_actors, '"] AND d.pub_city CONTAINS "London" AND d.pub_year IN range(', start_year, ',', end_year, ')
RETURN a1, a2, d, rel1, rel2') %>% 
  call_neo4j(con, type = "graph")

gg <- get_igraph_from_neo4j(g) %>%
  as.undirected(mode = "each")

plot.igraph(gg, vertex.size = .5, vertex.label = NA, label.cex = 0)

textstat_simil(temp_dfm, "monarch")

