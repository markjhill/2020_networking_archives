#Track work

#Load all Comms by text. Ignore dfm. Check for work Id. Record year and com. 
#Go back and look at top features, other works in the community.


setwd("code/")

options(stringsAsFactors = FALSE)
'%!in%' <- function(x,y)!('%in%'(x,y))

era <- seq(1501, 1800, 5)
era <- c(era, 1801, 1806)

work_to_check <- "43-eikon basilike"
work_to_check <- "4425-eikonoklastes in answer to book intitld eikon basilike portrature of his sacred majesty in his solitudes and sufferings"

work_to_check <- "552-observations on nature of civil liberty"
work_to_check <- "1558-to all that would know way to kingdome"
work_to_check <- "1601-instructions for right spelling and plain directions for reading and writing true english"
work_to_check <- "3476-a paper sent forth into world from them that are scornfully called quakers declaring grounds and reasons why they deny teachers of world who profess themselves to be ministers and dissent from them"
work_to_check <- "749-law is bottomless pit or history of john bull"

com_hits <- c()

for (i_era_check in 1:(length(era)-2)) {
  cat("\r", era[i_era_check])
  temp_files <- list.files(paste0("../../ESTC_SNA_data_creation/data/work/netowrking_archives/community_data/", era[i_era_check], "-", era[(i_era_check+2)]-1, "/"), full.names = TRUE)
  temp_files <- temp_files[-c(grep("actors", temp_files))]
  for(i_com_check in 1:length(temp_files)) {
    temp_com <- readRDS(temp_files[i_com_check])
    match <- which(temp_com$work_id == work_to_check)
    if(length(match) == 0) { next }
    cat("\nHit!", temp_files[i_com_check], "\n")
    com_hits <- c(com_hits, temp_files[i_com_check])
  }
}

temp_actors <- readRDS("../../ESTC_SNA_data_creation/data/work/netowrking_archives/community_data/1771-1780/actors.rds")
