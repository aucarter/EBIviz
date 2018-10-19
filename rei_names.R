library(data.table)

source("/home/j/temp/central_comp/libraries/current/r/get_rei_metadata.R")
source("/home/j/temp/central_comp/libraries/current/r/get_cause_metadata.R")

# read data
dt <- fread("/share/epi/risk/bmgf/decomp/2000_2016/decomp_147.csv")

rei.names <- rbind(fread("/homes/aucarter/bmgf_decomp/decomp/rei_names.csv"),
				   data.table(rei_id = c(311, 322), rei_name = c("ITN", "IRS")))

setdiff(unique(dt$rei_id), rei.names$rei_id)


cause_ids <- get_cause_metadata(cause_set_id=3, gbd_round_id=4)[most_detailed==1 & parent_id != 298 | cause_id == 298, .(cause_name, cause_id)]

setdiff(unique(dt$cause_id), cause_ids$cause_id)


# merge on names
cause.dt <- merge(dt, cause_ids)

rei.dt <- merge(cause.dt, rei.names, by = "rei_id", all.x = T)



# Add level 1 risk category
rei.dt[rei_id %in% c(313,312,311,322,314,315,316,317,318,324,325,327,330,336,323), level1 := "Intervention"]

reis <- get_rei_metadata(rei_set_id = 1, gbd_round_id = 4)
env_ids <- reis[grep('202', reis$path_to_top_parent)]$rei_id
behave_ids <- reis[grep('203', reis$path_to_top_parent)]$rei_id
rei.dt[rei_id %in% env_ids, level1 := "Environmental risk factors"]
rei.dt[rei_id %in% behave_ids, level1 := "Behavioral risk factors"]

out.dt <- rei.dt[order(cause_name, rei_name), .(cause_name, rei_name, level1)]


write.csv(out.dt, "/home/j/temp/aucarter/decomp_reis.csv", row.names = F)
