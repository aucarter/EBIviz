## Pull U5M and NN Mortality with births as denom straight from the life table
# Setup
source("/home/j/temp/central_comp/libraries/current/r/get_location_metadata.R")
source("/home/j/Project/Mortality/shared/functions/get_mort_outputs.R")

# location table
loc.table <- data.table(get_location_metadata(location_set_id = 21))
loc.names <- c("Rwanda", "Ethiopia", "Senegal", "Peru", "Nepal", "Cambodia", "Bangladesh")
loc.list <- loc.table[location_name %in% loc.names, location_id]

# Mortality rates
u5 <- get_mort_outputs("5q0", "estimate", location_id = loc.list, sex_id = 3, gbd_year = 2016)[grepl("w/fa", estimate_stage_name)]

# Calculate nn with enn and lnn per Grant's formula *** age_group_id 42 will have this soon!
enn <- get_mort_outputs("age_sex", "estimate", age_group_id = 2, location_id = loc.list, sex_id = 3, gbd_year = 2016)[grepl("w/fa", estimate_stage_name)]
lnn <- get_mort_outputs("age_sex", "estimate", age_group_id = 3, location_id = loc.list, sex_id = 3, gbd_year = 2016)[grepl("w/fa", estimate_stage_name)]
setnames(enn , "mean", "enn")
setnames(lnn , "mean", "lnn")
enn[, age_group_id := NULL]
lnn[, age_group_id := NULL]

nn <- merge(enn, lnn, by = c("location_id", "sex_id", "year_id"))
nn[, mean := 1 - (1 - lnn)*(1 - enn)]
nn[, age_group_id := 42]

# Combine
vars <- c("location_id", "year_id", "age_group_id", "mean")
out.dt <- rbind(u5[, vars, with = F], nn[, vars, with = F])

# Merge on location_name
merge.dt <- merge(out.dt, loc.table[, .(location_id, location_name)])
setnames(merge.dt, c("location_name", "year_id"), c("location", "year"))
merge.dt[, age := ifelse(age_group_id == 1, "Under 5", "Neonatal")]
merge.dt[, c("location_id", "age_group_id") := NULL]
# Write
write.csv(merge.dt, "/home/j/temp/aucarter/prob_death.csv", row.names = F)