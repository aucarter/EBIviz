## Pull U5M and NN Mortality with births as denom straight from the life table
# Setup
source("/home/j/temp/central_comp/libraries/current/r/get_location_metadata.R")
source("/home/j/temp/central_comp/libraries/current/r/get_age_metadata.R")
library(mortdb, lib = "/home/j/WORK/02_mortality/shared/r")

# location table
loc.table <- data.table(get_location_metadata(location_set_id = 21))
loc.names <- c("Rwanda", "Ethiopia", "Senegal", "Peru", "Nepal", "Cambodia", "Bangladesh")
loc.list <- loc.table[level %in% 3:2, location_id]

# Mortality rates
u5 <- get_mort_outputs("with shock life table", "estimate", age_group_id = 1:3, location_ids = loc.list, life_table_parameter_id = 3, gbd_year = 2017, sex_id =3)
enn <- u5[age_group_id == 2]
setnames(enn, "mean", "enn")
lnn <- u5[age_group_id == 3]
setnames(lnn, "mean", "lnn")
nn <- merge(enn, lnn, by = c("location_id", "sex_id", "year_id"))
nn[, mean := 1 - (1 - lnn)*(1 - enn)]
nn[, age_group_id := 42]

# Combine
vars <- c("location_id", "year_id", "age_group_id", "mean")
out.dt <- rbind(u5[age_group_id == 1, vars, with = F], nn[, vars, with = F])

# Merge on location_name
merge.dt <- merge(out.dt, loc.table[, .(location_id, location_name)])
setnames(merge.dt, c("location_name", "year_id"), c("location", "year"))
merge.dt[, age := ifelse(age_group_id == 1, "Under 5", "Neonatal")]
merge.dt[, c("location_id", "age_group_id") := NULL]
# Write
write.csv(merge.dt, "/home/j/temp/aucarter/prob_death.csv", row.names = F)