### Data Prep
library(data.table)

## Paths
# In
data.dir <- "data/raw/"
data.path <- paste0(data.dir, "GBD16_results_all2.csv")
ebi.path <- paste0(data.dir, "20180731_STAT Compiler All Countries (Prepped).csv")
dpt.path <- paste0(data.dir, "dpt3.csv")

# Out
out.dir <- "data/prepped/"
mort.out <- paste0(out.dir, "mort.csv")
stat.out <- paste0(out.dir, "stat.csv")
ebi.out <- paste0(out.dir, "ebi.csv")
plot.dir <- "C:/Users/AustinC/OneDrive - bgC3/Documents/plots/U5M_plots/ebi_summary/"
dir.create(plot.dir, showWarnings = F)

### Code
## Cause-specific mortality
# Read data
dt <- fread(data.path)
birth.dt <- fread(paste0(data.dir, "births.csv"))[sex_id == 3]
setnames(birth.dt, c("year_id", "location_name"), c("year", "location"))

# Calculate other category
all.dt <- copy(dt[cause == "All causes"])
combined.dt <- dt[cause != "All causes", .(sum_val = sum(val)), by = .(age, metric, year, measure, location)]
merge.dt <- merge(all.dt, combined.dt, by = c("age", "metric", "year", "measure", "location"))                 
merge.dt[, diff := val - sum_val]
merge.dt[, c("val", "upper", "lower", "sum_val") := NULL]
setnames(merge.dt, "diff", "val")
merge.dt[, cause := "Other"]
bound.dt <- rbind(dt, merge.dt, fill = T)

# Combine diphtheria, whooping cough, tetanus into DPT
dpt.causes <- c("Diphtheria", "Whooping cough", "Tetanus")
dpt.hold <- dt[cause %in% dpt.causes]
dpt.dt <- dt[cause %in% dpt.causes, .(val = sum(val)), by = .(age, metric, year, measure, location)]
dpt.dt[, cause := "DPT"]
bound.dt <- rbind(bound.dt[!(cause %in% dpt.causes)], dpt.dt, fill = T)

# Switch rate to have births in denominator
births.merge <- merge(bound.dt[metric == "Number"], birth.dt[, .(location, year, population)], by = c("year", "location"))
births.merge[, val := val / population]
births.merge[, population := NULL]
births.merge[, metric := "Rate"]
bound.dt <- rbind(bound.dt[metric == "Number"], births.merge)

# Calculate 5 year averages
bound.dt[year %%5 != 0, period := paste0((year - year%%5), "-", (year - year%%5 + 5))]
bound.dt[year %%5 == 0, period := paste0((year - year%%5 - 5), "-", (year - year%%5))]
period.dt <- bound.dt[, .(five_avg = mean(val)), by = .(age, metric, period, measure, location, cause)]
mort.dt <- merge(bound.dt, period.dt)

write.csv(mort.dt, mort.out, row.names = F)



## EBI
ebi.dt <- fread(ebi.path)

# DPT3 coverage from WHO
dpt.dt <- fread(dpt.path, header = T)
melt.dpt <- melt(dpt.dt, id.vars = "Cname", variable.name = "year")
setnames(melt.dpt, "Cname", "location")
melt.dpt[, c("Category", "Indicator") := .("DPT", "DPT3 Vaccine")]
ebi.dt <- rbind(ebi.dt, melt.dpt)

write.csv(ebi.dt, ebi.out, row.names = F)

### Stats
# Change between 2000 and 2015
start.val <- mort.dt[metric == "Rate" & age == "Under 5" & year == start.year & measure == "Deaths" & location == cloc, .(cause, val)]
setnames(start.val, "val", "start_val")
end.val <- mort.dt[metric == "Rate" & age == "Under 5" & year == end.year & measure == "Deaths" & location == cloc, .(cause, val)]
setnames(end.val, "val", "end_val")
change.dt <- merge(start.val, end.val)
change.dt[, change := (start_val - end_val) / start_val * 100]

# Rankings
start.val[order(start_val)]

## Table of improvement given uncertainty
bound2.dt <- rbind(mort.dt, dpt.hold, fill = T)
yr1 <- start.year
yr2 <- end.year
subset.mort <- bound2.dt[metric == "Rate" & age == "Under 5" & year %in% c(yr1, yr2) & measure == "Deaths", .(location, cause, year, val, upper, lower)]
# mean.dt <- dcast(subset.mort, location + cause ~ year, value.var = "val")
subset.mort[, cast_val := ifelse(year == start.year, lower, upper)]
cast.dt <- dcast(subset.mort, location + cause ~ year, value.var = "cast_val")
cast.dt[, sig := ifelse(get(as.character(start.year)) > get(as.character(end.year)), 1, 0)]
cast.dt[is.na(sig), sig := 0]

write.csv(cast.dt, stat.out, row.names =  F)