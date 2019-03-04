library(data.table); library(ggplot2); library(scales)
data.dir <- "data/prepped/"
dt16 <- fread(paste0(data.dir, "mort16.csv"))
dt16[, source := "GBD16"]
dt17 <- fread(paste0(data.dir, "mort17.csv"))
dt17[, source := "GBD17"]
dt17[, c("metric_id", "cause_id", "age_id", "sex_id", "location_id", "measure_id") := NULL]
setnames(dt17, "sex_name", "sex")
dt <- rbind(dt16, unique(dt17), fill = T)
neonat.dt <- dt[age %in% c("Early Neonatal", "Late Neonatal")]
neonat.dt <- neonat.dt[, .(val = sum(val)), by =  c("location", "cause", "metric", "source", "year")]
subset.dt <- dt[year %in% c(2000, 2015) & measure == "Deaths"]
change.dt <- copy(subset.dt)
cast.dt <- dcast(change.dt, location + age + cause + metric + source ~ year, value.var = "val")
cast.dt[, val := `2015` - `2000`]
cast.dt[, year := 999]
cast.dt[, c("2000", "2015") := NULL]
comb.dt <- rbind(subset.dt, cast.dt, fill = T)

plot.dt <- dcast(comb.dt, location + age + cause + metric + year ~ source, value.var = "val")
plot.dt[, diff := GBD17 - GBD16]
plot.dt[, rel_diff := 100 * diff / GBD16]
plot.dt <- plot.dt[!is.na(diff) & age == "Under 5" & cause != "All causes"]
plot.dt[, time := ifelse(year == 999, "2000-2015", year)]
plot.dt[rel_diff > 500, rel_diff := 500]
plot.dt[rel_diff < -500, rel_diff := -500]

# Plot deaths diff   
pdf("plots/gbd16v17_u5m_comp.pdf", width = 11, height = 8.5)
for(loc in unique(plot.dt$location)) {
  loc.dt <- plot.dt[location == loc & metric == "Number"]
  gg <- ggplot(data = loc.dt, aes(y = cause, x = rel_diff, color = time)) +
    geom_point(aes(size = abs(GBD16)), alpha=.5) +
    # shape = as.factor(metric), 
    geom_vline(xintercept = 0) +
    scale_color_manual(values = c("firebrick3", "gold1", "palegreen3")) +
    # scale_shape_manual(values = c(17, 16)) +
    theme_bw() +
    labs(color="Time Period", size = "Deaths in GBD16") +
    ylab("") +
    scale_x_continuous("Relative Difference (%)", labels = comma) +
    ggtitle(paste0(loc, ": Under-5 Deaths GBD 16 vs GBD 17 Relative Comparison "))
  print(gg)
}
dev.off()

# Plot deaths ratio
gg <- ggplot(data = group.dt, aes(y = location_name, x = ratio, color = as.factor(year_id))) +
  geom_point(aes(shape = as.factor(sex_name)), size = 3, alpha=.5) +
  geom_vline(xintercept = 1) +
  scale_color_manual(values = c("firebrick3", "gold1", "palegreen3", "royalblue3", "purple")) +
  scale_shape_manual(values = c(17, 16)) +
  theme_bw() +
  labs(color="", shape = "") +
  ylab("") +
  scale_x_continuous("Difference", labels = comma) +
  ggtitle(paste0(c.metric, " Ratio Comparison - GBD 16"))
print(gg)