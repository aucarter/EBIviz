## Rwanda Longform Plots

library(ggplot2); library(data.table); library(ggrepel)

dt <- fread("data/prepped/prob_death.csv")
mort.dt <- fread("data/prepped/mort.csv")

## U5M line plot
plot.dt <- dt[location == "Rwanda" & age == "Under 5" & year %in% 2000:2015]
gg <- ggplot(data = plot.dt, aes(x = year, y = mean * 1000)) + 
  geom_line() + 
  geom_text(data = plot.dt[year %% 5 == 0], aes(label = round(mean * 1000), y = mean * 1000 + 10)) +
  ylim(c(0, 200)) +
  xlab("Year") +
  ylab("Deaths per 1000 live births") +
  ggtitle("Under-5 Mortality Rate in Rwanda") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_bw()
print(gg)

## Neonatal mortality line plot
plot.dt <- dt[location == "Rwanda" & age == "Neonatal" & year %in% 2000:2015]
gg <- ggplot(data = plot.dt, aes(x = year, y = mean * 1000)) + 
  geom_line() + 
  geom_text(data = plot.dt[year %% 5 == 0], aes(label = round(mean * 1000), y = mean * 1000 + 4)) +
  ylim(c(0, 50)) +
  xlab("Year") +
  ylab("Deaths per 1000 live births") +
  ggtitle("Neonatal Mortality Rate in Rwanda") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_bw()
print(gg)

## Bar plot split by neonatal and other
cast.dt <- dcast(dt, location + year ~ age, value.var = "mean")
cast.dt[, mean := get("Under 5") - Neonatal]
cast.dt[, c("age", "Under 5", "Neonatal") := .("Infant and Child", NULL, NULL)]
all.dt <- rbind(dt, cast.dt)

plot.dt <- all.dt[location == "Rwanda" & age != "Under 5" & year %in% 2000:2015 & year %% 5 == 0]
gg <- ggplot(data = plot.dt, aes(x = year, y = mean * 1000)) + geom_bar(aes(fill = age), stat = "identity") +
  geom_text(aes(label = round(mean * 1000)), position = position_stack(vjust = 0.5)) +
  xlab("Year") + ylab("Deaths per 1000 live births") +
  theme(legend.position = "bottom", legend.title = element_blank())+
  ggtitle("Neonatal and Non-Neonatal Mortality Rate in Rwanda") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_bw()
print(gg)

## Proportion of U5M from each cause
all.cause <- copy(mort.dt[cause == "All causes"])
setnames(all.cause, "val", "all")
percent.dt <- merge(copy(mort.dt[cause != "All causes"]), 
                    all.cause[, .(measure, location, age, metric, year, all)], 
                    by = c("measure", "location", "age", "metric", "year"))
percent.dt[, prop := (val / all) * 100]
plot.dt <- percent.dt[location == "Rwanda" & year %in% 1995:2015 & year %% 5 == 0 & measure == "Deaths" & metric == "Number" & age == "Under 5"]
gg <- ggplot(data = plot.dt, aes(x = year, y = prop, fill = cause, label = round(prop))) + 
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5)) +
  xlab("Year") +
  ylab("Percent of Under-five deaths") +
  ggtitle("Under-five causes of death in Rwanda over time, percentage of total U5M") +
  guides(fill=guide_legend(title="Cause"))
print(gg)

## 