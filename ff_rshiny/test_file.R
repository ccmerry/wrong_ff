library(nflfastR)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

stats <- load_player_stats()
dplyr::glimpse(stats)
head(stats)
colnames(stats)
p_name <- c(unique(stats[["player_name"]]))
p_name
order_stats <- stats[order(stats["player_name"]),]
head(order_stats)

p_data <- stats %>% filter(stats['player_name'] == "T.Brady")
p_data_week <- p_data$week
p_data_passing_yards <- p_data$passing_yards
p_data_week
p_data_passing_yards

plot(p_data$week, p_data$passing_yards)
