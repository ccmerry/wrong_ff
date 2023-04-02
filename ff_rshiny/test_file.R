library(nflfastR)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

stats <- load_player_stats()
dplyr::glimpse(stats)
head(stats)
colnames(stats)
p_name <- c(unique(stats[["player_name"]]))
#order_stats <- stats[order(stats["player_name"]),]
#head(order_stats)

#p_data <- stats %>% filter(stats['player_name'] == "T.Brady")
#p_data_week <- p_data$week
#p_data_passing_yards <- p_data$passing_yards
#p_data_week
#p_data_passing_yards

#plot(p_data$week, p_data$passing_yards)

listc <- c("Tom Brady", "Aaron Rodgers")
#listc[1]

p_data <- stats %>% filter(stats$player_display_name %in% listc)


print("here")

test <- p_data %>%
  group_by(p_data$player_display_name) %>%
  summarise_if(is.numeric, funs(sum))
  #summarise(Freq = sum("passing_yards", na.rm=TRUE))

colnames(test)[1] <- "new_name"
colnames(test)

ggplot() +
  geom_col(data = test, aes(x = new_name, y = passing_yards))
test

df_grp_region = p_data %>% group_by("player_display_name")  %>%
  summarise(total_sales = sum("passing_yards"),
            .groups = 'drop')

df_grp_region
