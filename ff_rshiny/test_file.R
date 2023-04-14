library(nflfastR)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(tidyr)
library(magrittr)
library(tidyverse)

stats <- load_player_stats()
#dplyr::glimpse(stats)
head(stats)
p_names <- c(unique(stats[["player_display_name"]]))
sort(p_names)
set.seed(1)

stats %<>% mutate(num_rand = substr(player_id, nchar(player_id)-1+1, nchar(player_id)))
even_num <- c(2,4,6,8,0)
stats$even_odd <- ifelse(stats$num_rand %in% even_num, 1, -1)

stats$rnorm <- rnorm(nrow(stats), mean=0, sd=40)
stats$newrow <- sample(15, size = nrow(stats), replace = TRUE)
stats$abs <- (abs(stats$rnorm)/1000 * stats$even_odd) + 1

stats$single_change <- ifelse(stats$abs > .06, 1, 0)
stats <- stats %>% mutate(new_passing_yards = round(stats$passing_yards * stats$abs))
stats <- stats %>% filter(week < 18)
stats[15:20,5:10]



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

num_df <- 
  stats %>% 
  charToRaw %>% 
  as.numeric %>% 
  '-'(96) %>% 
  paste(collapse = '')

listc <- c("Adam Thielen")
#listc[1]

p_data <- stats %>% filter(stats$player_display_name %in% listc)
sum(p_data$receiving_air_yards,na.rm=TRUE)

print("here")

test <- p_data %>%
  group_by(p_data$player_display_name) %>%
  summarise_if(is.numeric, funs(sum))
  #summarise(Freq = sum("passing_yards", na.rm=TRUE))

colnames(test)[1] <- "new_name"

c()

long <- test %>% 
  pivot_longer(
    cols = "season":"fantasy_points_ppr", 
    names_to = "stats",
    values_to = "value"
  )

col_l <- c("completions","passing_yards","passing_tds","passing_first_downs",
           "interceptions","rushing_tds","receptions","rushing_yards",
           "targets","fantasy_points","fantasy_points_ppr")

long_f <- long %>%
  filter(stats %in% col_l)

head(long_f)

sg <- long_f %>% 
  group_by(stats) %>% 
  mutate(perc = value/sum(value))

head(sg)

sum_group <- long_f %>%
  sum(value) %>%       
  group_by(stats) %>%
  mutate(pct= prop.table(n) * 100)

head(sum_group)

ggplot() +
  geom_col(data = test, aes(x = new_name, y = passing_yards))
test


df_grp_region
