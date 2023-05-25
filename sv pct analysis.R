library(tidyverse)
library(extrafont)

theme_awesome <- function(font) {
  
  theme_minimal() +
    theme(plot.title = element_text(hjust = 0, face = "bold", family = font, size = 45, margin = margin(1, 0, 20, 0)),
          axis.title = element_text(face = "bold", family = font, size = 30),
          axis.text = element_text(family = font, face = "bold", size = 23),
          axis.title.x = element_text(margin = margin(35, 0, 0, 0)),
          axis.title.y = element_text(margin = margin(0, 35, 0, 0)),
          plot.margin = margin(25, 15, 25, 15),
          legend.title = element_text(family = font, face = "bold", size = 18),
          legend.title.align = 0,
          legend.text = element_text(family = font, face = "bold", size = 15),#, margin = margin(1, 1, 1, 100)),
          legend.key.size = unit(1, "cm"),
          legend.margin = margin(0, 30, 30, 20),
          #axis.line = element_blank(),
          axis.ticks.length = unit(0.7, "cm"),
          #panel.grid.major.y = element_blank(),
          #panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          strip.text = element_text(family = font, face = "bold", size = 18, margin = margin(0, 0, 10, 0)),
          strip.background = element_rect(fill = "white", color = "black"),
          plot.caption = element_text(size = 17, face = "bold", family = font, hjust = 0.95),
          plot.subtitle = element_text(size = 26, face = "bold", family = font, margin = margin(0, 0, 30, 0)))
  
}

all_sit <- read_delim("goalie_all_situations_2007_2023.csv", delim = "\t", col_types = cols())
five_v_five <- read_delim("goalie_5v5_2007_2023.csv", delim = "\t", col_types = cols())

# filtering all situations goalies to 5v5 TOI >= 500
all_sit <- all_sit %>%
  inner_join(select(filter(five_v_five, TOI >= 500), Season, Player), by = c("Season", "Player"))

# filtering 5v5 goalies to 5v5 TOI >= 500
five_v_five <- five_v_five %>%
  filter(TOI >= 500)

all_sit_vs_5v5 <- five_v_five %>%
  select(season = Season, player = Player, hd_5v5_sv_pct = `HDSV%`) %>%
  mutate(season_plus_10001 = season + 10001) %>%
  inner_join(all_sit, by = c("player" = "Player", "season_plus_10001" = "Season")) %>%
  select(season, player, hd_5v5_sv_pct, sv_pct = `SV%`) %>%
  mutate(across(.cols = c(sv_pct, hd_5v5_sv_pct), as.numeric))

cor(all_sit_vs_5v5$sv_pct, all_sit_vs_5v5$hd_5v5_sv_pct)

all_sit_vs_5v5 %>%
  ggplot(aes(x = hd_5v5_sv_pct, y = sv_pct)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.001)) +
  annotate("text", x = 0.910, y = 0.900, label = "r = 0.0977", family = "Tw Cen MT Condensed", size = 8) +
  labs(x = "5v5 HDSV% Year n", y = "All Sit. SV% Year n+1", subtitle = "Natural Stat Trick, 2007-2023, Regular Season, Minimum 500 5v5 Minutes", title = "Is Normal SV% the Best Predictor of Future SV%?") +
  theme_awesome(font = "Tw Cen MT Condensed")

ggsave("sv_pct_hd_5v5.png", dpi = 330, height = (2.72 * 4), width = (4.17 * 4))

all_sit_vs_all_sit <- all_sit %>%
  select(season = Season, player = Player, sv_pct = `SV%`) %>%
  mutate(season_plus_10001 = season + 10001) %>%
  inner_join(all_sit, by = c("player" = "Player", "season_plus_10001" = "Season")) %>%
  select(season, player, sv_pct, sv_pct_plus_1 =`SV%`) %>%
  mutate(across(.cols = c(sv_pct, sv_pct_plus_1), as.numeric))

cor(all_sit_vs_all_sit$sv_pct, all_sit_vs_all_sit$sv_pct_plus_1)

all_sit_vs_all_sit %>%
  ggplot(aes(x = sv_pct, y = sv_pct_plus_1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.001)) +
  annotate("text", x = 0.935, y = 0.890, label = "r = 0.191", family = "Tw Cen MT Condensed", size = 8) +
  labs(x = "All Sit. SV% Year n", y = "All Sit. SV% Year n+1", subtitle = "Natural Stat Trick, 2007-2023, Regular Season, Minimum 500 5v5 Minutes", title = "Is Normal SV% the Best Predictor of Future SV%?") +
  theme_awesome(font = "Tw Cen MT Condensed") 

ggsave("sv_pct_all.png", dpi = 330, height = (2.72 * 4), width = (4.17 * 4))

