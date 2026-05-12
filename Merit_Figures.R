# Title: Whose Merit, Which Redistribution? Figures
# This script contains all code necessary to replicate the figures in the main draft.
# Author: Lívio Silva-Muller
# Date: March 2026

# 0 Packages, help functions, and data------------------------------------------

library(tidyverse)

merit_dat <- readRDS("merit_dat.rds")

theme_plots <- function(x) {
  theme(text = element_text(size=12,  family="Times"),
        panel.background = element_rect("white", "black", .5, "solid"),
        panel.grid.major = element_line(color = "grey", linewidth = 0.2,
                                        linetype = "solid"),
        axis.text = element_text(color = "black", size = 10),
        title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 9, face = "plain"))
} # creates baseline for plot layout

# Figure 1: Counterfactual and observed correlation-----------------------------

cor_labels <- merit_dat %>%
  group_by(country) %>%
  summarise(cor = cor(elite_deserving, poor_undeserving, use = "complete.obs")) %>%
  mutate(
    label = paste0("r = ", round(cor, 2)),
    x = 1.5,
    y = 9.5)

# Then, add to your plot

merit_dat <- merit_dat %>%
  mutate(
    x_jit = poor_undeserving + runif(n(), -0.2, 0.2),
    y_jit = elite_deserving + runif(n(), -0.2, 0.2),
    proj = (x_jit + y_jit) / 2,
    x_proj = proj,
    y_proj = proj
  )

merit_dat %>%
  ggplot(aes(x = x_jit, y = y_jit, color = country)) +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +  # fictitious perfect correlation line
  geom_point(alpha = 0.75, size = 2) +  # jittered points
  geom_text(data = cor_labels, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, size = 4.5) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(
    limits = c(0, 11),
    breaks = 1:10,        # show only 1–10 on the axis
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 11),
    breaks = 1:10,        # show only 1–10 on the axis
    expand = c(0, 0)
  ) +
  labs(
    y = "Hard-working Elites", x = "Effortless Poor",
    # title = "Perceptions of Merit: Elites vs the Poor",
    # subtitle = "Higher levels in effortless poor suggest that poverty is due to lack of effort rather than lack of opportunities.\nHigher levels in hard-working elites suggest that success is due to hard-work rather than unique opportunities. ",
    # caption = "Observations were slightly jittered to improve visualization."
  ) +
  theme(
    panel.background = element_rect("white", "black", 0.5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 14),
    title = element_text(color = "black", size = 15, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    plot.subtitle = element_text(color = "black", size = 15, face = "plain"),
    legend.key.size = unit(1, "cm"),
    legend.position = "none",
    strip.text = element_text(size = 15)
  ) +
  facet_wrap(~country)

#ggsave("Figure1.png", width = 10, height = 5, dpi = 600, bg = "white")



# Figure 2: Means by sector-----------------------------------------------------

merit_long <- merit_dat %>%
  filter(country %in% c("Brazil", "South Africa"),
         !is.na(sector),
         !is.na(elite_deserving),
         !is.na(poor_undeserving)) %>%
  mutate(
    sector = factor(
      sector,
      levels = c(
        "Legislative Elites",
        "Bureaucratic Elites",
        "Business Elites"
      )
    )
  ) %>%
  pivot_longer(
    cols = c(elite_deserving, poor_undeserving),
    names_to = "merit_type",
    values_to = "score"
  )%>%
  mutate(
    merit_type = dplyr::recode(
      merit_type,
      elite_deserving  = "Hard-working Elites",
      poor_undeserving  = "Effortless Poor"
    )
  )

merit_summary <- merit_long %>%
  group_by(country, sector, merit_type) %>%
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    sd_score   = sd(score, na.rm = TRUE),
    n          = sum(!is.na(score)),
    se_score   = sd_score / sqrt(n),
    .groups    = "drop"
  )

ggplot(
  merit_summary,
  aes(
    x = sector,
    y = mean_score,
    colour = merit_type,
    shape  = merit_type,
    group  = merit_type
  )
) +
  geom_point(
    position = position_dodge(width = 0.4),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = mean_score - 1.96 * se_score,
      ymax = mean_score + 1.96 * se_score
    ),
    width = 0.2,
    position = position_dodge(width = 0.4)
  ) +
  scale_y_continuous(
    limits = c(1, 10),
    breaks = 1:10
  ) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "",
    y = "Mean merit score (1–10)",
    # title = "Mean Elite and Poor Merit Perceptions Across Sectors",
    # subtitle = "Points show group means; error bars indicate 95% confidence intervals (±1.96 × SE)"
  ) +
  theme(
    panel.background = element_rect("white", "black", 0.5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 14),
    title = element_text(color = "black", size = 15, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    plot.subtitle = element_text(color = "black", size = 15, face = "plain"),
    legend.key.size = unit(1, "cm"),
    legend.position = "top",
    strip.text = element_text(size = 15)
  ) +
  facet_wrap(~country)

#ggsave("Figure2.png", width = 14, height = 6, dpi = 600, bg = "white")

# Figure 3: Sample composition by race and sector----------------------------

race_sector_summary <- merit_dat %>%
  filter(!is.na(sector),
         !is.na(white),
         !is.na(country)) %>%
  mutate(
    race_group = if_else(white == 1, "White", "Non-white"),
    sector = factor(sector,
                    levels = c("Legislative Elites",
                               "Bureaucratic Elites",
                               "Business Elites"))
  ) %>%
  count(country, sector, race_group, name = "n") %>%
  group_by(country, sector) %>%
  mutate(
    pct = n / sum(n)
  ) %>%
  ungroup()


ggplot(race_sector_summary,
       aes(x = sector,
           y = n,
           fill = race_group)) +
  
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black")+

  
  geom_text(aes(label = percent(pct, accuracy = 1)),
            position = position_dodge(width = 0.8),
            width = 0.7,
            vjust = -0.3,
            size = 4.5)+
  
  scale_fill_grey(start = 0.85, end = 0.3, name = "Race") +

  labs(
    x = "",
    y = "n of elites",
    # title = "Racial composition of elite sectors",
    # subtitle = "Bars are labelled with percentage within sector."
  ) +
  theme(
    panel.background = element_rect("white", "black", 0.5, "solid"),
    panel.grid.major = element_line(color = "grey", size = 0.3, linetype = "solid"),
    axis.text = element_text(color = "black", size = 14),
    title = element_text(color = "black", size = 15, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    plot.subtitle = element_text(color = "black", size = 15, face = "plain"),
    legend.key.size = unit(1, "cm"),
    legend.position = "top",
    strip.text = element_text(size = 15)
  ) +
  facet_wrap(~country)

#ggsave("Figure3.png", width = 14, height = 6, dpi = 600, bg = "white")

