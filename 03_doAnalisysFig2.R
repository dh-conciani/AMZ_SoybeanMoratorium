## translate table 
## dhemerson.costa@ipam.org.br
library(dplyr)
library(ggplot2)
library(patchwork)

## read table
data <- read.csv('collection101-biomes-state-prodesMask-v3.csv', sep=';', dec='.') %>%
  select(-class_id)

## remove undesired data  -
data_filtered <- data %>%
  filter(
    !PRODES_mask %in% c(1, 2, 3), # nodata, water and non-forest
    biome != "Cerrado",   #  also remove cerrado biome
    year > 1998
  )

## aggregate for the biome
data_agg <- data_filtered %>%
  group_by(PRODES_mask, year, biome) %>%
  summarise(
    area = sum(area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    deforestation_year = PRODES_mask,
    soybean_year = year
  )

## compute soybean increases per prodes_mask
data_increase <- data_agg %>%
  arrange(deforestation_year, biome, soybean_year) %>%
  group_by(deforestation_year, biome) %>%
  mutate(
    area_increase = area - lag(area)
  ) %>%
  ungroup()

## compute time lag between deforestation and soybean establishement
data_increase <- data_increase %>%
  mutate(
    years_after_deforestation = soybean_year - deforestation_year
  ) %>%
  mutate(
    lag_time = soybean_year - deforestation_year,
    lag_time_class = case_when(
      lag_time == 0 ~ "Time Lag 0 year",
      lag_time %in% c(1, 2) ~ "Time Lag 1-2 years",
      lag_time >= 3 ~ "Deforestation Time Lag >2 years",
      TRUE ~ NA_character_
    )
  ) %>% na.omit()

## group by
data_stats <- data_increase %>%
  mutate(
    soybean_period = case_when(
      soybean_year >= 2000 & soybean_year <= 2004 ~ "2000-2004",
      soybean_year >= 2005 & soybean_year <= 2008 ~ "2005-2008",
      soybean_year >= 2009 & soybean_year <= 2012 ~ "2009-2012",
      soybean_year >= 2013 & soybean_year <= 2016 ~ "2013-2016",
      soybean_year >= 2017 & soybean_year <= 2020 ~ "2017-2020",
      soybean_year >= 2021 & soybean_year <= 2024 ~ "2021-2024",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(soybean_period)) %>%
  group_by(soybean_period, lag_time_class, biome) %>%
  summarise(
    total_area = sum(area, na.rm = TRUE),
    total_area_increase = sum(area_increase, na.rm = TRUE),
    .groups = "drop"
  )

## now, sum into inside and outside moratoria rules
data_rules <- data_stats %>%
  mutate(
    moratoria_group = case_when(
      lag_time_class %in% c("Time Lag 0 year",
                            "Time Lag 1-2 years") ~ "Outside Moratória Rules",
      lag_time_class == "Deforestation Time Lag >2 years" ~ "Inside Moratória Rules",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(moratoria_group)) %>%
  group_by(soybean_period, biome, moratoria_group) %>%
  summarise(
    total_area = sum(total_area, na.rm = TRUE),
    total_area_increase = sum(total_area_increase, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(soybean_period, biome) %>%
  mutate(
    percent_area = 100 * total_area / sum(total_area, na.rm = TRUE),
    percent_area_increase = 100 * total_area_increase / sum(total_area_increase, na.rm = TRUE)
  ) %>%
  ungroup()

## plot
fig2a <- ggplot(
  data = data_rules %>% 
    filter(moratoria_group == "Outside Moratória Rules") %>%
    mutate(
      soybean_period_label = recode(
        soybean_period,
        "2000-2004" = "2000 to\n2004",
        "2005-2008" = "2005 to\n2008",
        "2009-2012" = "2009 to\n2012",
        "2013-2016" = "2013 to\n2016",
        "2017-2020" = "2017 to\n2020",
        "2021-2024" = "2021 to\n2024"
      ),
      soybean_period_label = factor(
        soybean_period_label,
        levels = c(
          "2000 to\n2004", "2005 to\n2008", "2009 to\n2012",
          "2013 to\n2016", "2017 to\n2020", "2021 to\n2024"
        )
      ),
      label = paste0(
        round(percent_area_increase, 2), "%\n",
        round(total_area_increase / 1000, 0), " Kha"
      )
    ),
  mapping = aes(
    x = soybean_period_label,
    y = percent_area_increase,
    group = moratoria_group,
    colour = moratoria_group
  )
) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = label), vjust = -0.6, colour = "black", size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
  scale_colour_manual(values = c("#DA640A")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Proportion of soybean expansion occurring\non land deforested within the previous\n2 years")


fig2a

###############################
## FIG 2B
data_increase_plot <- data_increase %>%
  mutate(
    lag_time_class = factor(
      lag_time_class,
      levels = c(
        "Time Lag 0 year",
        "Time Lag 1-2 years",
        "Deforestation Time Lag >2 years"
      )
    )
  )

fig2b <- ggplot(
  data = data_increase_plot,
  mapping = aes(
    x = soybean_year,
    y = area_increase / 1000,
    fill = lag_time_class
  )
) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(
    "Deforestation Time Lag",
    labels = c("0 year", "1-2 years", ">2 years"),
    values = c("red", "orange", "gray")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.03, 0.97),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 11)
  ) +
  xlab(NULL) +
  ylab("Soybean Expansion (Kha)") +
  geom_vline(
    xintercept = 2008,
    colour = "black",
    linetype = "dashed",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 2008,
    y = Inf,
    label = "Soybean\nMoratorium",
    colour = "black",
    vjust = 1.5,
    hjust = -0.1,
    size = 4
  )

fig2b       

########################################################################################
## FIG2C

fig2c <- ggplot(
  data = data_increase,
  mapping = aes(
    x = soybean_year,
    y = area_increase / 1000,
    fill = years_after_deforestation
  )
) +
  geom_bar(stat = "identity") +
  
  geom_vline(
    xintercept = 2008,
    colour = "black",
    linetype = "dashed",
    linewidth = 1
  ) +
  
  annotate(
    "text",
    x = 2008,
    y = Inf,
    label = "Soybean\nMoratorium",
    colour = "black",
    vjust = 1.5,
    hjust = -0.1,
    size = 4
  ) +
  
  scale_fill_gradientn(
    colours = c("red", "orange", "forestgreen"),
    name = "Deforestation age\n(years)"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = c(0.03, 0.97),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  ) +
  
  ylab("Soybean Expansion (Kha)") +
  xlab(NULL)

fig2c

### DIAGRAMAR
legend_small <- theme(
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 11),
  legend.key.size = unit(0.55, "cm"),
  legend.spacing.y = unit(0.20, "cm")
)

fig2b <- fig2b + legend_small
fig2c <- fig2c + legend_small

empty_plot <- plot_spacer()

panel_fig2 <- (fig2a + fig2b) /
  (fig2c + plot_spacer()) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 14)
    )
  )

## exportar
ggsave(
  filename = "fig2.png",
  plot = panel_fig2,
  width = 16,
  height = 10,
  units = "in",
  dpi = 600,
  bg = "white"
)

panel_fig2

