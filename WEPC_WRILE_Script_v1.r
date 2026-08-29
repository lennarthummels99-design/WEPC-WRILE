#### Weighted Election-wide Party Change (WEPC) and Weighted RILE Change (WRILE)
#### Data: MARPOR MPDS2025a and Democratic Electoral Systems v5.0
########################################################################################################

library(dplyr)
library(ggplot2)

#Data
#############################
mp_raw <- read.csv(
  "MPDataset_MPDS2025a.csv"
)
es_raw <- read.csv(
  "es_data-v5_0.csv"
)

#Filtering
#############################
mp_filtered <- mp_raw %>%
  filter(
    date >= 198001,
    pervote >= 1
  )

issue_cols <- grep(
  "^per[0-9]+$",
  names(mp_filtered),
  value = TRUE
)

change_cols <- paste0(
  "change_",
  issue_cols
)


#Issue change
#############################
mp_sorted <- mp_filtered %>%
  arrange(
    countryname,
    party,
    date
  )

mp_changed <- mp_sorted %>%
  group_by(
    countryname,
    party
  ) %>%
  mutate(
    across(
      all_of(issue_cols),
      ~ abs(. - lag(.)),
      .names = "change_{.col}"
    ),
    change_rile = abs(rile - lag(rile))
  ) %>%
  ungroup()

mp_total <- mp_changed %>%
  mutate(
    total_issue_change = rowSums(
      across(
        all_of(change_cols)
      ),
      na.rm = TRUE
    ) / 2
  )

#WEPC
#############################
mp_epc_valid <- mp_total %>%
  filter(
    !is.na(change_per101)
  )

epc_by_election <- mp_epc_valid %>%
  group_by(
    countryname,
    date
  ) %>%
  summarise(
    weighted_EPC = sum(total_issue_change * (pervote / 100)),
    n_parties = n(),
    .groups = "drop"
  )

epc_by_country <- epc_by_election %>%
  group_by(
    countryname
  ) %>%
  summarise(
    mean_weighted_EPC = mean(weighted_EPC),
    n_elections_EPC = n(),
    n_changes_EPC = sum(n_parties),
    .groups = "drop"
  )


#WRILE
#############################
mp_rile_valid <- mp_total %>%
  filter(
    !is.na(change_rile)
  )

rile_by_election <- mp_rile_valid %>%
  group_by(
    countryname,
    date
  ) %>%
  summarise(
    weighted_RILE_change = sum(change_rile * (pervote / 100)),
    n_parties = n(),
    .groups = "drop"
  )

rile_by_country <- rile_by_election %>%
  group_by(
    countryname
  ) %>%
  summarise(
    mean_weighted_RILE_change = mean(weighted_RILE_change),
    n_elections_RILE = n(),
    n_changes_RILE = sum(n_parties),
    .groups = "drop"
  )

#ENPP
#############################
es_named <- es_raw %>%
  mutate(
    countryname = case_when(
      country == "United States of America" ~ "United States",
      country == "West Germany" ~ "Germany",
      TRUE ~ country
    )
  )

es_filtered <- es_named %>%
  filter(
    year >= 1980,
    year <= 2020,
    !is.na(enpp)
  )

enpp_by_country <- es_filtered %>%
  group_by(
    countryname
  ) %>%
  summarise(
    mean_enpp = mean(enpp),
    n_elections_ENPP = n(),
    .groups = "drop"
  )

#Merge
#############################
measures_by_country <- epc_by_country %>%
  inner_join(
    rile_by_country,
    by = "countryname"
  ) %>%
  inner_join(
    enpp_by_country,
    by = "countryname"
  )

#Samples
#############################
extended_major <- c(
  "United States",
  "United Kingdom",
  "Canada",
  "Australia",
  "France"
)

extended_prop <- c(
  "Austria",
  "Belgium",
  "Denmark",
  "Finland",
  "Germany",
  "Greece",
  "Iceland",
  "Ireland",
  "Italy",
  "Luxembourg",
  "Netherlands",
  "Norway",
  "Portugal",
  "Spain",
  "Sweden",
  "Switzerland"
)

restricted_major <- c(
  "United States",
  "United Kingdom",
  "Canada"
)

restricted_prop <- c(
  "Austria",
  "Belgium",
  "Denmark",
  "Finland",
  "Germany",
  "Iceland",
  "Ireland",
  "Luxembourg",
  "Netherlands",
  "Norway",
  "Portugal",
  "Spain",
  "Sweden",
  "Switzerland"
)

extended_data <- measures_by_country %>%
  filter(
    countryname %in% c(
      extended_major,
      extended_prop
    )
  ) %>%
  mutate(
    system = ifelse(
      countryname %in% extended_major,
      "Majoritarian",
      "Proportional"
    )
  ) %>%
  arrange(
    system,
    countryname
  )

restricted_data <- measures_by_country %>%
  filter(
    countryname %in% c(
      restricted_major,
      restricted_prop
    )
  ) %>%
  mutate(
    system = ifelse(
      countryname %in% restricted_major,
      "Majoritarian",
      "Proportional"
    )
  ) %>%
  arrange(
    system,
    countryname
  )

#Case numbers
#############################
extended_base <- mp_changed %>%
  filter(
    countryname %in% c(
      extended_major,
      extended_prop
    )
  ) %>%
  mutate(
    system = ifelse(
      countryname %in% extended_major,
      "Majoritarian",
      "Proportional"
    )
  )

restricted_base <- mp_changed %>%
  filter(
    countryname %in% c(
      restricted_major,
      restricted_prop
    )
  ) %>%
  mutate(
    system = ifelse(
      countryname %in% restricted_major,
      "Majoritarian",
      "Proportional"
    )
  )

extended_overview <- extended_base %>%
  summarise(
    n_countries = n_distinct(countryname),
    n_elections = n_distinct(paste(countryname, date)),
    n_parties_unique = n_distinct(paste(countryname, party)),
    n_manifestos = n(),
    n_changes_wepc = sum(!is.na(change_per101)),
    n_changes_rile = sum(!is.na(change_rile)),
    first_date = min(date),
    last_date = max(date)
  )

restricted_overview <- restricted_base %>%
  summarise(
    n_countries = n_distinct(countryname),
    n_elections = n_distinct(paste(countryname, date)),
    n_parties_unique = n_distinct(paste(countryname, party)),
    n_manifestos = n(),
    n_changes_wepc = sum(!is.na(change_per101)),
    n_changes_rile = sum(!is.na(change_rile)),
    first_date = min(date),
    last_date = max(date)
  )

extended_by_system <- extended_base %>%
  group_by(
    system
  ) %>%
  summarise(
    n_countries = n_distinct(countryname),
    n_elections = n_distinct(paste(countryname, date)),
    n_parties_unique = n_distinct(paste(countryname, party)),
    n_manifestos = n(),
    n_changes_wepc = sum(!is.na(change_per101)),
    n_changes_rile = sum(!is.na(change_rile)),
    .groups = "drop"
  )

restricted_by_system <- restricted_base %>%
  group_by(
    system
  ) %>%
  summarise(
    n_countries = n_distinct(countryname),
    n_elections = n_distinct(paste(countryname, date)),
    n_parties_unique = n_distinct(paste(countryname, party)),
    n_manifestos = n(),
    n_changes_wepc = sum(!is.na(change_per101)),
    n_changes_rile = sum(!is.na(change_rile)),
    .groups = "drop"
  )

extended_by_country <- extended_base %>%
  group_by(
    system,
    countryname
  ) %>%
  summarise(
    n_elections = n_distinct(date),
    n_parties_unique = n_distinct(party),
    n_manifestos = n(),
    n_changes_wepc = sum(!is.na(change_per101)),
    first_date = min(date),
    last_date = max(date),
    .groups = "drop"
  )

restricted_by_country <- restricted_base %>%
  group_by(
    system,
    countryname
  ) %>%
  summarise(
    n_elections = n_distinct(date),
    n_parties_unique = n_distinct(party),
    n_manifestos = n(),
    n_changes_wepc = sum(!is.na(change_per101)),
    first_date = min(date),
    last_date = max(date),
    .groups = "drop"
  )

duplicate_party <- mp_filtered %>%
  count(
    countryname,
    party,
    date
  ) %>%
  filter(
    n > 1
  )

#Group means
#############################
extended_groups_epc <- extended_data %>%
  group_by(
    system
  ) %>%
  summarise(
    mean_value = mean(mean_weighted_EPC),
    sd_value = sd(mean_weighted_EPC),
    n_countries = n(),
    n_elections = sum(n_elections_EPC),
    n_changes = sum(n_changes_EPC),
    .groups = "drop"
  ) %>%
  mutate(
    measure = "WEPC",
    sample = "Extended sample"
  )

restricted_groups_epc <- restricted_data %>%
  group_by(
    system
  ) %>%
  summarise(
    mean_value = mean(mean_weighted_EPC),
    sd_value = sd(mean_weighted_EPC),
    n_countries = n(),
    n_elections = sum(n_elections_EPC),
    n_changes = sum(n_changes_EPC),
    .groups = "drop"
  ) %>%
  mutate(
    measure = "WEPC",
    sample = "Restricted sample"
  )

extended_groups_rile <- extended_data %>%
  group_by(
    system
  ) %>%
  summarise(
    mean_value = mean(mean_weighted_RILE_change),
    sd_value = sd(mean_weighted_RILE_change),
    n_countries = n(),
    n_elections = sum(n_elections_RILE),
    n_changes = sum(n_changes_RILE),
    .groups = "drop"
  ) %>%
  mutate(
    measure = "WRILE",
    sample = "Extended sample"
  )

restricted_groups_rile <- restricted_data %>%
  group_by(
    system
  ) %>%
  summarise(
    mean_value = mean(mean_weighted_RILE_change),
    sd_value = sd(mean_weighted_RILE_change),
    n_countries = n(),
    n_elections = sum(n_elections_RILE),
    n_changes = sum(n_changes_RILE),
    .groups = "drop"
  ) %>%
  mutate(
    measure = "WRILE",
    sample = "Restricted sample"
  )

#T-tests
#############################
extended_ttest_epc <- t.test(
  mean_weighted_EPC ~ system,
  data = extended_data
)

restricted_ttest_epc <- t.test(
  mean_weighted_EPC ~ system,
  data = restricted_data
)

extended_ttest_rile <- t.test(
  mean_weighted_RILE_change ~ system,
  data = extended_data
)

restricted_ttest_rile <- t.test(
  mean_weighted_RILE_change ~ system,
  data = restricted_data
)

#Correlations
#############################
extended_pearson <- cor.test(
  extended_data$mean_weighted_EPC,
  extended_data$mean_weighted_RILE_change,
  method = "pearson"
)

extended_spearman <- cor.test(
  extended_data$mean_weighted_EPC,
  extended_data$mean_weighted_RILE_change,
  method = "spearman"
)

restricted_pearson <- cor.test(
  restricted_data$mean_weighted_EPC,
  restricted_data$mean_weighted_RILE_change,
  method = "pearson"
)

restricted_spearman <- cor.test(
  restricted_data$mean_weighted_EPC,
  restricted_data$mean_weighted_RILE_change,
  method = "spearman"
)

#Plots
#############################
plot_data <- extended_data %>%
  mutate(
    in_restricted = countryname %in% c(
      restricted_major,
      restricted_prop
    ),
    country_label = ifelse(
      in_restricted,
      countryname,
      paste0("(", countryname, ")")
    ),
    fill_group = case_when(
      system == "Majoritarian" & in_restricted ~ "Majoritarian",
      system == "Majoritarian" & !in_restricted ~ "Majoritarian (extended only)",
      system == "Proportional" & in_restricted ~ "Proportional",
      TRUE ~ "Proportional (extended only)"
    )
  )

plot_order <- plot_data %>%
  arrange(
    system,
    mean_weighted_EPC
  ) %>%
  pull(country_label)

plot_data <- plot_data %>%
  mutate(
    country_label = factor(
      country_label,
      levels = plot_order
    ),
    fill_group = factor(
      fill_group,
      levels = c(
        "Majoritarian",
        "Majoritarian (extended only)",
        "Proportional",
        "Proportional (extended only)"
      )
    )
  )

plot_wepc_bars <- ggplot(
  plot_data,
  aes(
    x = country_label,
    y = mean_weighted_EPC,
    fill = fill_group
  )
) +
  geom_col(
    width = 0.7
  ) +
  coord_flip() +
  facet_grid(
    rows = vars(system),
    scales = "free_y",
    space = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "Majoritarian" = "red",
      "Majoritarian (extended only)" = "#F5A3A3",
      "Proportional" = "blue",
      "Proportional (extended only)" = "lightblue"
    )
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    title = "Mean WEPC by country",
    subtitle = "Countries in brackets belong to the extended sample only",
    x = NULL,
    y = "Mean WEPC (1980-2025)",
    fill = "Electoral system"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

plot_rile_bars <- ggplot(
  plot_data,
  aes(
    x = country_label,
    y = mean_weighted_RILE_change,
    fill = fill_group
  )
) +
  geom_col(
    width = 0.7
  ) +
  coord_flip() +
  facet_grid(
    rows = vars(system),
    scales = "free_y",
    space = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "Majoritarian" = "red",
      "Majoritarian (extended only)" = "#f5a3a3",
      "Proportional" = "blue",
      "Proportional (extended only)" = "lightblue"
    )
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    title = "Mean weighted RILE change by country",
    subtitle = "Countries in brackets belong to the extended sample only; order follows the WEPC figure",
    x = NULL,
    y = "Mean weighted RILE change (1980-2025)",
    fill = "Electoral system"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

plot_enpp_bars <- ggplot(
  plot_data,
  aes(
    x = reorder(country_label, mean_enpp),
    y = mean_enpp,
    fill = fill_group
  )
) +
  geom_col(
    width = 0.7
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Majoritarian" = "red",
      "Majoritarian (extended only)" = "#F5A3A3",
      "Proportional" = "blue",
      "Proportional (extended only)" = "lightblue"
    )
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    title = "Mean ENPP by country",
    subtitle = "Countries in brackets belong to the extended sample only",
    x = NULL,
    y = "Mean ENPP (1980-2020)",
    fill = "Electoral system"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

plot_enpp_wepc_extended <- ggplot(
  extended_data,
  aes(
    x = mean_enpp,
    y = mean_weighted_EPC,
    colour = system,
    shape = system
  )
) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "grey50",
    linewidth = 0.6,
    inherit.aes = FALSE,
    aes(
      x = mean_enpp,
      y = mean_weighted_EPC
    )
  ) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = countryname
    ),
    vjust = -0.9,
    size = 3,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Majoritarian" = "red",
      "Proportional" = "blue"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Majoritarian" = 17,
      "Proportional" = 16
    )
  ) +
  labs(
    title = "Extended sample: ENPP and WEPC",
    x = "Mean ENPP (1980-2020)",
    y = "Mean WEPC (1980-2025)",
    colour = "Electoral system",
    shape = "Electoral system"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

plot_enpp_wepc_restricted <- ggplot(
  restricted_data,
  aes(
    x = mean_enpp,
    y = mean_weighted_EPC,
    colour = system,
    shape = system
  )
) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "grey50",
    linewidth = 0.6,
    inherit.aes = FALSE,
    aes(
      x = mean_enpp,
      y = mean_weighted_EPC
    )
  ) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = countryname
    ),
    vjust = -0.9,
    size = 3,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Majoritarian" = "red",
      "Proportional" = "blue"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Majoritarian" = 17,
      "Proportional" = 16
    )
  ) +
  labs(
    title = "Restricted sample: ENPP and WEPC",
    x = "Mean ENPP (1980-2020)",
    y = "Mean WEPC (1980-2025)",
    colour = "Electoral system",
    shape = "Electoral system"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

plot_enpp_rile_extended <- ggplot(
  extended_data,
  aes(
    x = mean_enpp,
    y = mean_weighted_RILE_change,
    colour = system,
    shape = system
  )
) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "grey50",
    linewidth = 0.6,
    inherit.aes = FALSE,
    aes(
      x = mean_enpp,
      y = mean_weighted_RILE_change
    )
  ) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = countryname
    ),
    vjust = -0.9,
    size = 3,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Majoritarian" = "red",
      "Proportional" = "blue"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Majoritarian" = 17,
      "Proportional" = 16
    )
  ) +
  labs(
    title = "Extended sample: ENPP and RILE change",
    x = "Mean ENPP (1980-2020)",
    y = "Mean weighted RILE change (1980-2025)",
    colour = "Electoral system",
    shape = "Electoral system"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

plot_enpp_rile_restricted <- ggplot(
  restricted_data,
  aes(
    x = mean_enpp,
    y = mean_weighted_RILE_change,
    colour = system,
    shape = system
  )
) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "grey50",
    linewidth = 0.6,
    inherit.aes = FALSE,
    aes(
      x = mean_enpp,
      y = mean_weighted_RILE_change
    )
  ) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = countryname
    ),
    vjust = -0.9,
    size = 3,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Majoritarian" = "red",
      "Proportional" = "blue"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Majoritarian" = 17,
      "Proportional" = 16
    )
  ) +
  labs(
    title = "Restricted sample: ENPP and RILE change",
    x = "Mean ENPP (1980-2020)",
    y = "Mean weighted RILE change (1980-2025)",
    colour = "Electoral system",
    shape = "Electoral system"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

plot_wepc_rile_extended <- ggplot(
  extended_data,
  aes(
    x = mean_weighted_EPC,
    y = mean_weighted_RILE_change,
    colour = system,
    shape = system
  )
) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "grey60",
    linewidth = 0.6,
    linetype = "dashed",
    inherit.aes = FALSE,
    aes(
      x = mean_weighted_EPC,
      y = mean_weighted_RILE_change
    )
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.7,
    inherit.aes = FALSE,
    aes(
      x = mean_weighted_EPC,
      y = mean_weighted_RILE_change,
      colour = system
    )
  ) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = countryname
    ),
    vjust = -0.9,
    size = 3,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Majoritarian" = "red",
      "Proportional" = "blue"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Majoritarian" = 17,
      "Proportional" = 16
    )
  ) +
  labs(
    title = "Extended sample: WEPC and RILE change",
    subtitle = "Dashed grey line: all countries pooled",
    x = "Mean WEPC (1980-2025)",
    y = "Mean weighted RILE change (1980-2025)",
    colour = "Electoral system",
    shape = "Electoral system"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

plot_wepc_rile_restricted <- ggplot(
  restricted_data,
  aes(
    x = mean_weighted_EPC,
    y = mean_weighted_RILE_change,
    colour = system,
    shape = system
  )
) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "grey60",
    linewidth = 0.6,
    linetype = "dashed",
    inherit.aes = FALSE,
    aes(
      x = mean_weighted_EPC,
      y = mean_weighted_RILE_change
    )
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.7,
    inherit.aes = FALSE,
    aes(
      x = mean_weighted_EPC,
      y = mean_weighted_RILE_change,
      colour = system
    )
  ) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = countryname
    ),
    vjust = -0.9,
    size = 3,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Majoritarian" = "red",
      "Proportional" = "blue"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Majoritarian" = 17,
      "Proportional" = 16
    )
  ) +
  labs(
    title = "Restricted sample: WEPC and RILE change",
    subtitle = "Dashed grey line: all countries pooled",
    x = "Mean WEPC (1980-2025)",
    y = "Mean weighted RILE change (1980-2025)",
    colour = "Electoral system",
    shape = "Electoral system"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

#Germany and USA
#############################
party_de_us <- mp_total %>%
  filter(
    countryname %in% c(
      "Germany",
      "United States"
    ),
    !is.na(change_per101)
  ) %>%
  mutate(
    year = floor(date / 100),
    weighted_EPC = total_issue_change * (pervote / 100),
    weighted_RILE = change_rile * (pervote / 100)
  ) %>%
  select(
    countryname,
    year,
    date,
    party,
    partyname,
    pervote,
    total_issue_change,
    rile,
    change_rile,
    weighted_EPC,
    weighted_RILE
  ) %>%
  arrange(
    countryname,
    date,
    desc(pervote)
  )

elections_de_us <- party_de_us %>%
  group_by(
    countryname,
    year,
    date
  ) %>%
  summarise(
    n_parties = n(),
    covered_vote = sum(pervote),
    WEPC = sum(weighted_EPC),
    WRILE = sum(weighted_RILE),
    .groups = "drop"
  )

countries_de_us <- elections_de_us %>%
  group_by(
    countryname
  ) %>%
  summarise(
    n_elections = n(),
    mean_WEPC = mean(WEPC),
    mean_WRILE = mean(WRILE),
    .groups = "drop"
  )

#Output
#############################
print(
  as.data.frame(extended_overview)
)

print(
  as.data.frame(restricted_overview)
)

print(
  as.data.frame(extended_by_system)
)

print(
  as.data.frame(restricted_by_system)
)

print(
  as.data.frame(extended_by_country)
)

print(
  as.data.frame(restricted_by_country)
)

print(
  as.data.frame(duplicate_party)
)

print(
  as.data.frame(extended_data)
)

print(
  as.data.frame(restricted_data)
)

print(extended_ttest_epc)

print(restricted_ttest_epc)

print(extended_ttest_rile)

print(restricted_ttest_rile)

print(extended_pearson)

print(extended_spearman)

print(restricted_pearson)

print(restricted_spearman)

print(plot_wepc_bars)

print(plot_rile_bars)

print(plot_enpp_bars)

print(plot_enpp_wepc_extended)

print(plot_enpp_wepc_restricted)

print(plot_enpp_rile_extended)

print(plot_enpp_rile_restricted)

print(plot_wepc_rile_extended)

print(plot_wepc_rile_restricted)

print(
  as.data.frame(elections_de_us)
)

print(
  as.data.frame(countries_de_us)
)
