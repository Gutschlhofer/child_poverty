## This file prepares all visualisations done on the data results of the 
#  original sample

library(pacman)
p_load(tidyverse, countrycode, cowplot, RColorBrewer)

## Visualisation settings
plot_path <- "output/plots" # save plots here
plot_template <- paste0("original_%s",".png")

identifiers <- c('au01h','au03h','au04h','au08h','au10h','au14h','br06h','br09h',
                 'br11h','br13h','br16h','ca00h','ca04h','ca07h','ca10h','ca12h',
                 'ca13h','ca14h','ca15h','ca16h','ca17h','cn02h','cn13h','de00h',
                 'de01h','de02h','de03h','de04h','de05h','de06h','de07h','de08h',
                 'de09h','de10h','de11h','de12h','de13h','de14h','de15h','de16h',
                 'in04h','in11h','ie00h','ie02h','ie03h','ie04h','ie05h','ie06h',
                 'ie07h','ie08h','ie09h','ie10h','ie11h','ie12h','ie13h','ie14h',
                 'ie15h','ie16h','ie17h','za08h','za10h','za12h','za15h','za17h',
                 'uk00h','uk01h','uk02h','uk03h','uk04h','uk05h','uk06h','uk07h',
                 'uk08h','uk09h','uk10h','uk11h','uk12h','uk13h','uk14h','uk15h',
                 'uk16h','uk17h','uk18h','us00h','us01h','us02h','us03h','us04h',
                 'us05h','us06h','us07h','us08h','us09h','us10h','us11h','us12h',
                 'us13h','us14h','us15h','us16h','us17h','us18h') 

hic <- c("au","ca","ie","uk","us")
mic <- c("br","cn","in", "za")

hic <- tolower(countrycode(hic, "iso2c", "iso3c", custom_match = c("UK" = "GBR")))
mic <- tolower(countrycode(mic, "iso2c", "iso3c", custom_match = c("UK" = "GBR")))

# We manually created this file reading/guessing the rates from the paper
caisme <- read.csv("input/caisme_results.csv") %>%
  mutate(iso3 = tolower(iso3),
         rel_pov_lis = rel_pov_lis / 100,
         abs_pov_lis = abs_pov_lis / 100,
         abs_pov_pc = abs_pov_pc / 100)

# read LISSY results
lissy <- read.csv("output/lissy_results.csv") %>%
  dplyr::mutate(cname = countrycode(iso3, "iso3c", "country.name", custom_match = c("RSB" = "Serbia"))) %>% 
  dplyr::filter(iso3 %in% c(hic,mic))

# $1.90 poverty line
lissy_190 <- read.csv("output/lissy_results_190.csv") %>%
  dplyr::filter(iso3 %in% mic) %>%
  mutate(cname = countrycode(iso3, "iso3c", "country.name"),
         line = "$1.90")

# $3.20 poverty line
lissy_320 <- read.csv("output/lissy_results_320.csv") %>%
  dplyr::filter(iso3 %in% mic) %>%
  mutate(cname = countrycode(iso3, "iso3c", "country.name"),
         line = "$3.20")

# $5.50 poverty line
lissy_550 <- read.csv("output/lissy_results_550.csv") %>%
  dplyr::filter(iso3 %in% mic) %>%
  mutate(cname = countrycode(iso3, "iso3c", "country.name"),
         line = "$5.50")

# Combine $1.90, $3.20 and $5.50 to one data frame
lissy_wb <- rbind(lissy_190, lissy_320, lissy_550) %>% 
  dplyr::select(iso3, cname, year, line, abs_pov_lis)


# Relative Poverty -------------------------------------------------------------

# lissy_rel <- comparison %>%
#   dplyr::select(iso3, year, rel.l, rel.comp, rel.comp.ok, rel.comp.diff) %>% 
#   dplyr::mutate(rel.comp = ifelse(rel.comp.ok, 0, rel.comp))

# choose discrete color palette
cols <- brewer.pal(name = "Dark2", n = 8)
# add one more color
additional_cols <- c("#0000FF")
cols <- c(cols, additional_cols)

# Make colors fit for each country
plot_cols <- as.character(cols)
names(plot_cols) <- as.character(unique(lissy$cname))

ggplot(lissy) +
  geom_line(aes(x=year, y=rel_pov_lis, colour=cname)) +
  geom_point(aes(x=year, y=rel_pov_lis, colour = cname), size = 0.8) +
  scale_color_manual(values = plot_cols) +
  labs(x ="Year", y = "Deep Child Poverty Rates (25% of median income)",
       colour = "") +
  scale_x_continuous(breaks = seq(min(lissy$year), max(lissy$year), by = 2)) +
  scale_y_continuous(breaks = seq(0, ceiling(max(lissy$rel_pov_lis)*100)/100, by = 0.02),
                     labels = scales::percent_format(accuracy = 1))

ggsave(path = plot_path, filename = sprintf(plot_template, "figure_1"), width = 8, height = 6)

# Absolute Poverty (6 and 2 dollars) -------------------------------------------

abs_scale <- c(
  # per capita
  abs_pov_pc = "per capita",
  # child param for pc
  abs_pov_g75t100 = "\u03B3 = .75, \u03B8 = 1",
  # LIS sqrt
  abs_pov_lis = "LIS sqrt",
  # child param for lis
  abs_pov_g75t50 = "\u03B3 = .75, \u03B8 = .50",
  # econ of scale
  abs_pov_g100t75 = "\u03B3 = 1, \u03B8 = .75",
  # different assumptions for both
  abs_pov_g75t75 = "\u03B3 = .75, \u03B8 = .75"
)

# HIC
lissy_hic <- lissy %>% 
  dplyr::filter(iso3 %in% hic) %>%
  dplyr::select(-abs_pov_g125t50,-abs_pov_g50t50) %>% 
  dplyr::select(cname, iso3, year, starts_with("abs_")) %>%
  tidyr::pivot_longer(cols = starts_with("abs_")) %>% 
  dplyr::mutate(name = abs_scale[name],
                name = factor(name, levels = abs_scale))

ggplot(lissy_hic) +
  geom_line(aes(x=year, y=value, colour = name)) +
  scale_color_viridis_d(end = 0.9) +
  labs(x ="Year", y = "Extreme Child Poverty Rates ($6 a day, 2011 const. int. $)",
       colour = "") +
  scale_x_continuous(breaks = seq(min(lissy$year), max(lissy$year), by = 4)) +
  scale_y_continuous(breaks = seq(0, ceiling(max(lissy$rel_pov_lis)*100)/100, by = 0.005),
                     labels = scales::percent_format(accuracy = 0.5)) +
  facet_wrap(~cname, nrow = 2)
ggsave(path = plot_path, filename = sprintf(plot_template, "absolute_hic_6"), width = 8, height = 6)

# MIC
lissy_mic <- lissy %>% 
  dplyr::filter(iso3 %in% mic) %>%
  dplyr::select(-abs_pov_g125t50,-abs_pov_g50t50) %>% 
  dplyr::select(cname, iso3, year, starts_with("abs_")) %>%
  tidyr::pivot_longer(cols = starts_with("abs_")) %>% 
  dplyr::mutate(name = abs_scale[name],
                name = factor(name, levels = abs_scale))

ggplot(lissy_mic) +
  geom_line(aes(x=year, y=value, colour = name)) +
  scale_color_viridis_d(end = 0.9) +
  labs(x ="Year", y = "Extreme Child Poverty Rates ($2 a day, 2011 const. int. $)",
       colour = "") +
  scale_x_continuous(breaks = seq(min(lissy$year), max(lissy$year), by = 2)) +
  scale_y_continuous(breaks = seq(0, ceiling(max(lissy$abs_pov_pc)*100)/100, by = 0.1),
                     labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~cname, nrow = 2)
ggsave(path = plot_path, filename = sprintf(plot_template, "absolute_mic_2"), width = 8, height = 6)


# Absolute Poverty $(3.20) -----------------------------------------------------

# HIC
lissy_hic <- lissy_320 %>% 
  dplyr::filter(iso3 %in% hic) %>%
  dplyr::select(-abs_pov_g125t50,-abs_pov_g50t50) %>% 
  dplyr::select(cname, iso3, year, starts_with("abs_")) %>%
  tidyr::pivot_longer(cols = starts_with("abs_")) %>% 
  dplyr::mutate(name = abs_scale[name],
                name = factor(name, levels = abs_scale)) %>% 
  dplyr::filter(name %in% c("LIS sqrt"))

ggplot(lissy_hic) +
  geom_line(aes(x=year, y=value, colour = cname)) +
  scale_color_manual(values = plot_cols) +
  labs(x ="Year", y = "Extreme Child Poverty Rates ($3.2 a day, LIS equivalence scale)",
       colour = "") +
  scale_x_continuous(breaks = seq(min(lissy_320$year), max(lissy_320$year), by = 2)) +
  scale_y_continuous(breaks = seq(0, ceiling(max(lissy_320$abs_pov_lis)*100)/100, by = 0.001),
                     labels = scales::percent_format(accuracy = 0.1))
ggsave(path = plot_path, filename = sprintf(plot_template, "absolute_hic_320"), width = 8, height = 6)

lissy_mic <- lissy_320 %>% 
  dplyr::filter(iso3 %in% mic) %>%
  dplyr::select(-abs_pov_g125t50,-abs_pov_g50t50) %>% 
  dplyr::select(cname, iso3, year, starts_with("abs_")) %>%
  tidyr::pivot_longer(cols = starts_with("abs_")) %>% 
  dplyr::mutate(name = abs_scale[name],
                name = factor(name, levels = abs_scale)) %>% 
  dplyr::filter(name %in% c("LIS sqrt"))

ggplot(lissy_mic) +
  geom_line(aes(x=year, y=value, colour = cname)) +
  scale_color_manual(values = plot_cols) +
  labs(x ="Year", y = "Extreme Child Poverty Rates ($3.2 a day, LIS equivalence scale)",
       colour = "") +
  scale_x_continuous(breaks = seq(min(lissy_320$year), max(lissy_320$year), by = 2)) +
  scale_y_continuous(breaks = seq(0, ceiling(max(lissy_320$abs_pov_lis)*10)/10, by = 0.05),
                     labels = scales::percent_format(accuracy = 1))
ggsave(path = plot_path, filename = sprintf(plot_template, "absolute_mic_320"), width = 8, height = 6)


# Absolute Poverty MICs ($1.90, $3.20, $5.50) ----------------------------------
lissy_wb_plot <- lissy_wb %>% 
  dplyr::filter(iso3 %in% mic) %>%
  dplyr::select(cname, iso3, year, starts_with("abs_"), line) %>%
  tidyr::pivot_longer(cols = starts_with("abs_")) %>% 
  dplyr::mutate(name = abs_scale[name],
                name = factor(name, levels = abs_scale)) %>% 
  dplyr::filter(name %in% c("LIS sqrt"))

ggplot(lissy_wb_plot) +
  geom_line(aes(x=year, y=value, colour = line)) +
  scale_color_viridis_d(begin = 0.1, end = 0.9) +
  labs(x ="Year", y = "Extreme Child Poverty Rates (LIS equivalence scale)",
       colour = "") +
  scale_x_continuous(breaks = seq(min(lissy_wb$year), max(lissy_wb$year), by = 2)) +
  scale_y_continuous(breaks = seq(0, ceiling(max(lissy_wb$abs_pov_lis)*10)/10, by = 0.1),
                     labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~cname, nrow = 2)
ggsave(path = plot_path, filename = sprintf(plot_template, "absolute_mic_worldbank"), width = 8, height = 6)


# Comparison -------------------------------------------------------------------

diff_names <- c(
  diff_rel_lis = "Deep Child Poverty Rate (LIS sqrt)",
  diff_abs_lis = "Extreme Child Poverty Rate (LIS sqrt)",
  diff_abs_pc = "Extreme Child Poverty Rate (per capita)"
)

comparison <- merge(lissy %>% dplyr::select(cname, iso3, year, rel_pov_lis, abs_pov_lis, abs_pov_pc), 
                    caisme, 
                    by = c("iso3", "year"),
                    suffixes = c(".l",".c")
                    ) %>%
  dplyr::mutate(
    diff_rel_lis = rel_pov_lis.l - rel_pov_lis.c,
    diff_abs_lis = abs_pov_lis.l - abs_pov_lis.c,
    diff_abs_pc = abs_pov_pc.l - abs_pov_pc.c
  ) %>%
  dplyr::filter(!is.na(diff_rel_lis)) %>%
  dplyr::select(iso3, cname, year, starts_with("diff_")) %>%
  tidyr::pivot_longer(cols = starts_with("diff_")) %>%
  dplyr::mutate(name = diff_names[name],
                name = factor(name, levels = diff_names))

ggplot(comparison) +
  geom_line(aes(x=year, y=value*100, colour = name)) +
  scale_color_viridis_d(begin = 0.1, end = 0.9) +
  geom_abline(intercept = 0, slope = 0, linetype = 2, alpha = 0.6) +
  labs(x ="Year", y = "Percentage Point Difference to Poverty Rates of Cai & Smeeding (2020)",
       colour = "") +
  scale_x_continuous(breaks = seq(min(comparison$year), max(comparison$year), by = 2)) +
  theme(legend.position="bottom",  axis.title.x = element_text(vjust = -0.5)) +
  facet_wrap(~cname, nrow = 3)
ggsave(path = plot_path, filename = sprintf(plot_template, "comparison"), width = 8, height = 6)


# Robustness checks ------------------------------------------------------------

do_robustness_comparison <- function(name = c("a","b")){
  name = name[1]
  file <- ifelse(name == "a", 
                 "output/lissy_results_robustnessA.csv",
                 "output/lissy_results_robustnessB.csv")
  
  robustness <- read.csv(file)
  
  lissy_robust <- lissy %>% 
    dplyr::left_join(robustness, by = c("iso3", "year"), suffix = c(".l",".c")) %>% 
    dplyr::mutate(
      diff_rel_lis = rel_pov_lis.l - rel_pov_lis.c,
      diff_abs_lis = abs_pov_lis.l - abs_pov_lis.c,
      diff_abs_pc = abs_pov_pc.l - abs_pov_pc.c
    ) %>%
    dplyr::filter(!is.na(diff_rel_lis)) %>%
    dplyr::select(iso3, cname, year, starts_with("diff_")) %>%
    tidyr::pivot_longer(cols = starts_with("diff_")) %>%
    dplyr::mutate(name = diff_names[name],
                  name = factor(name, levels = diff_names))
  
  ggplot(lissy_robust) +
    geom_line(aes(x=year, y=value*100, colour = name)) +
    scale_color_viridis_d(begin = 0.1, end = 0.9) +
    geom_abline(intercept = 0, slope = 0, linetype = 2, alpha = 0.6) +
    labs(x ="Year", y = sprintf("Percentage Point Difference in Poverty Rate to Robustness (%s)",name),
         colour = "") +
    scale_x_continuous(breaks = seq(min(lissy_robust$year), max(lissy_robust$year), by = 2)) +
    theme(legend.position="bottom") +
    facet_wrap(~cname, nrow = 3)
  
  ggsave(path = plot_path, filename = sprintf(plot_template, paste0("robustness_",name)), width = 8, height = 6)
}

do_robustness_comparison("a")
do_robustness_comparison("b")

# Fig.. 3a: Disagg -------------------------------------------------------------

disagg_name <- c(
  rel_pov_lis = "Disposable income",
  rel_means = "Means tested programs",
  rel_means_su = "Social insurance + Universal benefits",
  rel_means_su_pt = "Private transfers"
)

lissy_disaggregated <- lissy %>% 
  dplyr::select(all_of(names(disagg_name)),year,cname,iso3) %>%
  dplyr::mutate(
    rel_means_su_pt = rel_means_su_pt - rel_means_su,
    rel_means_su = rel_means_su - rel_means,
    rel_means = rel_means - rel_pov_lis
  ) %>% 
  tidyr::pivot_longer(cols = all_of(names(disagg_name))) %>%
  dplyr::mutate(name = disagg_name[name],
                name = factor(name, levels = c(rev(disagg_name)))) %>% 
  # now we replace all countryname-year combos just with the year if it's not the first
  dplyr::mutate(
    country_year = sprintf("%s, %0.f", cname, year),
    country_year = factor(country_year, levels = rev(unique(country_year))),
    year = factor(year, rev(sort(unique(year))))
  )

lissy_disaggregated_hic <- lissy_disaggregated %>% dplyr::filter(iso3 %in% hic,
                                                                 iso3 != "irl")
lissy_disaggregated_mic <- lissy_disaggregated %>% dplyr::filter(iso3 %in% mic)

max_pct_hic <- max((lissy_disaggregated_hic %>% 
                      dplyr::group_by(country_year) %>% 
                      dplyr::summarise(value = sum(value)))$value)
max_pct_mic <- max((lissy_disaggregated_mic %>% 
                      dplyr::group_by(country_year) %>% 
                      dplyr::summarise(value = sum(value)))$value)
  
dis_hic <- ggplot(lissy_disaggregated_hic) + 
  geom_bar(aes(fill=name, x=value, y=year), position="stack", stat="identity") +
  scale_fill_viridis_d(direction = -1) +
  facet_wrap(cname~., ncol = 1, nrow = 6, strip.position = "left") +
  scale_y_discrete(breaks = levels(lissy_disaggregated_hic$year)[seq(1, length(levels(lissy_disaggregated_hic$year)), by = 2)]) +
  scale_x_continuous(breaks = seq(0, max_pct_hic, by = 0.05),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x ="Deep Child Poverty Rate (LIS sqrt)", y = "",
       fill = "") +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(reverse = T))

dis_legend <- get_legend(dis_hic)

dis_hic <- dis_hic + 
  theme(legend.position="none")

dis_mic <- ggplot(lissy_disaggregated_mic) + 
  geom_bar(aes(fill=name, x=value, y=year), position="stack", stat="identity") +
  scale_fill_viridis_d(direction = -1) +
  facet_wrap(cname~., ncol = 1, nrow = 6, strip.position = "left") +
  scale_x_continuous(breaks = seq(0, max_pct_mic, by = 0.05),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x ="Deep Child Poverty Rate (LIS sqrt)", y = "",
       fill = "") +
  theme(legend.position="none") +
  guides(fill = guide_legend(reverse = T))

plot_grid(plot_grid(dis_hic, dis_mic, ncol = 2), dis_legend, nrow = 2, rel_heights = c(0.95, 0.05))
ggsave(path = plot_path, filename = sprintf(plot_template, "deep_disagg"), width = 8, height = 6)


max_pct_irl <- max((lissy_disaggregated %>% dplyr::filter(iso3 == "irl") %>% 
                     dplyr::group_by(country_year) %>% 
                     dplyr::summarise(value = sum(value)))$value)

ggplot(lissy_disaggregated %>% dplyr::filter(iso3 == "irl")) + 
  geom_bar(aes(fill=name, x=value, y=year), position="stack", stat="identity") +
  scale_fill_viridis_d(direction = -1) +
  facet_wrap(cname~., ncol = 1, nrow = 6, strip.position = "left") +
  scale_x_continuous(breaks = seq(0, ceiling(max_pct_irl*10)/10, by = 0.05),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x ="Deep Child Poverty Rate (LIS sqrt)", y = "",
       fill = "") +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(reverse = T))
ggsave(path = plot_path, filename = sprintf(plot_template, "deep_disagg_irl"), width = 8, height = 4)

