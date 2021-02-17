library(tidyverse)
library(countrycode)
library(cowplot)

## visualisation settings
# save plots under
plot_path <- "output/plots"
plot_template <- paste0("extension_%s",".png")

identifiers <- c(
  # 'au01h','au03h','au04h','au08h','au10h','au14h','br06h','br09h',
  # 'br11h','br13h','br16h','ca00h','ca04h','ca07h','ca10h','ca12h',
  # 'ca13h','ca14h','ca15h','ca16h','ca17h','cn02h','cn13h','de00h',
  'de01h','de02h','de03h','de04h','de05h','de06h','de07h','de08h',
  'de09h','de10h','de11h','de12h','de13h','de14h','de15h','de16h',
  # 'in04h','in11h','ie00h','ie02h','ie03h','ie04h','ie05h','ie06h',
  # 'ie07h','ie08h','ie09h','ie10h','ie11h','ie12h','ie13h','ie14h',
  # 'ie15h','ie16h','ie17h','za08h','za10h','za12h','za15h','za17h',
  # 'uk00h','uk01h','uk02h','uk03h','uk04h','uk05h','uk06h','uk07h',
  # 'uk08h','uk09h','uk10h','uk11h','uk12h','uk13h','uk14h','uk15h',
  # 'uk16h','uk17h','uk18h','us00h','us01h','us02h','us03h','us04h',
  # 'us05h','us06h','us07h','us08h','us09h','us10h','us11h','us12h',
  # 'us13h','us14h','us15h','us16h','us17h','us18h',
  # extensions:
  'at04h','at07h','at10h','at13h','at16h','dk04h','dk07h','dk10h',
  'dk13h','dk16h','fi04h','fi07h','fi10h','fi13h','fi16h','ge10h',
  'ge13h','ge16h','gr04h','gr07h','gr10h','gr13h','gr16h','ie03h',
  'ie04h','ie05h','ie06h','ie07h','ie08h','ie09h','ie10h','ie11h',
  'ie12h','ie13h','ie14h','ie15h','ie16h','ie17h','il05h','il07h',
  'il10h','il12h','il14h','il16h','jp08h','jp10h','jp13h','mx04h',
  'mx08h','mx10h','mx12h','mx14h','mx16h','pl04h','pl07h','pl10h',
  'pl13h','pl16h','ru00h','ru04h','ru07h','ru10h','ru11h','ru13h',
  'ru14h','ru15h','ru16h','ru17h','rs06h','rs10h','rs13h','rs16h',
  'kr06h','kr08h','kr10h','kr12h','kr14h','kr16h','es00h','es04h',
  'es07h','es10h','es13h','es16h','ch04h','ch06h','ch07h','ch08h',
  'ch09h','ch10h','ch11h','ch12h','ch13h','ch14h','ch15h','ch16h',
  'ch17h','tw05h','tw07h','tw10h','tw13h','tw16h','uy04h','uy07h',
  'uy10h','uy13h','uy16h')

ext <- c("de", "at", "dk", "fi", "ge", "gr", "il", "jp", "mx", "pl", "ru", "rs", 
         "kr", "es", "ch", "tw", "uy")
ext <- tolower(countrycode(ext, "iso2c", "iso3c", custom_match = c("UK" = "GBR")))

lissy <- read.csv("output/lissy_results.csv") %>%
  dplyr::mutate(cname = countrycode(iso3, "iso3c", "country.name", custom_match = c("RSB" = "Serbia"))) %>% 
  dplyr::filter(cname != "Japan",
                iso3 %in% ext)

# Relative Poverty -------------------------------------------------------------

library(RColorBrewer)
# choose discrete color palette
cols <- brewer.pal(name = "Dark2", n = 8)
# add 2 more colors
additional_cols <- c("#FF0000","#0000FF")
cols <- c(cols, additional_cols)

# make colors fit for each country
plot_cols <- as.character(cols)
names(plot_cols) <- as.character(unique(lissy$cname))

ggplot(lissy) +
  geom_line(aes(x=year, y=rel_pov_lis, colour=cname)) +
  geom_point(aes(x=year, y=rel_pov_lis, colour = cname), size = 0.8) +
  # scale_color_manual(values = plot_cols) +
  labs(x ="Year", y = "Deep Child Poverty Rates (25% of median income)",
       colour = "") +
  scale_x_continuous(breaks = seq(min(lissy$year), max(lissy$year), by = 2)) +
  scale_y_continuous(breaks = seq(0, ceiling(max(lissy$rel_pov_lis)*100)/100, by = 0.02),
                     labels = scales::percent_format(accuracy = 1))

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

# ext
lissy_ext <- lissy %>% 
  dplyr::filter(iso3 %in% ext) %>%
  dplyr::select(-abs_pov_g125t50,-abs_pov_g50t50) %>% 
  dplyr::select(cname, iso3, year, starts_with("abs_")) %>%
  tidyr::pivot_longer(cols = starts_with("abs_")) %>% 
  dplyr::mutate(name = abs_scale[name],
                name = factor(name, levels = abs_scale))

ggplot(lissy_ext) +
  geom_line(aes(x=year, y=value, colour = name)) +
  scale_color_viridis_d() +
  labs(x ="Year", y = "Absolute Child Poverty Rates ($5.50 a day)",
       colour = "") +
  scale_x_continuous(breaks = seq(min(lissy$year), max(lissy$year), by = 2)) +
  scale_y_continuous(breaks = seq(0, ceiling(max(lissy$abs_pov_pc,na.rm=T)*100)/100, by = 0.1),
                     labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~cname, nrow = 2)
ggsave(path = plot_path, filename = sprintf(plot_template, "absolute_poverty_5_50"), width = 8, height = 6)

## fig. 3a: disagg -------------------------------------------------------------

make_disagg_comparison <- function(group1, group2, plot_name){
  hic <- group1
  mic <- group2
  hic <- tolower(countrycode(hic, "iso2c", "iso3c", custom_match = c("UK" = "GBR")))
  mic <- tolower(countrycode(mic, "iso2c", "iso3c", custom_match = c("UK" = "GBR")))
  
  ## for this graph, 2004 looked very weird, so we excluded it:
  lissy <- lissy %>% dplyr::filter(year >= 2004)
  
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
    # dplyr::group_by(iso3) %>% 
    # dplyr::mutate(country_year = if_else(
    #   year == min(year),
    #   sprintf("%s, %0.f", cname, year),
    #   as.character(year))) %>% 
    # dplyr::ungroup() %>% 
    dplyr::mutate(name = disagg_name[name],
                  name = factor(name, levels = c(rev(disagg_name)))) %>% 
    # now we replace all countryname-year combos just with the year if it's not the first
    dplyr::mutate(
      # country_year == if_else(year == min(lissy$year[lissy$iso3 == iso3]), country_year, as.character(year)),
      country_year = sprintf("%s, %0.f", cname, year),
      country_year = factor(country_year, levels = rev(unique(country_year))),
      year = factor(year, rev(sort(unique(year))))
    )
  
  lissy_disaggregated_hic <- lissy_disaggregated %>% dplyr::filter(iso3 %in% hic,
                                                                   iso3 != "irl")
  lissy_disaggregated_mic <- lissy_disaggregated %>% dplyr::filter(iso3 %in% mic)
  
  max_pct_hic <- max((lissy_disaggregated_hic %>% 
                        dplyr::group_by(country_year) %>% 
                        dplyr::summarise(value = sum(value)))$value, na.rm=T)
  max_pct_mic <- max((lissy_disaggregated_mic %>% 
                        dplyr::group_by(country_year) %>% 
                        dplyr::summarise(value = sum(value)))$value, na.rm=T)
  
  # use one for both
  max_pct <- max(max_pct_hic, max_pct_mic)
  max_pct_hic <- max_pct
  max_pct_mic <- max_pct
  
  dis_hic <- ggplot(lissy_disaggregated_hic) + 
    geom_bar(aes(fill=name, x=value, y=year), position="stack", stat="identity") +
    scale_fill_viridis_d(direction = -1) +
    facet_wrap(cname~., ncol = 1, nrow = length(unique(lissy_disaggregated_hic$cname)), strip.position = "left") +
    # scale_y_discrete(breaks = levels(lissy_disaggregated_hic$year)[seq(1, length(levels(lissy_disaggregated_hic$year)), 
    #                                                                    by = ifelse(length(unique(lissy_disaggregated_hic$cname)) < 4,
    #                                                                                1,
    #                                                                                2))]) +
    # scale_x_continuous(breaks = seq(min(lissy_320$year), max(lissy_320$year), by = 2)) +
    scale_x_continuous(breaks = seq(0, max_pct_hic, by = 0.05),
                       labels = scales::percent_format(accuracy = 1),
                       limits = c(0, max_pct_hic)) +
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
    facet_wrap(cname~., ncol = 1, nrow = length(unique(lissy_disaggregated_mic$cname)), strip.position = "left") +
    # scale_y_discrete(breaks = levels(lissy_disaggregated_hic$year)[seq(1, length(levels(lissy_disaggregated_mic$year)), by = 2)]) +
    # scale_x_continuous(breaks = seq(min(lissy_320$year), max(lissy_320$year), by = 2)) +
    scale_x_continuous(breaks = seq(0, max_pct_mic, by = 0.05),
                       labels = scales::percent_format(accuracy = 1),
                       limits = c(0, max_pct_mic)) +
    labs(x ="Deep Child Poverty Rate (LIS sqrt)", y = "",
         fill = "") +
    theme(legend.position="none") +
    guides(fill = guide_legend(reverse = T))
  
  plot_grid(plot_grid(dis_hic, dis_mic, ncol = 2), dis_legend, nrow = 2, rel_heights = c(0.95, 0.05))
  ggsave(path = plot_path, filename = sprintf(plot_template, paste0("deep_disagg","_",plot_name)), width = 8, height = 6)
}

# Create the two groups to be compared
hic <- c("de", "at", "dk", "fi", "gr", "jp", "kr", "es", "ch") # left group
mic <- c("ge", "il", "mx", "pl", "ru", "rs", "tw", "uy")       # right group
make_disagg_comparison(group1 = hic, group2 = mic, plot_name = "all")

# Create the two groups to be compared
hic <- c("de", "at", "dk", "fi") # left group
mic <- c("gr", "es", "pl", "ch") # right group
make_disagg_comparison(group1 = hic, group2 = mic, plot_name = "europe")

# Create the two groups to be compared
hic <- c("ge","il","mx","ru") # left group
mic <- c("rs", "tw", "uy", "kr")    # right group
make_disagg_comparison(group1 = hic, group2 = mic, plot_name = "RoW")

## findings
# Switzerland: really cool, in 2013, it seems there was a new means-tested program
#              hmm or not, because the final values are not much lower, maybe 1) new unemployments or 2) new LISSY stuff
# Denmark has lots of means-testing going on (although Nordic country), Finland has less
# Greece and Spain were hit hard by the financial crisis
# South Korea has lots and lots of private transfers
# with massive private transfers Georgia brings down level to Israel
# seems as if Israel massively cut down on means-tested programs (or less eligible), increasing poverty
# Mexico also dependent on transfers
# Uruguay seems very effective in reducing child poverty


## pick one?
# ggplot(lissy_disaggregated %>% dplyr::filter(iso3 == "irl")) + 
#   geom_bar(aes(fill=name, x=value, y=year), position="stack", stat="identity") +
#   scale_fill_viridis_d(direction = -1) +
#   facet_wrap(cname~., ncol = 1, nrow = 6, strip.position = "left") +
#   # scale_y_discrete(breaks = levels(lissy_disaggregated_hic$year)[seq(1, length(levels(lissy_disaggregated_hic$year)), by = 2)]) +
#   # scale_x_continuous(breaks = seq(min(lissy_320$year), max(lissy_320$year), by = 2)) +
#   scale_x_continuous(breaks = seq(0, max_pct_hic, by = 0.05),
#                      labels = scales::percent_format(accuracy = 1)) +
#   labs(x ="Deep Child Poverty Rate (LIS sqrt)", y = "",
#        fill = "") +
#   theme(legend.position="bottom") +
#   guides(fill = guide_legend(reverse = T))





