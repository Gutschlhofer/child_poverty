## This file prepares all visualisations done on the data results of the 
#  extended sample

library(pacman)
p_load(tidyverse, countrycode, cowplot, RColorBrewer)

## Visualisation settings
plot_path <- "output/plots" # save plots here
plot_template <- paste0("extended_%s",".png")

# extensions
identifiers <- c(
  'de01h','de02h','de03h','de04h','de05h','de06h','de07h','de08h',
  'de09h','de10h','de11h','de12h','de13h','de14h','de15h','de16h',
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


## Fig. 3a: Disagg -------------------------------------------------------------

make_disagg_comparison <- function(group1, group2, plot_name){
  hic <- group1
  mic <- group2
  hic <- tolower(countrycode(hic, "iso2c", "iso3c", custom_match = c("UK" = "GBR")))
  mic <- tolower(countrycode(mic, "iso2c", "iso3c", custom_match = c("UK" = "GBR")))
  
  ## For this graph, 2004 looked very weird, so we excluded it:
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
    dplyr::mutate(name = disagg_name[name],
                  name = factor(name, levels = c(rev(disagg_name)))) %>% 
    # Now we replace all countryname-year combos just with the year if it's not the first
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
                        dplyr::summarise(value = sum(value)))$value, na.rm=T)
  max_pct_mic <- max((lissy_disaggregated_mic %>% 
                        dplyr::group_by(country_year) %>% 
                        dplyr::summarise(value = sum(value)))$value, na.rm=T)
  
  # Use one for both
  max_pct <- max(max_pct_hic, max_pct_mic)
  max_pct_hic <- max_pct
  max_pct_mic <- max_pct
  
  dis_hic <- ggplot(lissy_disaggregated_hic) + 
    geom_bar(aes(fill=name, x=value, y=year), position="stack", stat="identity") +
    scale_fill_viridis_d(direction = -1) +
    facet_wrap(cname~., ncol = 1, nrow = length(unique(lissy_disaggregated_hic$cname)), strip.position = "left") +
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
hic <- c("ge","il","ru")       # left group
mic <- c("mx", "uy")           # right group
make_disagg_comparison(group1 = hic, group2 = mic, plot_name = "RoW")

