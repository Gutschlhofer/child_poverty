

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

library(countrycode)

caisme <- data.frame(code = substr(identifiers, 0, 2),
                     iso3 = countrycode(substr(identifiers, 0, 2), "iso2c", "iso3c", custom_match = c("UK" = "GBR")),
                     year = as.numeric(paste0("20", substr(identifiers, 3, 4))),
                     rel = NA,
                     abs_pc = NA,
                     abs_sqrt = NA)

caisme <- caisme %>% filter(iso3 != "DEU")
length(unique(caisme$iso3)) # same as paper

# write.csv(caisme, "input/caisme_results.csv")

caisme <- read.csv("input/caisme_results.csv")

lissy <- read.csv("output/lissy_results.csv")

comparison <- caisme %>% 
  dplyr::mutate(iso3 = tolower(iso3)) %>% 
  dplyr::left_join(lissy, by = c("year", "iso3"), suffix = c(".c",".l")) %>% 
  dplyr::mutate(
    rel.comp = round((rel.l-rel.c)/rel.c * 100),
    abs_sqrt.comp = round((abs_sqrt.l-abs_sqrt.c)/abs_sqrt.c * 100),
    abs_pc.comp = round((abs_pc.l-abs_pc.c)/abs_pc.c * 100),
    # OK if within 5%, or within 0.1
    rel.comp.ok = if_else(abs(rel.comp)<=5, TRUE, if_else(abs(rel.l-rel.c) < 0.1, TRUE, FALSE)),
    abs_sqrt.comp.ok = if_else(abs(abs_sqrt.comp)<=5, TRUE, if_else(abs(abs_sqrt.l-abs_sqrt.c) < 0.1, TRUE, FALSE)),
    abs_pc.comp.ok = if_else(abs(abs_pc.comp)<=5, TRUE, if_else(abs(abs_pc.l-abs_pc.c) < 0.1, TRUE, FALSE))
  ) %>% 
  dplyr::filter(!is.na(rel.comp)) %>% 
  dplyr::select(iso3, year,
                rel.l,rel.c,rel.comp,rel.comp.ok,
                abs_sqrt.l,abs_sqrt.c,abs_sqrt.comp,abs_sqrt.comp.ok,
                abs_pc.l,abs_pc.c,abs_pc.comp,abs_pc.comp.ok,
                rel_means, rel_means_su, rel_means_su_pt) %>% 
  dplyr::mutate(
    rel_means_su_pt = rel_means_su_pt - rel_means_su,
    rel_means_su = rel_means_su - rel_means,
    rel_means = rel_means - rel.l,
  )

## fig. 1: rel/deep

lissy_rel <- comparison %>%
  dplyr::select(iso3, year, rel.l, rel.comp, rel.comp.ok) %>% 
  dplyr::mutate(rel.comp = ifelse(rel.comp.ok, 0, rel.comp))

ggplot(lissy_rel) +
  geom_line(aes(colour=iso3, x=year, y=rel.l))

ggplot(lissy_rel) +
  geom_line(aes(colour=iso3, x=year, y=rel.comp))

## fig. 2: abs/extreme

lissy_abs <- comparison %>%
  dplyr::select(iso3, year, starts_with("abs_")) %>%
  dplyr::mutate(abs_sqrt.comp = ifelse(abs_sqrt.comp.ok, 0, abs_sqrt.comp),
                abs_pc.comp = ifelse(abs_pc.comp.ok, 0, abs_pc.comp)) %>% 
  tidyr::pivot_longer(cols = starts_with("abs_"))

# hic
ggplot(lissy_abs %>% 
         dplyr::filter(iso3 %in% tolower(countrycode::countrycode(hic, "iso2c", "iso3c", custom_match = c("UK" = "GBR"))),
                       name %in% c("abs_pc.l", "abs_sqrt.l"))) +
  geom_line(aes(colour=iso3, linetype = name, x=year, y=value))

ggplot(lissy_abs %>% 
         dplyr::filter(iso3 %in% tolower(countrycode::countrycode(hic, "iso2c", "iso3c", custom_match = c("UK" = "GBR"))),
                       name %in% c("abs_pc.comp", "abs_sqrt.comp"))) +
  geom_line(aes(colour=iso3, linetype = name, x=year, y=value))

# mic
ggplot(lissy_abs %>% 
         dplyr::filter(iso3 %in% tolower(countrycode::countrycode(mic, "iso2c", "iso3c", custom_match = c("UK" = "GBR"))),
                       name %in% c("abs_pc.l", "abs_sqrt.l"))) +
  geom_line(aes(colour=iso3, linetype = name, x=year, y=value))

ggplot(lissy_abs %>% 
         dplyr::filter(iso3 %in% tolower(countrycode::countrycode(mic, "iso2c", "iso3c", custom_match = c("UK" = "GBR"))),
                       name %in% c("abs_pc.comp", "abs_sqrt.comp"))) +
  geom_line(aes(colour=iso3, linetype = name, x=year, y=value))


## fig. 3a: disagg

disagg_name <- c(
  rel.l = "Final",
  rel_means = "Means tested programs",
  rel_means_su = "Social insurance + Universal benefits",
  rel_means_su_pt = "Private transfers"
)

lissy_disaggregated <- comparison %>% 
  dplyr::select(starts_with("rel_"),rel.l,year,iso3) %>%
  tidyr::pivot_longer(cols = c(starts_with("rel_"),rel.l)) %>%
  dplyr::mutate(iso3_year = paste(iso3, year),
                iso3_year = factor(iso3_year, levels = rev(unique(iso3_year)))) %>% # to have the y axis order fit
  dplyr::mutate(name = disagg_name[name],
                name = factor(name, levels = c(rev(disagg_name))))

ggplot(lissy_disaggregated) + 
  geom_bar(aes(fill=name, x=value, y=iso3_year), position="stack", stat="identity") +
  scale_fill_viridis_d(direction = -1)

lissy_deu <- lissy %>% dplyr::filter(iso3 == "deu")
## TODO longer
ggplot(lissy_deu) + 
  geom_bar(aes(fill=name, x=value, y=iso3_year), position="stack", stat="identity")
