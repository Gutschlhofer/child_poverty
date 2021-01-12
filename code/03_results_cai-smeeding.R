

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

