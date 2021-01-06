

# install.packages("readstata13")
library(readstata13)
library(dplyr)


hh <- read.dta13("input/us16ih.dta")
ind <- read.dta13("input/us16ip.dta")

intersect(colnames(ind), colnames(hh))
d <- c("did", "dname", "cname", "iso2", "iso3", "year", "wave", "currency", "grossnet")

dat <- left_join(ind, select(hh, -d), by = "hid")
dat <- dat %>%
  select(iso3, year, hid, pid, hpopwgt, ppopwgt, nhhmem, age, sex, dhi) %>%
  filter(dhi > 0)

dat <- dat %>%
  group_by(hid) %>%
  mutate(phi = dhi/n()) %>% # equil of 1
  ungroup() %>%
  arrange(phi) %>%
  mutate(cum_ppopwgt = cumsum(ppopwgt)) %>%
  mutate(i = if_else(cum_ppopwgt == quantile(cum_ppopwgt, 0.5), 1, 0))
