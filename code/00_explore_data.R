# install.packages("readstata13")
# install.packages("gamboostLSS")
library(readstata13)
library(dplyr)
# library(gamboostLSS) # copied weighted.median function

# v1: hh only ------------------------------------------------------------------

hh <- read.dta13("input/us16ih.dta",convert.factors = F)
# ind <- read.dta13("input/us16ip.dta")
# 
# intersect(colnames(ind), colnames(hh))
# d <- c("did", "dname", "cname", "iso2", "iso3", "year", "wave", "currency", "grossnet")

# dat <- left_join(ind, select(hh, -d), by = "hid")
dat <- hh
dat <- dat %>%
  # select(iso3, year, hid, pid, hpopwgt, ppopwgt, nhhmem, age, sex, dhi) %>%
  filter(dhi > 0) # exclude negative and zero income

lambda <- 1; gamma <- 1 #per capita scale
lambda <- 1; gamma <- 1/2 #lis sqrt scale


# deep/relative poverty
dat_child <- dat %>%
  mutate(sumhpopwgt = hpopwgt*nhhmem) %>% 
  # mutate(child = if_else(age < 18, 1, 0)) %>% 
  # group_by(hid) %>%
  # mutate(n_child = sum(child)) %>% 
  mutate(phi = dhi/((nhhmem-nhhmem17)+lambda*nhhmem17)^gamma) %>% # equivalence scale
  # ungroup() %>%
  
  # arrange(phi) %>%
  # mutate(cum_hpopwgt = cumsum(sumhpopwgt)) %>%
  # mutate(dist = abs(cum_hpopwgt-quantile(cum_hpopwgt, 0.5)),
  #        is_median = if_else(dist == min(dist),T,F),
  #        rel_povline = 0.25*mean(phi[is_median])) %>% 
  
  mutate(rel_povline = 0.25*(weighted.median(phi, w = sumhpopwgt))) %>% #not sure if that is correct?
  
  mutate(rel_pov_i = if_else(phi <= rel_povline, T, F)) %>% #maybe < and not <=?
  filter(nhhmem17 >= 1)

rel_poverty <- sum(dat_child$sumhpopwgt[dat_child$rel_pov_i])/sum(dat_child$sumhpopwgt)*100
rel_poverty


# v2: hh+ind -------------------------------------------------------------------

hh <- read.dta13("input/us16ih.dta",convert.factors = F)
ind <- read.dta13("input/us16ip.dta")

# intersect(colnames(ind), colnames(hh))
d <- c("did", "dname", "cname", "iso2", "iso3", "year", "wave", "currency", "grossnet")

dat <- left_join(ind, select(hh, -d), by = "hid")
dat <- dat %>%
  select(iso3, year, hid, pid, hpopwgt, ppopwgt, nhhmem, age, sex, dhi) %>%
  filter(dhi > 0) # exclude negative and zero income

lambda <- 1; gamma <- 1 #per capita scale
# lambda <- 1; gamma <- 1/2 #lis sqrt scale
# more???

## weighted median function ####################################################
weighted.median <- function (x, w = 1, na.rm = FALSE) 
{
  if (length(w) == 1) 
    w <- rep(w, length(x))
  x <- x[w != 0]
  w <- w[w != 0]
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]
    w <- w[keep]
  }
  else {
    if (any(is.na(x)) | any(is.na(w))) 
      return(NA)
  }
  ind <- order(x)
  x <- x[ind]
  w <- w[ind]
  ind1 <- min(which(cumsum(w)/sum(w) > 0.5))
  ind2 <- ifelse(ind1 == 1, 1, max(which(cumsum(w)/sum(w) <= 
                                           0.5)))
  if (sum(w)%%1 == 0 && sum(w)%%2 == 0) 
    return(mean(c(x[ind1], x[ind2])))
  return(max(c(x[ind1], x[ind2])))
}
################################################################################

# deep/relative poverty
dat_child <- dat %>%
  mutate(child = if_else(age < 18, 1, 0)) %>% 
  group_by(hid) %>%
  mutate(n_child = sum(child)) %>% 
  mutate(phi = dhi/((n()-n_child)+lambda*n_child)^gamma) %>% # equivalence scale
  ungroup() %>%
  
  # arrange(phi) %>%
  # mutate(cum_ppopwgt = cumsum(ppopwgt),
  #        dist = abs(cum_ppopwgt-quantile(cum_ppopwgt, 0.5)),
  #        is_median = if_else(dist == min(dist),T,F),
  #        rel_povline = 0.25*mean(phi[is_median])) %>% # not sure how to calc weighted median by hand, so use package instead?
  
  mutate(rel_povline = 0.25*(weighted.median(phi, w = ppopwgt))) %>%

  mutate(is_rel_pov = if_else(phi <= rel_povline, T, F)) %>% # maybe < and not <=?
  filter(age < 18)

sum(dat_child$ppopwgt[dat_child$is_rel_pov])/sum(dat_child$ppopwgt)*100


# extreme/absolute poverty
deflat <- read.csv("input/resources_ppp_2011.txt", header = TRUE, sep = ",", dec = ".")
# https://www.lisdatacenter.org/data-access/lissy/syntax/#ppp
line <- 6

dat_child <- dat_child %>%
  left_join(deflat, by = c("iso3", "year")) %>%
  mutate(abs_povline = line/(cpi/100)) %>% # we need to calculate back to the poverty line of 2011
  mutate(phi_ppp = (phi/ppp)/365.25) %>%
  mutate(abs_pov_i = if_else(phi_ppp <= abs_povline, T, F)) #maybe < and not <=?

sum(dat_child$ppopwgt[dat_child$abs_pov_i])/sum(dat_child$ppopwgt)*100


