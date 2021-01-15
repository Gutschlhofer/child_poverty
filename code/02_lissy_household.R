library(dplyr) 
library(stats)

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

hic <- c("au","ca","de","ie","uk","us")
mic <- c("br","cn","in", "za")

# years <- c('04','07','10','13','15')
years <- 2:16

identifiers <- identifiers[identifiers != 'uk18h'] # uk18h is invalid

identifiers <- identifiers[as.numeric(substr(identifiers,3,4)) %in% years] # reduce to most recent years because file size is too big

# only HICs
# identifiers <- identifiers[substr(identifiers,1,2) %in% hic]

# identifiers <- identifiers[substr(identifiers,1,2) == "us"]

poverty <- data.frame(
  iso3 = NA,
  year = NA,
  rel_poverty = NA,
  abs_poverty = NA,
  rel_poverty_pc = NA,
  abs_poverty_pc = NA,
  rel_poverty_means = NA,
  rel_poverty_means_su = NA,
  rel_poverty_means_su_pt = NA
)

for(i in 1:length(identifiers)){
  
  # identifier <- identifiers[1]
  identifier <- identifiers[i]
  
  print(identifier)

  dat <- read.LIS(identifier) # HH income
  
  # au and de have NAs in nhhmem or nhhmem17
  dat <- dat %>% filter(!is.na(hid),!is.na(dhi),!is.na(hpopwgt),!is.na(nhhmem),!is.na(nhhmem17))
  
  dat <- dat %>% 
    # select(iso3, year, hid, hpopwgt, nhhmem, nhhmem17, dhi) %>%
    filter(dhi > 0) # exclude negative and zero income
    # mutate(dhi = ifelse(dhi > 0, dhi, 0)) # version b) recode inc <=0 to 0

  ## weighted median function ##################################################
  weighted.median <- function (x, w = 1, na.rm = FALSE) {
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
  ##############################################################################
  
  # calc dhi by hand
  # dhi = hitotal (= hilabour + hicapital + hipension + hipubsoc + hiprivate) - hxitsc (=tax + social sec contrib)
  # other interesting:
  # hxiht Inter-Household Transfers Paid
  # hxotax Other Direct Taxes including property taxes
  dat <- dat %>% mutate(dhi2 = (hilabour + hicapital + hipension + hipubsoc + hiprivate) - hxitsc,
                        hpublic2 = hpub_i + hpub_a + hpub_u,
                        hpublicdiff = hpublic - hpublic2,
                        hpubsocdiff = hipubsoc - hpublic2)
  
  print(summary(dat$hpublicdiff))
  print(summary(dat$hpubsocdiff))
  print(summary(dat$hpub_a))
  
  # all.equal(dat$dhi, dat$dhi2) %>% print()
  
  # per capita scale -----------------------------------------------------------
  lambda <- 1; gamma <- 1 #per capita scale
  dat_child_pc <- dat %>%
    mutate(sumhpopwgt = hpopwgt*nhhmem,
           sumhpopwgt17 = hpopwgt*nhhmem17) %>%
    mutate(phi = dhi/((nhhmem-nhhmem17)+lambda*nhhmem17)^gamma) %>% # equivalence scale
    mutate(rel_povline = 0.25*(weighted.median(phi, w = sumhpopwgt))) %>%
    mutate(is_rel_pov = if_else(phi < rel_povline, T, F)) %>% #maybe < and not <=?
    filter(nhhmem17 >= 1)

  rel_poverty_pc <- sum(dat_child_pc$sumhpopwgt17[dat_child_pc$is_rel_pov])/sum(dat_child_pc$sumhpopwgt17)*100
  # print(rel_poverty_pc)
  
  # LIS sqrt scale -------------------------------------------------------------
  # lambda <- 1; gamma <- 1 #per capita scale
  lambda <- 1; gamma <- 1/2 #lis sqrt scale
  # more???
  dat_child <- dat %>%
    mutate(sumhpopwgt = hpopwgt*nhhmem,
           sumhpopwgt17 = hpopwgt*nhhmem17) %>%
    mutate(phi = dhi/((nhhmem-nhhmem17)+lambda*nhhmem17)^gamma) %>% # equivalence scale
    mutate(rel_povline = 0.25*(weighted.median(phi, w = sumhpopwgt))) %>%
    mutate(is_rel_pov = if_else(phi < rel_povline, T, F)) %>% #maybe < and not <=?
    filter(nhhmem17 >= 1)
  
  rel_poverty <- sum(dat_child$sumhpopwgt17[dat_child$is_rel_pov])/sum(dat_child$sumhpopwgt17)*100
  # print(rel_poverty)
  
  
  dat_child_disaggregated <- dat_child %>% 
    mutate(
      dhi_means = dhi - hpub_a, # - assistance transfers
      dhi_means_su = dhi_means - hpub_u - hpub_i,
      dhi_means_su_pt = dhi_means_su - hiprivate,
      phi_means = dhi_means/((nhhmem-nhhmem17)+lambda*nhhmem17)^gamma,
      phi_means_su = dhi_means_su/((nhhmem-nhhmem17)+lambda*nhhmem17)^gamma,
      phi_means_su_pt = dhi_means_su_pt/((nhhmem-nhhmem17)+lambda*nhhmem17)^gamma
    )
  
  rel_poverty_means <- sum(dat_child_disaggregated$sumhpopwgt17[dat_child_disaggregated$phi_means < dat_child_disaggregated$rel_povline])/sum(dat_child_disaggregated$sumhpopwgt17)*100
  rel_poverty_means_su <- sum(dat_child_disaggregated$sumhpopwgt17[dat_child_disaggregated$phi_means_su < dat_child_disaggregated$rel_povline])/sum(dat_child_disaggregated$sumhpopwgt17)*100
  rel_poverty_means_su_pt <- sum(dat_child_disaggregated$sumhpopwgt17[dat_child_disaggregated$phi_means_su_pt < dat_child_disaggregated$rel_povline])/sum(dat_child_disaggregated$sumhpopwgt17)*100
  
  # extreme/absolute poverty
  deflat <- read.dta13(paste(INC_DIR, "/ppp_2011.dta", sep=""),convert.factors=FALSE)
  # deflat <- read.csv("input/resources_ppp_2011.txt", header = TRUE, sep = ",", dec = ".")
  # https://www.liscenter.org/data-access/lissy/syntax/#ppp
  # line <- 6
  line <- if_else(substr(identifier,1,2) %in% hic, 6, 2)
  
  leap_year <- c(2000,2004,2008,2012,2016,2020)
  
  dat_child <- dat_child %>%
    left_join(deflat, by = c("iso3", "year")) %>%
    # mutate(abs_povline = line/(cpi/100)) %>% # we need to calculate back to the poverty line of 2011
    # mutate(phi_ppp = (phi/ppp)/365.25) %>%
    mutate(abs_povline = line,
           phi_ppp = (phi/lisppp)/ifelse(year %in% leap_year,366,365)) %>%
    mutate(abs_pov_i = if_else(phi_ppp <= abs_povline, T, F)) #maybe < and not <=?
  
  abs_poverty <- sum(dat_child$sumhpopwgt17[dat_child$abs_pov_i])/sum(dat_child$sumhpopwgt17)*100
  # print(abs_poverty)
  
  dat_child_pc <- dat_child_pc %>%
    left_join(deflat, by = c("iso3", "year")) %>%
    # mutate(abs_povline = line/(cpi/100)) %>% # we need to calculate back to the poverty line of 2011
    # mutate(phi_ppp = (phi/lisppp)/365.25) %>%
    mutate(abs_povline = line,
           phi_ppp = (phi/lisppp)/ifelse(year %in% leap_year,366,365)) %>%
    mutate(abs_pov_i = if_else(phi_ppp <= abs_povline, T, F)) #maybe < and not <=?
  
  # print(line)
  abs_poverty_pc <- sum(dat_child_pc$sumhpopwgt17[dat_child_pc$abs_pov_i])/sum(dat_child_pc$sumhpopwgt17)*100
  # print(abs_poverty_pc)
  
  poverty[i,]$iso3 <- unique(dat$iso3)
  poverty[i,]$year <- unique(dat$year)
  poverty[i,]$rel_poverty <- rel_poverty
  poverty[i,]$abs_poverty <- abs_poverty
  poverty[i,]$rel_poverty_pc <- rel_poverty_pc
  poverty[i,]$abs_poverty_pc <- abs_poverty_pc
  poverty[i,]$rel_poverty_means <- rel_poverty_means
  poverty[i,]$rel_poverty_means_su <- rel_poverty_means_su
  poverty[i,]$rel_poverty_means_su_pt <- rel_poverty_means_su_pt
}

# rel_poverty
# 
# abs_poverty

print(poverty$rel_poverty)
print(poverty$abs_poverty)
print(poverty$rel_poverty_pc)
print(poverty$abs_poverty_pc)
print(poverty$iso3)
print(poverty$year)

sprintf("%s (%s): rel: %.2f | abs: %.2f |PC:| rel: %.2f | abs: %.2f", 
        poverty$iso3, 
        poverty$year, 
        poverty$rel_poverty, poverty$abs_poverty,
        poverty$rel_poverty_pc, poverty$abs_poverty_pc)

# .csv output
paste("iso3", "year", "rel", "abs_sqrt", "rel_pc", "abs_pc","rel_means","rel_means_su","rel_means_su_pt", sep = ",")
paste(poverty$iso3, 
      poverty$year, 
      poverty$rel_poverty, poverty$abs_poverty,
      poverty$rel_poverty_pc, poverty$abs_poverty_pc,
      poverty$rel_poverty_means, poverty$rel_poverty_means_su, poverty$rel_poverty_means_su_pt, sep = ",")

