library(dplyr) 
library(stats)

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

# deflator for abs poverty
deflat <- read.dta13(paste(INC_DIR, "/ppp_2011.dta", sep=""),convert.factors=FALSE)
# deflat <- read.csv("input/resources_ppp_2011.txt", header = TRUE, sep = ",", dec = ".")
# https://www.liscenter.org/data-access/lissy/syntax/#ppp
# line <- 6

final_string <- ""

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
  
  # calc dhi by hand
  # dhi = hitotal (= hilabour + hicapital + hipension + hipubsoc + hiprivate) - hxitsc (=tax + social sec contrib)
  # other interesting:
  # hxiht Inter-Household Transfers Paid
  # hxotax Other Direct Taxes including property taxes
  # dat <- dat %>% mutate(dhi2 = (hilabour + hicapital + hipension + hipubsoc + hiprivate) - hxitsc, # dhi is exactly the same
  #                       hpublic2 = hpub_i + hpub_a + hpub_u,
  #                       hpublicdiff = hpublic - hpublic2,
  #                       hpubsocdiff = hipubsoc - hpublic2)
  # 
  # print(summary(dat$hpublicdiff))
  # print(summary(dat$hpubsocdiff))
  # print(summary(dat$hpub_a))
  
  # all.equal(dat$dhi, dat$dhi2) %>% print()
  
  line <- if_else(substr(identifier,1,2) %in% hic, 6, 2)
  
  leap_year <- c(2000,2004,2008,2012,2016,2020)
  
  scales <- data.frame(
    suffix = c("_pc", "_lis", "_l100g75", "_l75g100", "_l75g50", "_l125g50", "_l50g50"),
    lambda = c(1,     1,      1,          0.75,       0.75,      1.25,       0.5),
    gamma =  c(1,     1/2,    0.75,       1,          1/2,       1/2,        1/2)
  )
  
  out_params <- paste("iso3","year", sep = ",")
  out_string <- paste(unique(dat$iso3), unique(dat$year), sep = ",")
  
  for(s in 1:nrow(scales)){
    scale <- scales[s,]
    lambda <- scale$lambda
    gamma <- scale$gamma
    suffix <- scale$suffix
    
    # relative poverty
    dat_child <- dat %>%
      mutate(sumhpopwgt = hpopwgt*nhhmem,
             sumhpopwgt17 = hpopwgt*nhhmem17) %>%
      mutate(phi = dhi/((nhhmem-nhhmem17)+lambda*nhhmem17)^gamma) %>% # equivalence scale
      mutate(rel_povline = 0.25*(weighted.median(phi, w = sumhpopwgt))) %>%
      mutate(is_rel_pov = if_else(phi < rel_povline, T, F)) %>% #maybe < and not <=?
      filter(nhhmem17 >= 1)
    
    rel_poverty <- sum(dat_child$sumhpopwgt17[dat_child$is_rel_pov])/sum(dat_child$sumhpopwgt17)*100
    
    if(suffix == "_lis"){
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
      
      out_params <- paste(out_params, "rel_means","rel_means_su","rel_means_su_pt", sep = ",")
      out_string <- paste(out_string, 
                          rel_poverty_means,
                          rel_poverty_means_su,
                          rel_poverty_means_su_pt, 
                          sep = ",")
    }
    
    # abs poverty
    dat_child <- dat_child %>%
      left_join(deflat, by = c("iso3", "year")) %>%
      # mutate(abs_povline = line/(cpi/100)) %>% # we need to calculate back to the poverty line of 2011
      # mutate(phi_ppp = (phi/ppp)/365.25) %>%
      mutate(abs_povline = line,
             phi_ppp = (phi/lisppp)/ifelse(year %in% leap_year,366,365)) %>%
      mutate(abs_pov_i = if_else(phi_ppp <= abs_povline, T, F)) # maybe < and not <=?
    
    abs_poverty <- sum(dat_child$sumhpopwgt17[dat_child$abs_pov_i])/sum(dat_child$sumhpopwgt17)*100
    
    out_params <- paste(out_params, paste0("rel_pov",suffix), paste0("abs_pov",suffix), sep = ",")
    out_string <- paste(out_string, 
                        rel_poverty,
                        abs_poverty,
                        sep = ",")
  }
  if(i == 1){
    final_string <- out_params
  }
  final_string <- paste(final_string, out_string, sep = "\n")
}

cat(final_string)

