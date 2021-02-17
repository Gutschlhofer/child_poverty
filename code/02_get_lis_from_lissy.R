library(dplyr) 
library(stats)

## weighted median function ####################################################
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
################################################################################

identifiers <- c(
                 'au01h','au03h','au04h','au08h','au10h','au14h','br06h','br09h',
                 'br11h','br13h','br16h','ca00h','ca04h','ca07h','ca10h','ca12h',
                 'ca13h','ca14h','ca15h','ca16h','ca17h','cn02h','cn13h',
                 'in04h','in11h','ie00h','ie02h','ie03h','ie04h','ie05h','ie06h',
                 'ie07h','ie08h','ie09h','ie10h','ie11h','ie12h','ie13h','ie14h',
                 'ie15h','ie16h','ie17h','za08h','za10h','za12h','za15h','za17h',
                 'uk00h','uk01h','uk02h','uk03h','uk04h','uk05h','uk06h','uk07h',
                 'uk08h','uk09h','uk10h','uk11h','uk12h','uk13h','uk14h','uk15h',
                 'uk16h','uk17h','uk18h','us00h','us01h','us02h','us03h','us04h',
                 'us05h','us06h','us07h','us08h','us09h','us10h','us11h','us12h',
                 'us13h','us14h','us15h','us16h','us17h','us18h',
                 # extensions:'de00h',
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

hic <- c("au","ca","ie","uk","us")
mic <- c("br","cn","in", "za")
ext <- c("de", "at", "dk", "fi", "ge", "gr", "il", "jp", "mx", "pl", "ru", "rs", 
         "kr", "es", "ch", "tw", "uy")

# reduce to most recent years because file size is too big
years <- 2:16
identifiers <- identifiers[as.numeric(substr(identifiers,3,4)) %in% years]

# only HICs
# identifiers <- identifiers[substr(identifiers,1,2) %in% hic]

# specific country
# identifiers <- identifiers[substr(identifiers,1,2) == "us"]

# deflator for abs poverty
deflat <- read.dta13(paste(INC_DIR, "/ppp_2011.dta", sep=""),convert.factors=FALSE)
# deflat <- read.csv("input/resources_ppp_2011.txt", header = TRUE, sep = ",", dec = ".")
# https://www.liscenter.org/data-access/lissy/syntax/#ppp
# line <- 6

final_string <- ""

for(i in 1:length(identifiers)){
  
  identifier <- identifiers[i]
  
  print(identifier)

  dat <- read.LIS(identifier)
  
  # au and de have NAs in nhhmem or nhhmem17
  dat <- dat %>% filter(!is.na(hid),!is.na(dhi),!is.na(hpopwgt),!is.na(nhhmem),!is.na(nhhmem17))
  
  # Main setting ---------------------------------------------------------------
  dat <- dat %>%
    dplyr::filter(dhi > 0)
  
  # robustness a) --------------------------------------------------------------
  # dat <- dat %>% 
  #   dplyr::filter(dhi > 0,
  #                 hi12/dhi < 0.5) # exclude negative and zero income AND self empl >= 0.5 of dhi

  # robustness b) --------------------------------------------------------------
  # dat <- dat %>% 
  #   dplyr::mutate(dhi = ifelse(dhi > 0, dhi, 0))

  # line <- c(1.9,3.2,5.5)[3]
  line <- ifelse(substr(identifier,1,2) %in% hic, 6, # HIC
                 ifelse(substr(identifier,1,2) %in% mic, 2, # MIC
                        c(1.9,3.2,5.5)[2])) # the last option are the extension countries
  
  # create vector of leap years to control days
  leap_year <- c(2000,2004,2008,2012,2016,2020)
  
  scales <- data.frame(
    suffix = c("_pc", "_lis", "_g100t75", "_g75t100", "_g75t50", "_g125t50", "_g50t50", "_g75t75"),
    gamma =  c(1,     1,      1,          0.75,       0.75,      1.25,       0.5,       .75),
    theta =  c(1,     1/2,    0.75,       1,          1/2,       1/2,        1/2,       .75)
  )
  
  out_params <- paste("iso3","year", sep = ",")
  out_string <- paste(unique(dat$iso3), unique(dat$year), sep = ",")
  
  for(s in 1:nrow(scales)){
    scale <- scales[s,]
    gamma <- scale$gamma
    theta <- scale$theta
    suffix <- scale$suffix
    
    # relative poverty
    dat_child <- dat %>%
      mutate(sumhpopwgt = hpopwgt*nhhmem,
             sumhpopwgt17 = hpopwgt*nhhmem17) %>%
      mutate(phi = dhi/((nhhmem-nhhmem17)+gamma*nhhmem17)^theta) %>% # equivalence scale
      mutate(rel_povline = 0.25*(weighted.median(phi, w = sumhpopwgt))) %>%
      mutate(is_rel_pov = if_else(phi < rel_povline, T, F)) %>%
      filter(nhhmem17 >= 1)
    
    rel_poverty <- sum(dat_child$sumhpopwgt17[dat_child$is_rel_pov])/sum(dat_child$sumhpopwgt17)#*100
    
    if(suffix == "_lis"){
      dat_child_disaggregated <- dat_child %>% 
        mutate(
          dhi_means = dhi - hpub_a, # - assistance transfers
          dhi_means_su = dhi_means - hpub_u - hpub_i,
          dhi_means_su_pt = dhi_means_su - hiprivate,
          phi_means = dhi_means/((nhhmem-nhhmem17)+gamma*nhhmem17)^theta,
          phi_means_su = dhi_means_su/((nhhmem-nhhmem17)+gamma*nhhmem17)^theta,
          phi_means_su_pt = dhi_means_su_pt/((nhhmem-nhhmem17)+gamma*nhhmem17)^theta
        )
      
      rel_poverty_means <- sum(dat_child_disaggregated$sumhpopwgt17[dat_child_disaggregated$phi_means < dat_child_disaggregated$rel_povline])/sum(dat_child_disaggregated$sumhpopwgt17)#*100
      rel_poverty_means_su <- sum(dat_child_disaggregated$sumhpopwgt17[dat_child_disaggregated$phi_means_su < dat_child_disaggregated$rel_povline])/sum(dat_child_disaggregated$sumhpopwgt17)#*100
      rel_poverty_means_su_pt <- sum(dat_child_disaggregated$sumhpopwgt17[dat_child_disaggregated$phi_means_su_pt < dat_child_disaggregated$rel_povline])/sum(dat_child_disaggregated$sumhpopwgt17)#*100
      
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
    
    abs_poverty <- sum(dat_child$sumhpopwgt17[dat_child$abs_pov_i])/sum(dat_child$sumhpopwgt17)#*100
    
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
# copy-paste the output into output/results_lissy.csv
