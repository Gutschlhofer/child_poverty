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
years <- 3:15

identifiers <- identifiers[identifiers != 'uk18h'] # uk18h is invalid

identifiers <- identifiers[as.numeric(substr(identifiers,3,4)) %in% years] # reduce to most recent years because file size is too big

# only HICs
identifiers <- identifiers[substr(identifiers,1,2) %in% hic]

identifiers <- identifiers[substr(identifiers,1,2) == "ca"]

poverty <- data.frame(
  iso3 = NA,
  year = NA,
  rel_poverty = NA,
  abs_poverty = NA,
  equiv = "pc"
)

for(i in 1:length(identifiers)){
  identifier <- identifiers[i]
  
  print(identifier)
  
  pidentifier <- paste0(substr(identifier,1,4),'p')
  
  var_names <- c('hid','did','dname','cname','iso3','year','hpopwgt','hwgt','hwgta',
                 'currency','grossnet','fhimpu','nhhmem','hitotal','dhi','hifactor',
                 'hitransfer','hpublic','hpub_i','hpub_u','hpub_a','hcexp','hilabour',
                 'hicapital','hipension','hipubsoc','hiprivate','hxitsc','hid','pid',
                 'did','dname','cname','iso3','year','ppopwgt','pwgta','currency',
                 'currency','grossnet','fpimpu','age','sex','ppub_i','pitotal','pilabour',
                 'pipension','pxitsc') 
  
  var_names <- var_names[substr(var_names,1,1)!="p"]
  
  var_names <- c("did", "dname", "cname", "iso2", "iso3", "year", "wave", "currency", "grossnet")
  
  # data <- read.LIS(identifiers, vars = var_names)
  
  # v1 -----------------------------------------------------------------------------
  
  # hdata <- read.LIS(identifier)
  # 
  # # pdata <- read.LIS(pidentifier)
  # 
  # summary(hdata$hpub_u)
  # 
  # # summary(pdata$age)
  # 
  # nrow(hdata)
  # # nrow(pdata)
  # 
  # # d <- c("did", "dname", "cname", "iso2", "iso3", "year", "wave", "currency", "grossnet")
  # # data <- left_join(pdata, select(hdata, -all_of(d)), by = "hid")
  # 
  # data <- hdata
  # # data <- data %>% filter(!is.na(hid),!is.na(dhi),!is.na(ppopwgt))
  # 
  # data <- data %>% 
  #   # select(iso3, year, hid, pid, hpopwgt, ppopwgt, nhhmem, age, sex, dhi) %>%
  #   filter(dhi > 0) # exclude negative and zero income
  # 
  # lambda <- 1; gamma <- 1 #per capita scale
  # # lambda <- 1; gamma <- 1/2 #lis sqrt scale
  # # more???
  # 
  # # deep/relative poverty
  # dat_child <- data %>%
  #   mutate(sumhpopwgt = hpopwgt*nhhmem) %>% 
  #   # mutate(child = if_else(age < 18, 1, 0)) %>% 
  #   # group_by(hid) %>%
  #   # mutate(n_child = sum(child)) %>% 
  #   mutate(phi = dhi/((nhhmem-nhhmem17)+lambda*nhhmem17)^gamma) %>% # equivalence scale
  #   # ungroup() %>%
  #   arrange(phi) %>%
  #   mutate(cum_hpopwgt = cumsum(sumhpopwgt)) %>%
  #   mutate(dist = abs(cum_hpopwgt-quantile(cum_hpopwgt, 0.5)),
  #          is_median = if_else(dist == min(dist),T,F),
  #          rel_povline = 0.25*mean(phi[is_median])) %>% 
  #   mutate(rel_pov_i = if_else(phi <= rel_povline, 1, 0)) %>% #maybe < and not <=?
  #   filter(nhhmem17 >= 1)
  # 
  # rel_poverty <- sum(dat_child$sumhpopwgt[dat_child$rel_pov_i == 1])/sum(dat_child$sumhpopwgt)*100
  # # rel_poverty
  # 
  # v2 -------------------------------------------------------------------------
  
  hdata <- read.LIS(identifier)
  
  pdata <- read.LIS(pidentifier)
  
  # summary(hdata$hpub_u)
  
  # summary(pdata$age)
  
  # nrow(hdata)
  # nrow(pdata)
  
  d <- c("did", "dname", "cname", "iso2", "iso3", "year", "wave", "currency", "grossnet")
  data <- left_join(pdata, select(hdata, -all_of(d)), by = "hid")
  
  data <- data %>% filter(!is.na(hid),!is.na(dhi),!is.na(ppopwgt),!is.na(age))
  
  data <- data %>% 
    select(iso3, year, hid, pid, hpopwgt, ppopwgt, nhhmem, age, sex, dhi) %>%
    filter(dhi > 0) # exclude negative and zero income
  
  lambda <- 1; gamma <- 1 #per capita scale
  # lambda <- 1; gamma <- 1/2 #lis sqrt scale
  # more???
  
  dat_child <- data %>%
    mutate(child = if_else(age < 18, 1, 0)) %>% 
    group_by(hid) %>%
    mutate(n_child = sum(child)) %>% 
    mutate(phi = dhi/((n()-n_child)+lambda*n_child)^gamma) %>% # equivalence scale
    ungroup() %>%
    arrange(phi) %>%
    mutate(cum_ppopwgt = cumsum(ppopwgt),
           dist = abs(cum_ppopwgt-quantile(cum_ppopwgt, 0.5)),
           is_median = if_else(dist == min(dist),T,F),
           rel_povline = 0.25*mean(phi[is_median])) %>% 
    mutate(is_rel_pov = if_else(phi <= rel_povline, T, F)) %>% #maybe < and not <=?
    # mutate(cum_ppopwgt = cumsum(ppopwgt)) %>%
    # mutate(rel_povline = 0.25*phi[cum_ppopwgt == quantile(cum_ppopwgt, 0.5)]) %>%
    filter(age < 18)
  
  rel_poverty <- sum(dat_child$ppopwgt[dat_child$is_rel_pov])/sum(dat_child$ppopwgt)*100
  print(rel_poverty)
  
  # data <- data %>%
  #   dplyr::select(all_of(var_names))
  
  # summary(data$age)
  
  # extreme/absolute poverty
  deflat <- read.dta13(paste(INC_DIR, "/ppp_2011.dta", sep=""),convert.factors=FALSE)
  # deflat <- read.csv("input/resources_ppp_2011.txt", header = TRUE, sep = ",", dec = ".")
  # https://www.lisdatacenter.org/data-access/lissy/syntax/#ppp
  line <- 6
  
  dat_child <- dat_child %>%
    left_join(deflat, by = c("iso3", "year")) %>%
    mutate(abs_povline = line/(cpi/100)) %>% # we need to calculate back to the poverty line of 2011
    mutate(phi_ppp = (phi/ppp)/365.25) %>%
    mutate(abs_pov_i = if_else(phi_ppp <= abs_povline, 1, 0)) #maybe < and not <=?
  
  abs_poverty <- sum(dat_child$ppopwgt[dat_child$abs_pov_i == 1])/sum(dat_child$ppopwgt)*100
  print(abs_poverty)
  
  poverty[i,]$iso3 <- unique(data$iso3)
  poverty[i,]$year <- unique(data$year)
  poverty[i,]$rel_poverty <- rel_poverty
  poverty[i,]$abs_poverty <- abs_poverty
}

# rel_poverty
# 
# abs_poverty

print(poverty$rel_poverty)
print(poverty$abs_poverty)
print(poverty$iso3)
print(poverty$year)

sprintf("%s (%s): rel: %.2f | abs: %.2f", poverty$iso3, 
        poverty$year, poverty$rel_poverty, poverty$abs_poverty)
