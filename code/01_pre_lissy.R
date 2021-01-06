file <- readxl::read_xlsx("input/our-lis-documentation-availability-matrix.xlsx")

library(tidyverse)

var_names <- file[,3][!is.na(file[,3]) & file[,3]!="Variable Name"]
datasets <- file[2,][!is.na(file[2,]) & file[2,]!="Dataset shortname"]

years <- as.integer(substr(datasets, 3, 4))
datasets <- datasets[years < 21] # filter out everything that is 19xx

datasets <- paste0(datasets, "h")

paste(tolower(datasets), sep = " ", collapse = "','")
# identifiers <- c('au81h','au85h','au89h','au95h','au01h','au03h','au04h','au08h','au10h','au14h','br06h','br09h','br11h','br13h','br16h','ca71h','ca75h','ca81h','ca87h','ca91h','ca94h','ca97h','ca98h','ca00h','ca04h','ca07h','ca10h','ca12h','ca13h','ca14h','ca15h','ca16h','ca17h','cn02h','cn13h','de73h','de78h','de81h','de83h','de84h','de87h','de89h','de91h','de94h','de95h','de98h','de00h','de01h','de02h','de03h','de04h','de05h','de06h','de07h','de08h','de09h','de10h','de11h','de12h','de13h','de14h','de15h','de16h','in04h','in11h','ie87h','ie94h','ie95h','ie96h','ie00h','ie02h','ie03h','ie04h','ie05h','ie06h','ie07h','ie08h','ie09h','ie10h','ie11h','ie12h','ie13h','ie14h','ie15h','ie16h','ie17h','za08h','za10h','za12h','za15h','za17h','uk69h','uk74h','uk79h','uk86h','uk91h','uk94h','uk95h','uk99h','uk00h','uk01h','uk02h','uk03h','uk04h','uk05h','uk06h','uk07h','uk08h','uk09h','uk10h','uk11h','uk12h','uk13h','uk14h','uk15h','uk16h','uk17h','uk18h','us74h','us79h','us86h','us91h','us92h','us93h','us94h','us95h','us96h','us97h','us98h','us99h','us00h','us01h','us02h','us03h','us04h','us05h','us06h','us07h','us08h','us09h','us10h','us11h','us12h','us13h','us14h','us15h','us16h','us17h','us18h')

paste(var_names, sep = " ", collapse = "','")
# var_names <- c('hid','did','dname','cname','iso3','year','hpopwgt','hwgt','hwgta','currency','currency','grossnet','fhimpu','nhhmem','hitotal','dhi','hifactor','hitransfer','hpublic','hpub_i','hpub_u','hpub_a','hcexp','hilabour','hicapital','hipension','hipubsoc','hiprivate','hxitsc','hid','pid','did','dname','cname','iso3','year','ppopwgt','pwgta','currency','currency','grossnet','fpimpu','age','sex','ppub_i','pitotal','pilabour','pipension','pxitsc')

# problem: the var_names need to by identifier