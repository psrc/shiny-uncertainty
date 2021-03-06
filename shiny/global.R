library(shinythemes)
library(ggplot2)
library(plotly)
library(grid)
library(gridExtra)
library(reshape2)
library(data.table)
library(magrittr)
library(stringr)
library(scales)
library(purrr)

base <- list(Modelsrv5 = "/media/modelsrv5d/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv6 = "/media/modelsrv6d/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv8 = "/media/modelsrv8d/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv3 = "/media/modelsrv3e/opusgit/urbansim_data/data/psrc_parcel/runs"
             )
# base <- list(Modelsrv5 = "//modelsrv5/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Modelsrv6 = "//modelsrv6/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Modelsrv8 = "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Modelsrv3 = "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
#              )
# base <- list(Modelsrv3 = "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
#             Modelsrv8 = "/Volumes/d$/opusgit/urbansim_data/data/psrc_parcel/runs")

wrkdir <- '/home/shiny/apps/shiny-uncertainty/'
# wrkdir <- 'C:/Users/CLam/Desktop/shiny-uncertainty/'

indicator.names <- c('Households', 'Employment')

# lookup tables and ancillary data
cnty.choices <- c("All" = "All", "King" = "King", "Kitsap" = "Kitsap", "Pierce" = "Pierce", "Snohomish" = "Snohomish")

rgs.lu <- read.csv(file.path(wrkdir, "data/fips_rgs.csv"), header = TRUE) %>% as.data.table()
rgs.lvl <- rgs.lu[rev(order(county_name, rgs_id))][,fips_rgs_name := as.character(fips_rgs_name)][['fips_rgs_name']]

cities.lu <- read.csv(file.path(wrkdir, "data/cities.csv"), header = TRUE) %>% as.data.table()
setnames(cities.lu, "County", "county_name")
# cities.lvl <- cities.lu[rev(order(county_name, city_name))][['city_name']]

pol.num <- read.csv(file.path(wrkdir, "data/policy_nums.csv"), stringsAsFactors = FALSE, header = TRUE) %>% as.data.table()
pol.num.city <- read.csv(file.path(wrkdir, "data/policy_nums_city.csv"), stringsAsFactors = FALSE, header = TRUE) %>% as.data.table()

cap <- read.csv(file.path(wrkdir, "data/UStotal_capacity_cities_run101.csv"), stringsAsFactors = FALSE, header = TRUE) %>% as.data.table()
setnames(cap, c("DU", "job_capacity"), c("households", "employment"))

# scan for all directories in servers
allruns <- list()
for (b in 1:length(base)) {
  fdirlist <- list.dirs(base[[b]], full.names = TRUE, recursive = FALSE)
  ndirlist <- list.dirs(base[[b]], full.names = FALSE, recursive = FALSE)
  dirlist <- setNames(fdirlist, ndirlist)
  allruns[[length(allruns)+1]] <- dirlist
}
names(allruns) <- names(base) %>% toupper

# subset for confidence interval directories
bm.runs <- lapply(allruns, function(x) x[grep("^bm_", names(x))]) %>% .[lapply(.,length)>0]
