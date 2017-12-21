library(shinythemes)
library(ggplot2)
library(plotly)
library(grid)
library(gridExtra)
library(reshape2)
library(data.table)
library(magrittr)
library(stringr)

base <- list(Modelsrv6 = "/media/modelsrv6d/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv8 = "/media/modelsrv8d/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv3 = "/media/modelsrv3e/opusgit/urbansim_data/data/psrc_parcel/runs"
             )
# base <- list(Modelsrv6 = "//modelsrv6/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Modelsrv8 = "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
#              Modelsrv3 = "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
#              )
# base <- list(Modelsrv3 = "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
#             Modelsrv8 = "/Volumes/d$/opusgit/urbansim_data/data/psrc_parcel/runs")

indicator.names <- c('Households', 'Employment')

# scan for all directories in servers
allruns <- list()
for (b in 1:length(base)) {
  fdirlist <- list.dirs(base[[b]], full.names = TRUE, recursive = FALSE)
  ndirlist <- list.dirs(base[[b]], full.names = FALSE, recursive = FALSE)
  dirlist <- setNames(fdirlist, ndirlist)
  allruns[[length(allruns)+1]] <- dirlist
}
names(allruns) <- names(base) %>% toupper

# how many FAZes to plot
nlargest <- 20

# should differences between groups be considered
dif.between.groups <- TRUE