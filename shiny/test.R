library(data.table)
library(purrr)
library(RCurl)
library(openxlsx)
library(dplyr)

# dir <- "//modelsrv6/d$/opusgit/urbansim_data/data/psrc_parcel/runs/bm_36_37"
dir <- "//modelsrv6/d$/opusgit/urbansim_data/data/psrc_parcel/runs/bm_44_7runs"
filename <- "cache_directories.txt"
rawtbl <- read.table(file.path(dir, filename), col.names = TRUE, stringsAsFactors=F) %>% as.data.table()
rawtbl

corr.runs <- rawtbl[[colnames(rawtbl)]] %>% lapply(function(x) basename(x)) %>% unlist()

base <- list(Modelsrv5 = "//modelsrv5/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv6 = "//modelsrv6/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv8 = "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
             Modelsrv3 = "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
)
allruns <- list()
for (b in 1:length(base)) {
  fdirlist <- list.dirs(base[[b]], full.names = TRUE, recursive = FALSE)
  ndirlist <- list.dirs(base[[b]], full.names = FALSE, recursive = FALSE)
  dirlist <- setNames(fdirlist, ndirlist)
  allruns[[length(allruns)+1]] <- dirlist
}
names(allruns) <- names(base) %>% toupper

r <- corr.runs[1:2]
allruns[[2]][match(corr.runs[1:2], names(allruns[[2]]))] # returns named vector
test <- map(allruns, pluck(r)) # works for one element



sub.runs <- NULL
for (i in seq_along(r)) {
  s <- map(allruns, ~ .[r[i]]) %>% discard(is.na) 
  ifelse(is.null(sub.runs), sub.runs <- s, sub.runs <- append(sub.runs, s))
}
flatten_chr(sub.runs)
print(sub.runs)

polnum <- read.xlsx("J:/Projects/V2050/STC_RGS/Script/RGS2050_STC.xlsx", sheet = 1, colNames = TRUE)
poldf <- polnum %>%
  mutate(fips_rgs_id = as.integer(paste0(County, "0", RG))) %>%
  left_join(rgs.lu, by = c("fips_rgs_id")) %>%
  select(1:8)
write.csv(poldf, "C:/Users/CLam/Desktop/shiny-uncertainty/data/policy_nums.csv")


new.rgs.lu <- read.csv(file.path(wrkdir, "data/fips_rgs_new.csv"), header = TRUE) %>% as.data.table()

d <- new.rgs.lu %>%
  rename(fips_rgs_id = `ï..fips_rgs_id`) %>%
  mutate(fips_rgs_name1 = fips_rgs_name, fips_rgs_label = fips_rgs_id) %>%
  separate(fips_rgs_name1, c("county_name", "area_name"), sep = '-') %>%
  mutate(county_name = trimws(county_name), area_name = trimws(area_name))

write.csv(d, file.path(wrkdir, "data/fips_rgs.csv"), row.names = FALSE)

