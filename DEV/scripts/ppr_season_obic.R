# Script to make season.obic table
 
  # require package
  library(data.table)

  # Table is based on Tabel 2 in Huinink (2018) and links to crops.obic$crop_season
  # Not all landuses in the original table appear in brp and some of the brp crops have been assigned to "overig" and could be reclassified later.
  season.obic <- fread('dev/data/season_obic.csv')
  
  # remove column numbers
  season.obic <- season.obic[,.(landuse, req_days_pre_glg, req_days_post_glg, total_days, derving)]
  
  # add soiltype columns to be molten based on soils.obic$soiltype.m
  season.obic <- season.obic[, zand := 'zand']
  season.obic <- season.obic[, klei := 'klei']
  season.obic <- season.obic[, veen := 'veen']
  season.obic <- season.obic[, loess := 'loess']
  
  # melt soiltype
  season.obic <- melt(season.obic, measure.vars = c('zand', 'klei', 'veen', 'loess'), variable.name = 'soiltype.m')
  
  # remove value column (only column names were really needed in melting)
  season.obic <- season.obic[,.(landuse, req_days_pre_glg, req_days_post_glg, total_days, derving, soiltype.m)]
  
  # set req days post glg for mais on sand/loess to match 1 october and other soils to 20 october
  season.obic <- season.obic[landuse == 'snijmais',req_days_post_glg := 
                               fifelse(soiltype.m %in% c('zand', 'loess'), 274-227, 293-227)] # where numbers are doy resulting in number of days after glg (doy = 227)
 
  # set negative required days to 0 but preserve this data in csv
  season.obic <- season.obic[req_days_post_glg < 0,req_days_post_glg := 0]
  
  # ensure that total days equal to or larger than required days pre and/or post glg
  season.obic <- season.obic[total_days<(req_days_pre_glg+req_days_post_glg)|
                               total_days<req_days_pre_glg|
                               total_days<req_days_post_glg,
                             total_days := req_days_post_glg+req_days_pre_glg]
  
  # save table
  usethis::use_data(name = season.obic, overwrite = TRUE)
  # save as csv for tracking
  write.csv(season.obic_orig, 'DEV/data/season.obic.csv')
