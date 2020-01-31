library(s4wr)

date_end <- lubridate::now()
date_beg <- get_datelast_ais(){ "SELECT MAX(dtime) FROM ais_data" }

urls <- dates2urls(date_beg, date_end)

# Google Drive archive?

df   <- urls2df(urls)
# save df to db.ais_data, return tbl connection to db
# + columns: url, date_loaded (replaces log)

segs <- df2segs(df){

  pts  <- df2pts(pts, date_beg = NULL, date_end = NULL, ...)
  # save pts to db.ais_pts, return tbl connection to db
  # filtering step to not include points too close to one another
  
  segs <- pts2segs(pts, date_beg = NULL, date_end = NULL, ...)
  # save segs to db.ais_segs, return tbl connection to db
  
}

segs <- segs2compliance(segs, date_beg = NULL, date_end = NULL, ...)
# save segs to db.ais_segs, return tbl connection to db
# need: lon_beg, lon_end, dtime_beg, dtime_end, ....
# 

# TODO: pull shiny time slider https://github.com/marinebon/sdg14-shiny
