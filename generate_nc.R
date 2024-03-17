rm(list = ls()); while(!is.null(dev.list()))dev.off()

invisible(suppressWarnings(suppressPackageStartupMessages(lapply(
  c('tidyverse', 'lubridate', 'data.table', 'terra'),
  library, character.only = TRUE
))))

tanggal <- seq.Date(ymd('2021-01-01'), ymd('2022-12-31'), by = 'day')

tmp <- data.table(a=1:12) %>% 
  .[, tanggal %>% as.character := lapply(seq_along(tanggal), \(x) rnorm(12))] %>% 
  .[, !'a']

output <- rast(
  nrows = 3, ncols = 4, nlyrs = length(tanggal), crs = '+proj=longlat',
  extent = ext(110,113,-2,0), time = tanggal, vals = tmp, names = tanggal %>% as.character
)
