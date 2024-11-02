rm(list = ls())
setwd('../data/tepco')
getwd()

library(dplyr)

sjis <- list.files(path = 'sjis')
utf8 <- substr(sjis, 1, nchar(sjis) - 5)

cmd <- paste('pwd; iconv -f sjis -t utf8 -o', 
             paste0('./tmp_utf8/', utf8), 
             paste0('./sjis/', sjis)) 

for ( i in seq_along(cmd) )
{
  system(cmd[i])
  if ( i <= 3 )
  {
    d0 <- read.csv(paste0('./tmp_utf8/', utf8[i]))
    t <- as.POSIXct(paste(d0$DATE, d0$HE - 1), format = '%Y%m%d %H')
    d1 <- data.frame(DATE  = format(t, '%Y %-m %-d'),
                     TIME  = format(t, '%-H %M'),
                     mankw = round(d0$MW / 10))
    colnames(d1) <- c('DATE', 'TIME', '実績(万kW)')
  } else
  {
    d0 <- read.csv(paste0('./tmp_utf8/', utf8[i]))
    t <- as.POSIXct(paste(d0$DATE, d0$TIME), format = '%Y/%m/%d %H:%M')
    d1 <- data.frame(DATE  = format(t, '%Y %-m %-d'),
                     TIME  = format(t, '%-H %M'),
                     mankw = d0[3])
  }
    colnames(d1) <- c('DATE', 'TIME', '実績(万kW)')
    write.csv(d1, utf8[i], quote = F, row.names = F)
}
