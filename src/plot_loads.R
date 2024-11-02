rm(list = ls())
if (!require(RSQLite)) install.packages("RSQLite")

con <- dbConnect(SQLite(), 'load.db')
dbListTables(con)
d0 <- dbGetQuery(con, 'SELECT datetime, mw FROM TEPCO WHERE datetime BETWEEN "2021-05-24 00:00:00" AND "2021-06-20 23:00:00"')
#dbSendQuery(con, 'CREATE TABLE TEPCO AS SELECT * FROM TEPCO_TEST')
#dbSendQuery(con, 'DROP TABLE tepco5min')
#dbSendQuery(con, 'DROP TABLE tepco5min_mva')
dbDisconnect(con)

tail(d0)

px <- as.POSIXlt(d0$datetime)
x <- seq_along(px)
at.x <- seq(1, length(px), 24*7)
gw <- round(d0$mw / 1e3)

cairo_pdf('load.pdf', height = 6, width = 6,
          family = 'UD Digi Kyokasho NP-R')

matplot(x = x, y = gw, type = 'n', xaxt = 'n', cex.main = 1.0,
        main = '電力需要', xlab = '2021年', ylab = '電力 [ GW ]')

abline(v = at.x, col = gray(0.8), lty = 2, lwd = 0.5 )

axis(side = 1, at = at.x+12, line = 0.2, tick = F, 
     label = paste(px[at.x]$mon + 1, '/', px[at.x]$mday))

is.mn <- px$hour == 0
mtext(text = format(px[is.mn], '%a'), side = 1, line = 0, at = 12+x[is.mn])

matlines(x = x, y = gw, type = 'l', col = 'blue')

for (i in dev.list()) dev.off()

