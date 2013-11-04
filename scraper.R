#!/usr/bin/Rscript
# mail scraper fn in R
# 20131029 HCrockford
f <- file("stdin")
open(f)
mail = readLines(f) # use for mail reading
# mail = readLines(con = 'bid')
dat = as.POSIXct(unlist(strsplit(grep('(Date:) [0-9]',mail, value = T),': '))[2], format = '%d/%m/%Y')
fid = as.numeric(unlist(strsplit(grep('Farm No. :',mail, value = T), split = ' '))[4])
# redundency in storing fname and fid - use other table w fkey constraint?
fname = unlist(strsplit(grep('Farm No. :',mail, value = T), split = ': '))[3]
dstart = grep('Date:       PU', mail)
d = dstart + 1
lin = mail[d]
datl = NULL
while(lin != ''){
	sdat = as.POSIXct(unlist(strsplit(lin, ' +'))[1],format = '%d/%m/%Y')
	svol = as.numeric(unlist(strsplit(lin, ' +'))[3])
	sfatpc = as.numeric(unlist(strsplit(lin, ' +'))[4])
	sfatkg = as.numeric(unlist(strsplit(lin, ' +'))[5])
	sprotpc = as.numeric(unlist(strsplit(lin, ' +'))[6])
	sprotkg = as.numeric(unlist(strsplit(lin, ' +'))[7])
	stemp = as.numeric(unlist(strsplit(lin, ' +'))[8])
	sbmcc = as.numeric(unlist(strsplit(lin, ' +'))[9])
	datl = rbind(datl, data.frame(sdat, svol, sfatpc, sfatkg, sprotpc, sprotkg, stemp, sbmcc))
	d = d + 1
	lin = mail[d]
}
res = cbind(dat, fid, fname, datl)
res$key = apply(res[,c(2,4:7)],1,function(i) paste(i, collapse = ''))
########
### DB
########
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="milkmon")
dbWriteTable(con,'tobs',res[-1,],row.names = F)
nobs = dbGetQuery(con, 'SELECT COUNT(*) from tobs WHERE key NOT IN (SELECT key FROM obs);')
nfarm = dbGetQuery(con, 'SELECT COUNT(DISTINCT(fid)) FROM tobs;')
write(paste('inserted',nobs,'new obs from',nfarm,'farms','\n'), stdout())
dbGetQuery(con, 'INSERT INTO obs (SELECT * from tobs WHERE key NOT IN (SELECT key FROM obs));')
dbGetQuery(con, 'DROP TABLE tobs;')
dbDisconnect(con)
