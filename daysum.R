# script to update table sum from obs.
# to be run occasionaly, will check for already entered data before inserting
# 20131031 HCrockford
library(RPostgreSQL)
library(gdata)
library(ggplot2)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="milkmon")
fid = 6661                             # paddy

fids = dbGetQuery(con, 'SELECT DISTINCT fid FROM obs;')

sapply(fids, fn)

#fn 
daysummer = function(fid) {
	w = dbGetQuery(con, sprintf('SELECT sdat as Date, svol as Volume, sfatkg as FatKG, sprotkg as ProtienKG, sbmcc as BMCC FROM obs WHERE fid = %s;', fid))
	names(w) = c('Date', 'Volume', 'FatKG', 'ProteinKG','BMCC')
	w = transform(w, Date = as.POSIXct(Date, format = '%d/%m/%Y'))
	w = unknownToNA(w, unknown = 0.00)
	w$tcells = w$BMCC * w$Volume
	wsumm = aggregate(w[,-c(1,5)], list(w$Date), sum)
	names(wsumm)[1] = 'Date'
	wsumm = wsumm[order(wsumm$Date),]
	nday = round(as.numeric(wsumm$Date - c(NA,wsumm$Date[-length(wsumm$Date)])) / (24*60*60),0 )
	nday = NAToUnknown(nday, unknown = 0, force = T)
	dvol = with(wsumm, Volume / nday)
	wsummn = transform(wsumm, 
			  Volume = Volume / nday,
			  FatKG = FatKG / nday,
			  ProteinKG = ProteinKG / nday,
			  tcells = tcells / nday
			  )
	diff.dvol = dvol - c(NA, dvol)[-length(dvol)]
	fups = which(diff.dvol > 5000)         # if cows jumped/dropped >5000 l in day, call bullshit
	wsummn$Volume[c(fups, fups-1)] = (wsummn$Volume[fups] + wsummn$Volume[fups-1]) / 2
	wsummn$FatKG[c(fups, fups-1)] = (wsummn$FatKG[fups] + wsummn$FatKG[fups-1]) / 2
	wsummn$ProteinKG[c(fups, fups-1)] = (wsummn$ProteinKG[fups] + wsummn$ProteinKG[fups-1]) / 2
	wsummn$tcells[c(fups, fups-1)] = (wsummn$tcells[fups] + wsummn$tcells[fups-1]) / 2
	wsumm = transform(wsummn, 
			  FatPC = 100*FatKG / Volume,
			  ProteinPC = 100*ProteinKG / Volume,
			  BMCC = tcells / Volume
			  )
	return(wsumm)
}
