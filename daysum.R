# script to update table sum from obs.
# to be run occasionaly, will check for already entered data before inserting
# 20131031 HCrockford
library(gdata)
library(ggplot2)
library(xts)
w = read.csv('../wort/wort.csv')
w = transform(w, Date = as.POSIXct(Date, format = '%d/%m/%Y'))
w = unknownToNA(w, unknown = 0.00)
w$tcells = w$BMCC * w$Volume
wsumm = aggregate(w[,c(2,5,7,9,11,13)], list(w$Date), sum)
names(wsumm)[1] = 'Date'
wsumm = wsumm[order(wsumm$Date),]
# sdats = as.Date(as.Date(min(w$Date)):as.Date(max(w$Date)))[-c(1:2,length(wsumm$Date))]
nday = round(as.numeric(wsumm$Date - c(NA,wsumm$Date[-length(wsumm$Date)])) / (24*60*60),0 )
# sometimes tanks over few days - need to divide total vol & kg by nday.
nday = NAToUnknown(nday, unknown = 0, force = T)
# volrep = rep(wsumm$Volume,times = as.integer(nday))
wsumm = transform(Volume = Volume / nday,
		  FatKG = FatKG / nday,
		  ProteinKG = ProteinKG / nday,
		  LactoseKG = LactoseKG / nday,
		  SNFKG = SNFKG / nday,
		  tcells = tcells / nday
		  )

dvol = with(wsumm, Volume / nday)
dfat = with(wsumm, FatKG / nday)

# misses them when we get a tank early one day and late the next
which(dvol >17500)
cbind(wsumm[130:140,1:2], dvol[130:140])
cbind(wsumm[490:510,1:2], dvol[490:510])
cbind(nday, wsumm[,1:2], dvol)[350:370,] # nday wrong?? - FIXED w round

# find with moving average
plot(wsumm$Volume, type = 'l')
lines(filter(wsumm$Volume, rep(1/7,7), sides = 1), col = 'red')
plot(wsumm$Volume - filter(wsumm$Volume, rep(1/7,7), sides = 1), type = 'l')
plot(wsumm$Volume - filter(wsumm$Volume, rep(1/14,14), sides = 1), type = 'l')

# easier with differencing
diff.dvol = dvol - c(NA, dvol)[-length(dvol)]
# diff(wsumm$Volume,1) - equivalent??
# plot(diff.dvol,type = 'l')
# abline(h = 5000,col = 'red')
fups = which(diff.dvol > 5000)
dvol.fix = (dvol[fups] + dvol[fups-1]) / 2
dvol[c(fups, fups-1)] = dvol.fix
plot(dvol, type = 'l')


wsumm = transform(wsumm, doy = as.numeric(format(Date, '%j')), yr = format(Date, '%Y'))
mon = c(31, 28,31,30,31,30,31,31,30,31,30,31)
mon = c(1,cumsum(mon))[1:12]
monl = format(seq(as.Date('2013/1/1'), as.Date('2013/12/31'), 'month'), '%b')


wsumm = transform(wsumm, 
		  FatPC = 100*FatKG / Volume,
		  ProteinPC = 100*ProteinKG / Volume,
		  LactosePC = 100*LactoseKG / Volume, 
		  SNFPC = 100*SNFKG / Volume, 
		  BMCC = tcells / Volume
		  )

###########
## plots
#########


ggplot(wsumm, aes(doy, Volume,color = yr)) + geom_line(aes(color = yr)) + scale_x_continuous(breaks = mon, labels = monl) + theme(axis.text.x = element_text(hjust = -0.4)) + stat_smooth()
ggplot(wsumm, aes(doy, ProteinPC,color = yr)) + geom_line(aes(color = yr)) + scale_x_continuous(breaks = mon, labels = monl) + theme(axis.text.x = element_text(hjust = -0.4)) + stat_smooth()
ggplot(wsumm, aes(doy, Volume,color = yr)) + geom_line(aes(color = yr)) + scale_x_continuous(breaks = mon, labels = monl) + theme(axis.text.x = element_text(hjust = -0.4))
ggplot(wsumm, aes(doy, Volume,color = yr)) + geom_line(aes(color = yr)) + scale_x_continuous(breaks = mon, labels = monl) + theme(axis.text.x = element_text(hjust = -0.4))
ggplot(wsumm, aes(doy, BMCC,color = yr)) + geom_line(aes(color = yr)) + scale_x_continuous(breaks = mon, labels = monl) + theme(axis.text.x = element_text(hjust = -0.4)) + stat_smooth(degree = 0,span = 0.1)
# BMCC in SEPT 2010

ggplot(w, aes(doy, FatPercentage,color = yr)) + 
geom_point() + 
stat_smooth() + 
scale_x_continuous(breaks = mon, labels = monl) + 
theme(axis.text.x = element_text(hjust = -0.4)) +
ggtitle('Wortley Pastoral Milk Fat percentage, 2010 - 2013') + 
xlab('')
ggsave('wort.pdf')


ggplot(w, aes(doy, FatPercentage,color = yr)) + geom_line(aes(color = yr)) + scale_x_continuous(breaks = mon, labels = monl) + theme(axis.text.x = element_text(hjust = -0.4))
ggplot(w, aes(doy, FatPercentage,color = yr)) + geom_point() + stat_smooth(degree = 0,span = 0.1)
