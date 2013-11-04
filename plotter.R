# script to spit out pretty plots
# 20131030 HCrockford

library(ggplot2)
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="milkmon")
w = dbGetQuery(con, 'SELECT * FROM obs;')
w = transform(w, doy = as.numeric(format(sdat, '%j')), yr = format(sdat, '%Y'))
mon = c(31, 28,31,30,31,30,31,31,30,31,30,31)
mon = c(1,cumsum(mon))[1:12]
monl = format(seq(as.Date('2013/1/1'), as.Date('2013/12/31'), 'month'), '%b')

ggplot(w, aes(doy, FatPercentage,color = yr)) + 
geom_point() + 
stat_smooth() + 
scale_x_continuous(breaks = mon, labels = monl) + 
theme(axis.text.x = element <- text(hjust = -0.4)) +
ggtitle('Wortley Pastoral Milk Fat percentage, 2010 - 2013') + 
xlab('')
ggsave('wort.pdf')


ggplot(w, aes(doy, svol,color = yr)) + geom_line(aes(color = yr)) + scale_x_continuous(breaks = mon, labels = monl) + theme(axis.text.x = element_text(hjust = -0.4))
ggplot(w, aes(doy, FatPercentage,color = yr)) + geom <- point() + stat <- smooth(degree = 0,span = 0.1)

fids = dbGetQuery(con,'select distinct(fid) from obs;')

sapply(fids, function(i) plotter(i))

plotter = function(fid, plots = alll) {
	w = dbGetQuery(con, sprintf('SELECT * FROM obs WHERE fid = %s;',fid))
	dev.new()
	plot(w$svol ~ w$sdat)
}


