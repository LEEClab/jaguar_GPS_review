#--------------------------------------------------------
#
# Exploring Jaguar GPS movement data sampling 
# based on the datapaper from Morato et al. 2018
#
# Milton Cezar Ribeiro
# Julia E.F. Oshima
# Bernardo Brandão Niebuhr
# Milene Amancio Alves-Eigenheer
# Vanesa Berjarano
# Claudia Kanda
#
# May 2020
# GNU/GPLv2 license
#--------------------------------------------------------

# Remove previous R objects
rm(list = ls())

#-----------------------------
# Set up 

# Load packages
if(!require(install.load)) install.packages('install.load'); library(install.load)
install.load::install_load('move', 'adehabitatLT', 'tidyverse')

# Options

# exportFIG: if TRUE, export figures; if FALSE, plot them within R
exportFIG <- TRUE

# Directories

# Code folder
codedir <- 'code/'

# Data folder
datadir <- 'data/'

# Output folder
outdir <- 'output/'

#-----------------------------
# Load data

# Load movement data

# Raw movement data
mov.data <- read.csv(paste0(datadir, 'jaguar_movement_data.csv'), header = T, sep = ',', dec = '.', comment.char = '')

head(mov.data)
ncol(mov.data)
nrow(mov.data)
str(mov.data)

# Individual info
ind.info <- read.table(paste0(datadir, 'Jaguar_additional information.txt'), header = T, sep = '\t', dec = '.')
head(ind.info)
nrow(ind.info)
#View(ind.info)
colnames(ind.info)

# Function to keep only the numeric part of the 
# planned.schedule column
ff <- function(x) {
  init <- 1
  endd <- gregexpr('h', x, ignore.case = T)[[1]][1]-1
  out <- substr(x, init, endd)
  as.numeric(ifelse(out == '', x, out))
}

ff(as.character(ind.info$Planned.Schedule)[1])
ind.info$planned.schedule.numeric <- unlist(as.numeric(sapply(as.character(ind.info$Planned.Schedule), ff)))

# Check planned schedule in hours
ind.info[c('Planned.Schedule', 'planned.schedule.numeric')]

#-----------------------------
# Organize data

mov.data.org <- mov.data

# Add 2000 to years
get.year <- function(time.stamp) {
  init <- gregexpr('/', time.stamp, fixed = T)[[1]][2] + 1
  end <- gregexpr(' ', time.stamp, fixed = T)[[1]][1] - 1
  substr(time.stamp, init, end)
}

# Test
get.year(time.stamp = mov.data.org$timestamp[10000])
# All individuals
year <- as.numeric(sapply(mov.data.org$timestamp, get.year))
table(year)

# Add 1900/2000
new.year <- as.character(ifelse(year > 50, year + 1900, year + 2000))
table(new.year)

# New dates
set.year <- function(time.stamp, year) {
  init <- gregexpr('/', time.stamp, fixed = T)[[1]][2]
  end <- gregexpr(' ', time.stamp, fixed = T)[[1]][1]
  paste(substr(time.stamp, 1, init), year,
        substr(time.stamp, end, nchar(time.stamp)), sep = "")
}

# Test
set.year(time.stamp = as.character(mov.data.org$timestamp[10000]), year = '2013')
# All individuals
date.time <- as.character(mapply(set.year, as.character(mov.data.org$timestamp),
                                 new.year))
str(date.time)
date.time

# Date/Time as POSIXct object
mov.data.org$timestamp.posix <- as.POSIXct(date.time, 
                                           format = "%m/%d/%Y %H:%M", tz = 'GMT')

#-----------------------------
# Distribution of fix rates

mov.data.diff <- mov.data.org

# Calculation of time between fixes
time.between <- function(individual, dat) {
  # Select individual
  ind <- dat[dat$individual.local.identifier..ID. == individual,]
  # Calculate difference in time, in hours, and return
  c(as.numeric(diff.POSIXt(ind$timestamp.posix), units = 'hours'), NA)
}

# Test - one individuals
time.between(individual = 2, dat = mov.data.diff)
# Test two individuals
unlist(sapply(1:2, FUN = time.between, dat = mov.data.diff))

# All individuals
inds <- unique(mov.data.diff$individual.local.identifier..ID.)
mov.data.diff$time.diff <- round(unlist(sapply(inds, FUN = time.between, dat = mov.data.diff)))

head(mov.data.diff)
write.csv(mov.data.diff, paste0(datadir, 'mov_time_diff.csv'), row.names = F)

# Looking at long time between which individuals were not monitored
(ww <- which(mov.data.diff$time.diff < 0))
mov.data.diff[(ww[1]-2):(ww[1]+2),]

# Sampling of individuals
if(exportFIG) {
  pdf(paste0(outdir, 'monitoring_time_julian_days.pdf'), width = 10, height = 30)
}
plot(mov.data.diff$timestamp.posix, rep(0, length(mov.data.diff$timestamp.posix)),
     ylim = c(1,length(inds)), type = 'n', axes = F,
     xlab = 'Julian days', ylab = 'Individual')
abline(h = 1:117, col = 'grey', lwd = 3, lty = 2)

min.x.year <- format(min(mov.data.diff$timestamp.posix), '%Y')
min.x <- as.POSIXct(paste('1/1/', min.x.year, ' 00:00', sep = ''), format = '%m/%d/%Y %H:%M', tz = 'UTC')
max.x.year <- format(max(mov.data.diff$timestamp.posix), '%Y')
max.x <- as.POSIXct(paste('12/12/', max.x.year, ' 00:00', sep = ''), format = '%m/%d/%Y %H:%M', tz = 'UTC')

# abline(v = seq(min(mov.data.diff$timestamp.posix), max(mov.data.diff$timestamp.posix), 'years'),lwd = 3)
abline(v = seq(min.x, max.x, 'years'), lwd = 3, lty = 2)
axis.POSIXct(1, at = seq(min.x, max.x, 'years'), format = "%m/%Y")
axis.POSIXct(3, at = seq(min.x, max.x, 'years'), format = "%m/%Y")
axis(2, at = 1:117, labels = 1:117, las = 1)
box()
# cols <- rainbow(117)
cols <- rainbow(length(levels(ind.info$Collar.Brand)))[ind.info$Collar.Brand] 
for(i in inds) {
  ind <- mov.data.diff[mov.data.diff$individual.local.identifier..ID. == i,]
  points(ind$timestamp.posix, rep(i, length(ind$timestamp.posix)), col = sample(cols, 1),
         pch = 20)
}
if(exportFIG) dev.off()

# Replacing individual points in time (same begining date)
replace.in.time <- function(individual, dat, begin) {
  # Select individual
  ind <- dat[dat$individual.local.identifier..ID. == individual,]
  # Calculate the time difference between min.time and begin,
  # and subtract that from all time frames
  min.time <- min(ind$timestamp.posix)
  ind$timestamp.posix - (min.time - begin)
}

# Test - one individuals
replace.in.time(individual = 2, dat = mov.data.diff, begin = min.x)
# Test two individuals
do.call('c', sapply(1:2, FUN = replace.in.time, dat = mov.data.diff, begin = min.x))

# All individuals
inds <- unique(mov.data.diff$individual.local.identifier..ID.)
mov.data.diff$displaced.timestamp <- do.call('c', sapply(inds, FUN = replace.in.time, dat = mov.data.diff, begin = min.x))

# Plot
if(exportFIG) {
  pdf(paste0(outdir, 'monitoring_time_julian_days_displaced.pdf'), width = 10, height = 30)
}
plot(mov.data.diff$displaced.timestamp, rep(0, length(mov.data.diff$displaced.timestamp)),
     ylim = c(1,length(inds)), type = 'n', axes = F,
     xlab = 'Julian days', ylab = 'Individual')
abline(h = 1:117, col = 'grey', lwd = 3, lty = 2)

min.x.year <- format(min(mov.data.diff$displaced.timestamp), '%Y')
min.x <- as.POSIXct(paste('1/1/', min.x.year, ' 00:00', sep = ''), format = '%m/%d/%Y %H:%M', tz = 'UTC')
max.x.year <- format(max(mov.data.diff$displaced.timestamp), '%Y')
max.x <- as.POSIXct(paste('12/12/', max.x.year, ' 00:00', sep = ''), format = '%m/%d/%Y %H:%M', tz = 'UTC')

# abline(v = seq(min(mov.data.diff$timestamp.posix), max(mov.data.diff$timestamp.posix), 'years'),lwd = 3)
abline(v = seq(min.x, max.x, 'months'), lwd = 1, lty = 1, col = 'grey')
abline(v = seq(min.x, max.x, 'years'), lwd = 3, lty = 2)
axis.POSIXct(1, at = seq(min.x, max.x, 'years'), format = "%m/%Y")
axis.POSIXct(3, at = seq(min.x, max.x, 'years'), format = "%m/%Y")
axis(2, at = 1:117, labels = 1:117, las = 1)
box()
cols <- rainbow(117)
# cols <- rainbow(length(levels(ind.info$Collar.Brand)))[ind.info$Collar.Brand] 
for(i in inds) {
  ind <- mov.data.diff[mov.data.diff$individual.local.identifier..ID. == i,]
  points(ind$displaced.timestamp, rep(i, length(ind$displaced.timestamp)), col = sample(cols, 1),
         pch = 20)
}
if(exportFIG) dev.off()

#--------------

# Proportion of observation with less than 30 days between subsequent observations
# observed
less.30days <- mov.data.diff$time.diff[mov.data.diff$time.diff > 0 &
                                         mov.data.diff$time.diff < 30*24]
100*length(less.30days)/length(mov.data.diff$time.diff)

# Proportion of observation with less than 8 h between subsequent observations
# observed
less.8hours <- mov.data.diff$time.diff[mov.data.diff$time.diff > 0 &
                                         mov.data.diff$time.diff < 8]
100*length(less.8hours)/length(mov.data.diff$time.diff)

# planned
sum(ind.info$planned.schedule.numeric < 8)/nrow(ind.info)
inds.less <- ind.info$ID[ind.info$planned.schedule.numeric < 8]
planned.less.8hours <- length(mov.data.diff$Event_ID[mov.data.diff$individual.local.identifier..ID. %in% inds.less])
(prop.planned.less.8h <- planned.less.8hours/nrow(mov.data.diff))

length(less.8hours)/length(mov.data.diff$time.diff)/prop.planned.less.8h

# for 2 h, observed
less.2hours <- mov.data.diff$time.diff[mov.data.diff$time.diff > 0 &
                                         mov.data.diff$time.diff <= 2]
100*length(less.2hours)/length(mov.data.diff$time.diff)
# planned
sum(ind.info$planned.schedule.numeric <= 2)/nrow(ind.info)
inds.less <- ind.info$ID[ind.info$planned.schedule.numeric <= 2]
planned.less.2hours <- length(mov.data.diff$Event_ID[mov.data.diff$individual.local.identifier..ID. %in% inds.less])
(prop.planned.less.2h <- planned.less.2hours/nrow(mov.data.diff))

length(less.2hours)/length(mov.data.diff$time.diff)/prop.planned.less.2h

# Histogram
pdf(paste0(outdir, 'time_lag_movement.pdf'))
par(mfrow = c(4, 5), mar = c(2,2,2,2) + 0.1)
for(i in inds) {
  print(paste(i, paste(range(mov.data.diff$time.diff[mov.data.diff$individual.local.identifier..ID. == i], na.rm = T), collapse = ', '), sep = ': '))
  hist(mov.data.diff$time.diff[mov.data.diff$individual.local.identifier..ID. == i &
                                 mov.data.diff$time.diff > 0],
       main = paste('Individual', i), xlab = "", ylab = '')
}
dev.off()
hist(mov.data.diff$time.diff[mov.data.diff$time.diff > 0 & mov.data.diff$time.diff < 48],
     main = 'All individuals pooled', xlab = "Time between fixes (h)")

# For individual 1
hist(mov.data.diff$time.diff[mov.data.diff$individual.local.identifier..ID. == 2], breaks = 20,
     main = 'Individual 1', xlab = "Time between fixes (h)")
plot(density.default(mov.data.diff$time.diff[mov.data.diff$individual.local.identifier..ID. == 1],
                     na.rm = T), main = '')

#-----------------------------
# Transforms data into movement data for different packages

## adehabitatLT
# Transforms in ltraj object
coords <- data.frame(mov.data.diff$location.long, mov.data.diff$location.lat)
mov.traj <- as.ltraj(xy = coords, date=mov.data.diff$timestamp.posix, 
                     id=mov.data.diff$individual.local.identifier..ID., 
                     burst=mov.data.diff$individual.local.identifier..ID., 
                     infolocs = mov.data.diff[,-c(3:4)])
#mov.traj.df <- ld(mov.traj)

mov.traj
# plot(mov.traj)
#hist(mov.traj.df$dist)

# Saving data frame with complete traj to check planned schedule
mov.traj.df <- ld(mov.traj)
# write.table(mov.traj.df,"mov_traj_df.txt", sep = '\t', dec = '.')

# Mode 1 and 2
mode.n.prop <- function(x, n) {
  tab <- table(round(x, digits = 1)) %>% sort(decreasing = T)
  c(as.numeric(names(tab)[n]), 100*as.numeric(tab[n])/sum(tab))
}
mode.n.prop(mov.traj.df$dt/3600, 1)
mode.n.prop(mov.traj.df$dt/3600, 2)

mov.traj.df %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    ind = as.numeric(unique(as.character(id))),
    min_dt_h = min(dt/3600, na.rm = T),
    max_dt_h = max(dt/3600, na.rm = T),
    median_dt_h = median(dt/3600, na.rm = T),
    mode1 = mode.n.prop(dt/3600, 1)[1],
    prop_mode1 = mode.n.prop(dt/3600, 1)[2],
    mode2 = mode.n.prop(dt/3600, 2)[1],
    prop_mode2 = mode.n.prop(dt/3600, 2)[2],
  ) %>% 
  dplyr::arrange(ind) %>% 
  write.table(paste0(outdir, 'mode_dt_each_individual.csv'), sep = ';', dec = '.', row.names = F)

# Reading data frame corrected/checked
data.time.checked <- read.csv(paste0(datadir, "mov_traj_df_correct.txt"),
         sep = "\t")
data.time.checked$date <- lubridate::dmy_hm(data.time.checked$date)

mov.traj <- dl(data.time.checked)

# Cut individual trajectories in different bursts if dt > 30 days
foo <- function(dt) {
  return(dt > (30*24*60*60))
}
mov.traj.cut <- cutltraj(mov.traj, "foo(dt)", nextr = TRUE)
mov.traj.cut
summary(mov.traj.cut)$burst

# Order cut trajectories according to individuals (1, 2, 3, ...)
mov.traj.cut.df <- ld(mov.traj.cut)
head(mov.traj.cut.df)
mov.traj.cut.df.ord <- mov.traj.cut.df[order(mov.traj.cut.df$id),]
head(mov.traj.cut.df.ord)

# Is the result of dt (t+1 - t) the same for ltraj calculation and
# diff.POSIXct? Yes!
head(cbind(mov.traj.cut.df.ord$dt/3600, mov.traj.cut.df.ord$time.diff))
tail(cbind(mov.traj.cut.df.ord$dt/3600, mov.traj.cut.df.ord$time.diff))
sum(mov.traj.cut.df.ord$dt/3600 - mov.traj.cut.df.ord$time.diff, na.rm = T) # Zero?

## move
# Organize data as a move package format
move.data <- move(x = mov.traj.cut.df.ord$x, y = mov.traj.cut.df.ord$y, 
                  time = mov.traj.cut.df.ord$date, 
                  proj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'),
                  data = mov.traj.cut.df.ord, animal = mov.traj.cut.df.ord$id, sensor = 'GPS')

# Plot
move::plot(move.data, pch = 19, xlab = 'Longitude', ylab = 'Latitude')

# Change projection of move data to aeqd
projection.aeqd <- '+proj=aeqd +ellps=WGS84'
move.data2 <- spTransform(move.data, CRSobj = projection.aeqd, center = T)

# ################Script Animove lesson 3
# 
# n.locs(move.data2 )#number of locations for each individual
# str(move.data2 )
# 
# #n.locs(bats)
# 
# ################
# 
# timeLags <- timeLag(move.data2 , units='hours')#o timelags tem que ser uma lista com os dados por individuo
# str(timeLags)
# 
# head(timeLag(move.data2, units="mins")) #vendo em minutos
# 
# ################
# 
# timeLagsVec <- unlist(timeLags)#tira os dados do formato de lista e passo a olhar os dados de maneira geral
# summary(timeLagsVec)#timelag consigo ver se tenho algo de errado nos dados, por exemplo o timelag minimo de 1 segudo, erro nos dados
# 
# par(mar=c(5,4,4,5))
# hist(timeLagsVec, breaks=50, main=NA, xlab="Time lag in hours")
# head(timeLagsVec)
# arrows(24.5,587.5,20.7,189.7, length=0.1)
# arrows(49.5,587.5,45.7,189.7, length=0.1)
# 
# ################
# png("timeLags_ate12h.png", 700, 400)
# hist(timeLagsVec[timeLagsVec<12], breaks="FD", 
#      main=NA, xlab="Time lag in hours")
# dev.off()
# summary(as.factor(round(timeLagsVec, 4)), maxsum=20)#olho quantos dados eu tenho em diferentes intervalos de tempo entre os fixes
# 
# 
# ################
# 
# ts <- timestamps(move.data2 )
# 
# library('lubridate')
# tapply(ts, hour(ts), length)#distribui??o das amostras em diferentes hor?rios do dia
# 
# png("fixespertimeoftheday.png", 700, 400)
# plot(tapply(ts, hour(ts), length), xlim=c(1,24), xlab='time of the day', ylab='number of fixes collected', pch=19)
# dev.off()
# 
# png("fixespermonth.png", 700, 400)
# plot(tapply(ts, month(ts), length), xlim=c(1,12), xlab='month', ylab='number of fixes collected', pch=19)
# dev.off()
# 
# ################
# 
# tapply(ts, list(month(ts),hour(ts)), length)#ploto uma tabela com os diferentes meses(eixo y) e horas (eixo x)
# plot(tapply(ts, list(month(ts),hour(ts)), length))
# 
# 
# ################
# 
# summary(unlist(distance(move.data2)))#distancia - step length, a unidade depende da leitura dos seus dados
# distalljaguar<-unlist(distance(move.data2))
# distalljaguarRealistic <- distalljaguar[distalljaguar<5000]
# 
# 
# png("Steplengthalltall.png", 700,400)
# hist(distalljaguarRealistic, main=NA,  xlim=c(0, 5000), ylim=c(0, 80000), xlab="Step length in m", breaks= seq(0, 5000, by=100), ylab="Frequency")
# dev.off()
# 
# ################
# 
# speeds <- unlist(speed(move.data2 ))#distance/timelag em segundos
# summary(speeds)
# #tems erros nestes dados pq a maxima velocidade esta muito acima do normal, entao tenho problemas entre os timelags
# 
# ################
# 
# speeds <- speeds[speeds<15]#filtro por velocidades + reais
# speedVsTimeLag <- data.frame(timeLag=timeLagsVec, 
#                              speeds=c(speeds, NA))
# speedVsTimeLag <- speedVsTimeLag[speedVsTimeLag$timeLag<12 
#                                  & speedVsTimeLag$speeds<15,]
# 
# png("Speed_alljaguar_lessthan15m-s.png", 700,400)
# plot(speedVsTimeLag$timeLag, speedVsTimeLag$speeds, xlab='Time lag (hours)', ylab='Speed (m/s)', pch=19)
# dev.off()
# 
# hist(speeds, main=NA, xlab="Speed in m/s", breaks="FD", ylab="Frequency")
# #olho pro histograma para ver a distribui??o das velocidades que se encaixa geralmente 
# #em uma distribui??o exponencial
# 
# ################
# 
# direction <- angle(move.data )
# summary(unlist(direction))
# 
# ############
# turnAngles <- unlist(turnAngleGc(move.data ))#relative angle, data must be in decimal degrees
# 
# png("turningangle_alljaguar.png", 700, 400)
# hist(unlist(turnAngles), breaks=18, xlab="Turning Angle", main=NA)
# dev.off()
# #padrao de forrageio, migratorio teria um padr?o mais centrado em 0 que quer dizer que seguir reto por longas distancias numa certa dire??o
# #uma curva em sino suave pode estar associada a home range com boundaries
# #tudo distribuido igualmente indica um home range behaviour
# 
# ######


# Recalculate distances in meters
str(move.data2)
mov.traj.df.meters <- as(move.data2, 'data.frame')

mov.traj.df.meters.clean <- mov.traj.df.meters[,11:ncol(mov.traj.df.meters)]
head(mov.traj.df.meters.clean)

coords <- data.frame(mov.traj.df.meters.clean$coords.x1, mov.traj.df.meters.clean$coords.x2)
mov.traj.meters.clean <- as.ltraj(xy = coords, date=mov.traj.df.meters.clean$timestamps, 
                                  id=mov.traj.df.meters.clean$id, 
                                  burst=mov.traj.df.meters.clean$burst, 
                                  infolocs = mov.traj.df.meters.clean[,-c(8:9)])
mov.traj.meters.clean
head(mov.traj.meters.clean[[100]])
mov.traj.meters.clean.df <- ld(mov.traj.meters.clean)
head(mov.traj.meters.clean.df)

# Plot distance versus dt
# 30 days pediod
plot(mov.traj.meters.clean.df$dt/3600, mov.traj.meters.clean.df$dist/1000,
     xlab = 'Time lag (h)', ylab = 'Distance (km)',
     pch = 19, col = grey(0.1, alpha = 0.3))
# 3 days pediod
plot(mov.traj.meters.clean.df$dt/3600, mov.traj.meters.clean.df$dist/1000,
     xlab = 'Time lag (h)', ylab = 'Distance (km)',
     xlim = c(0, 72), pch = 19, col = grey(0.1, alpha = 0.3))



### Plot distance versus time interval between fixes
plot(mov.traj.meters.clean.df$dt/3600+0.5, (mov.traj.meters.clean.df$dist+1),
     xlab = 'Time lag (h)', ylab = 'Distance (M in log scale)',
     log="xy",
     pch = 19, col = grey(0.1, alpha = 0.3))

mov.traj.meters.clean.df.2 <- mov.traj.meters.clean.df %>% 
  dplyr::select(dt, dist) %>% 
  dplyr::mutate(dt = dt/3600) %>% 
  dplyr::filter(dt > 0.3, dist > 1)

# plot
p <- ggplot(mov.traj.meters.clean.df.2, aes(dt+0.1, dist)) +
  theme_bw() + 
  geom_point(size = 0.7) +
  scale_y_log10(breaks = c(1,10,25,50,100,250,500,750,1250,2500,5000,10000,20000,40000)) +
  scale_x_log10(breaks = c(1, 2, 3, 4, 6, 9, 12, 16, 24, 30, 42, 60, 80, 100, 150, 200, 300, 500, 720)) +
  theme(axis.text.x = element_text(size=9)) + 
  theme(axis.text.y = element_text(size=10)) + 
  theme(axis.title.x = element_text(size=16)) + 
  theme(axis.title.y = element_text(size=16)) + 
  geom_smooth() + 
  ylab("Distance traveled between fixes\n(in m, log 10 scale)") +
  xlab("Interval between fixes (in h, log 10 scale)")
p
ggsave("dist_vs_time_lag.png", plot = p, device = "png", path = outdir, 
       width = 20, height = 14, units = "cm", dpi = 600)
ggsave("dist_vs_time_lag.tif", plot = p, device = "tiff", path = outdir, 
       width = 20, height = 14, units = "cm", dpi = 600)

# summary statistics
summary(mov.traj.meters.clean.df.2$dist)
quantile(mov.traj.meters.clean.df.2$dist, probs=c(0.05, 0.5, 0.95))

install.load::install_load("Rmisc")
CI(mov.traj.meters.clean.df.2$dist, ci = 0.95)

# dividing data among intervals of fix rate, to test for differences
mov.traj.meters.clean.df.mcr.01<-subset(mov.traj.meters.clean.df, dt/3600<1)
mov.traj.meters.clean.df.mcr.01<-subset(mov.traj.meters.clean.df.mcr.01, select=c(dt, dist))
mov.traj.meters.clean.df.mcr.01$period<-"a< 1 h"
plot(density(mov.traj.meters.clean.df.mcr.01$dist))

mov.traj.meters.clean.df.mcr.02<-subset(mov.traj.meters.clean.df, dt/3600<2)
mov.traj.meters.clean.df.mcr.02<-subset(mov.traj.meters.clean.df.mcr.02, dt/3600>1)
mov.traj.meters.clean.df.mcr.02<-subset(mov.traj.meters.clean.df.mcr.02, select=c(dt, dist))
mov.traj.meters.clean.df.mcr.02$period<-"b1 to 2 h"
plot(density(mov.traj.meters.clean.df.mcr.02$dist))

mov.traj.meters.clean.df.mcr.03<-subset(mov.traj.meters.clean.df, dt/3600<3)
mov.traj.meters.clean.df.mcr.03<-subset(mov.traj.meters.clean.df.mcr.03, dt/3600>2)
mov.traj.meters.clean.df.mcr.03<-subset(mov.traj.meters.clean.df.mcr.03, select=c(dt, dist))
mov.traj.meters.clean.df.mcr.03$period<-"c2 to 3 h"
plot(density(mov.traj.meters.clean.df.mcr.03$dist))

mov.traj.meters.clean.df.mcr.04<-subset(mov.traj.meters.clean.df, dt/3600<4)
mov.traj.meters.clean.df.mcr.04<-subset(mov.traj.meters.clean.df.mcr.04, dt/3600>3)
mov.traj.meters.clean.df.mcr.04<-subset(mov.traj.meters.clean.df.mcr.04, select=c(dt, dist))
mov.traj.meters.clean.df.mcr.04$period<-"d3 to 4 h"
plot(density(mov.traj.meters.clean.df.mcr.04$dist))

mov.traj.meters.clean.df.mcr.06<-subset(mov.traj.meters.clean.df, dt/3600<6)
mov.traj.meters.clean.df.mcr.06<-subset(mov.traj.meters.clean.df.mcr.06, dt/3600>4)
mov.traj.meters.clean.df.mcr.06<-subset(mov.traj.meters.clean.df.mcr.06, select=c(dt, dist))
mov.traj.meters.clean.df.mcr.06$period<-"e4 to 6 h"
plot(density(mov.traj.meters.clean.df.mcr.06$dist))

mov.traj.meters.clean.df.mcr.08<-subset(mov.traj.meters.clean.df, dt/3600<8)
mov.traj.meters.clean.df.mcr.08<-subset(mov.traj.meters.clean.df.mcr.08, dt/3600>6)
mov.traj.meters.clean.df.mcr.08<-subset(mov.traj.meters.clean.df.mcr.08, select=c(dt, dist))
mov.traj.meters.clean.df.mcr.08$period<-"f6 to 8 h"
plot(density(mov.traj.meters.clean.df.mcr.08$dist))

mov.traj.meters.clean.df.mcr.12<-subset(mov.traj.meters.clean.df, dt/3600<12)
mov.traj.meters.clean.df.mcr.12<-subset(mov.traj.meters.clean.df.mcr.12, dt/3600>8)
mov.traj.meters.clean.df.mcr.12<-subset(mov.traj.meters.clean.df.mcr.12, select=c(dt, dist))
mov.traj.meters.clean.df.mcr.12$period<-"g8 to 12 h"
plot(density(mov.traj.meters.clean.df.mcr.12$dist))

mov.traj.meters.clean.df.mcr.24<-subset(mov.traj.meters.clean.df, dt/3600<24)
mov.traj.meters.clean.df.mcr.24<-subset(mov.traj.meters.clean.df.mcr.24, dt/3600>12)
mov.traj.meters.clean.df.mcr.24<-subset(mov.traj.meters.clean.df.mcr.24, select=c(dt, dist))
mov.traj.meters.clean.df.mcr.24$period<-"h12 to 24 h"
plot(density(mov.traj.meters.clean.df.mcr.24$dist))

mov.traj.meters.clean.df.mcr.36<-subset(mov.traj.meters.clean.df, dt/3600<36)
mov.traj.meters.clean.df.mcr.36<-subset(mov.traj.meters.clean.df.mcr.36, dt/3600>24)
mov.traj.meters.clean.df.mcr.36<-subset(mov.traj.meters.clean.df.mcr.36, select=c(dt, dist))
mov.traj.meters.clean.df.mcr.36$period<-"i24 to 36 h"
plot(density(mov.traj.meters.clean.df.mcr.36$dist))

mov.traj.meters.clean.df.mcr.48<-subset(mov.traj.meters.clean.df, dt/3600<48)
mov.traj.meters.clean.df.mcr.48<-subset(mov.traj.meters.clean.df.mcr.48, dt/3600>24)
mov.traj.meters.clean.df.mcr.48<-subset(mov.traj.meters.clean.df.mcr.48, select=c(dt, dist))
mov.traj.meters.clean.df.mcr.48$period<-"j36 to 48 h"
plot(density(mov.traj.meters.clean.df.mcr.48$dist))

mov.traj.meters.clean.df.mcr.720<-subset(mov.traj.meters.clean.df, dt/3600<720)
mov.traj.meters.clean.df.mcr.720<-subset(mov.traj.meters.clean.df.mcr.720, dt/3600>48)
mov.traj.meters.clean.df.mcr.720<-subset(mov.traj.meters.clean.df.mcr.720, select=c(dt, dist))
mov.traj.meters.clean.df.mcr.720$period<-"k48 to 720 h"
plot(density(mov.traj.meters.clean.df.mcr.720$dist))

# time intervals < 2h
x<-c(mov.traj.meters.clean.df.mcr.01$dist,
     mov.traj.meters.clean.df.mcr.02$dist)
summary(x)
quantile(x, probs=c(0.05, 0.5, 0.95))
CI(x, ci = 0.95)

# time intervals < 8h
x<-c(mov.traj.meters.clean.df.mcr.01$dist,
     mov.traj.meters.clean.df.mcr.02$dist,
     mov.traj.meters.clean.df.mcr.03$dist,
     mov.traj.meters.clean.df.mcr.04$dist,
     mov.traj.meters.clean.df.mcr.06$dist,
     mov.traj.meters.clean.df.mcr.08$dist)
summary(x)
quantile(x,probs=c(0.05, 0.95))
CI(x, ci = 0.95)

# time intervals of 8-48h
y<-c(mov.traj.meters.clean.df.mcr.12$dist,
     mov.traj.meters.clean.df.mcr.24$dist,
     mov.traj.meters.clean.df.mcr.36$dist,
     mov.traj.meters.clean.df.mcr.48$dist)
summary(y)
quantile(y,probs=c(0.05, 0.95))
CI(y, ci = 0.95)

# time intervals > 48h
z<-c(mov.traj.meters.clean.df.mcr.720$dist)
summary(z)
quantile(z,probs=c(0.05, 0.95))
CI(z, ci = 0.95)

# merging all in a table and comparing the differences
tab.x<-data.frame(cbind(dist=x, period="a:<8h"))
tab.y<-data.frame(cbind(dist=y, period="b:8-48h"))
tab.z<-data.frame(cbind(dist=z, period="c:>48h"))

tab.xyz<-data.frame(rbind(tab.x, tab.y, tab.z))
tab.xyz$dist<-as.numeric(as.character(tab.xyz$dist))
mod.lm <- lm(dist ~ period, data = tab.xyz)
summary(mod.lm)
# aov(mod.lm)
anova(mod.lm)

# doing the same for each category
mov.traj.meters.clean.df.3 <- mov.traj.meters.clean.df %>% 
  dplyr::select(dist, dt) %>% 
  dplyr::mutate(
    dt = dt/3600,
    period = case_when(
      dt <= 1 ~ "a:<=1h",
      dt <= 2 ~ "b:1-2h",
      dt <= 3 ~ "c:2-3h",
      dt <= 4 ~ "d:3-4h",
      dt <= 6 ~ "e:4-6h",
      dt <= 8 ~ "f:6-8h",
      dt <= 12 ~ "g:8-12h",
      dt <= 24 ~ "h:12-24h",
      dt <= 36 ~ "i:24-36h",
      dt <= 48 ~ "j:36-48h",
      TRUE ~ "k:48-720h"))

mod.lm2 <- lm(dist ~ period, data = mov.traj.meters.clean.df.3)
summary(mod.lm2)
anova(mod.lm2)
aov(dist ~ period, data = mov.traj.meters.clean.df.3)
(a1 <- aov(mod.lm2))
(posthoc <- TukeyHSD(a1))

mov.traj.meters.clean.df.mcr.periods<-data.frame(rbind(mov.traj.meters.clean.df.mcr.01, 
                                                       mov.traj.meters.clean.df.mcr.02,
                                                       mov.traj.meters.clean.df.mcr.03,
                                                       mov.traj.meters.clean.df.mcr.04,
                                                       mov.traj.meters.clean.df.mcr.06,
                                                       mov.traj.meters.clean.df.mcr.08,
                                                       mov.traj.meters.clean.df.mcr.12,
                                                       mov.traj.meters.clean.df.mcr.24,
                                                       mov.traj.meters.clean.df.mcr.36,
                                                       mov.traj.meters.clean.df.mcr.48,
                                                       mov.traj.meters.clean.df.mcr.720))

leg.levels<-unique(as.character(mov.traj.meters.clean.df.mcr.periods$period))
leg.levels<-substring(leg.levels, 2, nchar(leg.levels))

mov.traj.meters.clean.df.mcr.periods$periodlevel<-factor(mov.traj.meters.clean.df.mcr.periods$period, labels=leg.levels)

colors = c("grey50", "red", "blue", "green", "purple", 
           "cyan", "orange", "limegreen", "pink", "maroon",
           "navyblue")


png("dist_vs_time_lag_30days_mcr_periods.png", 1200,700)
p=ggplot(subset(mov.traj.meters.clean.df.mcr.periods, dist>0.2),aes(x=dist, fill=periodlevel)) 
p=p+ geom_density(alpha=0.25)
p=p+  theme_bw()

p=p+ scale_x_log10(breaks = c(5,10,25,50,100,250,500,750,1000,2500,5000,10000,20000,40000)) 
p=p+ theme(axis.text.x = element_text(size=14))
p=p+ theme(axis.text.y = element_text(size=14))
p=p+ theme(axis.title.x = element_text(size=18))
p=p+ theme(axis.title.y = element_text(size=28))
p=p+ xlab("Distance in meter (log scale)")
p=p+ ylab("Density of records")
p=p+ scale_fill_manual("Periods", values=colors)
p
dev.off()


write.table(mov.traj.meters.clean.df.mcr.periods,
            "..\\figures_movements_different_fixes\\__dados.txt",
            col.names=T, row.names=F, append=F, quote=F, sep="\t")




# Merge planned schedule into ltraj movement data
colnames(ind.info)[1] <- 'id'
colnames(ind.info)
mov.traj.meters.clean.df.plan.sch <- merge(mov.traj.meters.clean.df, ind.info, by = 'id')
head(mov.traj.meters.clean.df.plan.sch)

# Plot planned vs observed fix rate
plot(mov.traj.meters.clean.df.plan.sch$planned.schedule.numeric, mov.traj.meters.clean.df.plan.sch$dt/3600,
     xlab = 'Planned schedule (h)', ylab = 'Observed time lag (h)')
abline(a = 0, b = 1, lwd = 3, col = 2)

plot(mov.traj.meters.clean.df.plan.sch$planned.schedule.numeric, mov.traj.meters.clean.df.plan.sch$dt/3600,
     ylim = c(0, 12), xlab = 'Planned schedule (h)', ylab = 'Observed time lag (h)')
abline(a = 0, b = 1, lwd = 3, col = 2)

boxplot(mov.traj.meters.clean.df.plan.sch$dt/3600 ~ as.factor(mov.traj.meters.clean.df.plan.sch$planned.schedule.numeric),
        ylim = c(0, 24), xlab = 'Planned schedule (h)', ylab = 'Observed time lag (h)')
abline(a = 0, b = 1, lwd = 3, col = 2)

tab <- table(mov.traj.meters.clean.df.plan.sch$planned.schedule.numeric)
axis(3, at = 1:length(tab), labels = paste('n =', tab), las = 2, 
     cex.axis = 0.8)

# divided into categories
planned_hr <- as.factor(mov.traj.meters.clean.df.plan.sch$planned.schedule.numeric)
table(planned_hr)
observed_hr <- mov.traj.meters.clean.df.plan.sch$dt/3600
hr_classes <- c(0, 0.55, 1.2, 1.7, 2.2, 3.2, 4.2, 4.9, 5.2, 6.2, 8.2, 16.2, 24.2, 36.5, 48.5, 72.5, 96.5, 120, 720)
hr_classes.print <- c(0, 0.5, 1, 1.5, 2, 3, 4, 4.8, 5, 6, 8, 16, 24, 36, 48, 72, 96, 120, 720)
observed_hr_cut <- cut(observed_hr, breaks = hr_classes)
table(observed_hr_cut)
observed_hr_cut.print <- cut(observed_hr, breaks = hr_classes.print)

planned_observed_hr <- table(planned_hr, observed_hr_cut)
plot(planned_observed_hr)

planned_observed_hr.pct <- planned_observed_hr/rowSums(planned_observed_hr)*100
planned_observed_hr.pct

plot(planned_observed_hr.pct)

image(planned_observed_hr.pct, col = heat.colors(10))

require(lattice)
planned_observed_hr.pct[planned_observed_hr.pct < 0.1]<-NA
levelplot(planned_observed_hr.pct, col.regions = heat.colors(100)[length(heat.colors(100)):1])
levelplot(planned_observed_hr.pct, col.regions = topo.colors(200)[length(topo.colors(200)):1])
levelplot(planned_observed_hr.pct, col.regions = topo.colors(100)[length(topo.colors(100)):1])

colnames(planned_observed_hr.pct) <- levels(observed_hr_cut.print)

# tiff(paste0(outdir, "planned_observed_fixrate.tif"), height = 15, width = 13, 
#     units = "cm", res = 600)
png(paste0(outdir, "planned_observed_fixrate.png"), height = 15, width = 13, 
    units = "cm", res = 600)
par(mar = c(2,2,2,2) + 0.1)
col.l <- colorRampPalette(c('yellow', "orange", 'red'))(30)
levelplot(planned_observed_hr.pct, col.regions = col.l, 
          xlab = "Planned fix rate (h)", ylab = "Observed fix rate (h)")
dev.off()

mov.traj.meters.clean.df %>% 
  dplyr::filter(Planned.schedule <= 1) %>% 
  dplyr::summarize(n_inds = length(unique(id)),
                   prop_inds = n_inds/117,
                   n_pts = nrow(.),
                   prop.pts = n_pts/nrow(mov.traj.meters.clean.df)) 
  
