install.packages("remotes")
library(remotes)
install_github("bczernecki/climate")
install.packages('archive')
install.packages(c('s2', 'units'))
install.packages("sf", "ggplot2")

# station swapped from Jelenia Góra to Fasty due to error while downloading data
stations = c("BOŻEPOLE SZLACHECKIE", "FASTY" , "LGOTA NADWARCIE")
setwd("/home/Fedora/rprojects/")


# ------- Map

library(sf)
library(ggplot2)

stations = c("BOŻEPOLE SZLACHECKIE", "FASTY", "LGOTA NADWARCIE")

#lat, lon
bp = c(53.97269, 18.22538)
f  = c(53.17387, 23.02267)
ln = c(50.61779, 19.25572)

stacje_df = data.frame(
  stacja = stations,
  lat    = c(bp[1], f[1], ln[1]),
  lon    = c(bp[2], f[2], ln[2])
)
stacje_sf = st_as_sf(stacje_df, coords = c("lon", "lat"), crs = 4326)

Polska = read_sf("polska_84.shp")
stacje_sf = st_transform(stacje_sf, st_crs(Polska))

mapa = ggplot() +
  geom_sf(data = Polska, size = 0.1, color = "black", fill = "NA") +
  theme_minimal() +
  ggtitle("Stacje hydrologiczne") +
  coord_sf() +
  geom_sf(data = stacje_sf, size = 3, color = "red") +
  geom_sf_text(data = stacje_sf, aes(label = stacja), hjust = 0, size = 3, nudge_y = 0.2)

plot(mapa)
ggsave("mapka.png", plot=mapa, width = 8, height=6, dpi=1000)


# ------- Initial Analysis

library(climate)

# create a set with data. Excludes year 2023 due to some data errors
options(encoding = "CP1250")
data_pre23 = lapply(stations, function(s) {
  hydro_imgw("daily", year=2015:2022, station=s, allow_failure=FALSE)$COPRZP
})
data_post23 = lapply(stations, function(s) {
  hydro_imgw("daily", year=2024:2025, station=s, allow_failure=FALSE)$COPRZP
})
options(encoding = "native.enc")

y23 = read.csv("codz_2023.csv")

data = list(
  "BOŻEPOLE SZLACHECKIE" = c(data_pre23[[1]], y23[y23$nazwa == "BOŻEPOLE SZLACHECKIE", ]$Q, data_post23[[1]]),
  "FASTY"                = c(data_pre23[[2]], y23[y23$nazwa == "FASTY", ]$Q,                data_post23[[2]]),
  "LGOTA NADWARCIE"      = c(data_pre23[[3]], y23[y23$nazwa == "LGOTA NADWARCIE", ]$Q,      data_post23[[3]])
)

max_y = max(data[[1]], data[[2]], data[[3]])
dates = seq(as.Date("2015-01-01"), as.Date("2024-12-31"), by="day")

plot(dates, data[[1]], type="l", ylim=c(0,10), xaxt="n", col="blue")
axis(1, at=seq(as.Date("2015-01-01"), as.Date("2025-01-01"), by="year"), labels=2015:2025)
abline(v=seq(as.Date("2015-01-01"), as.Date("2025-01-01"), by="year"), col="black", lty=3)
abline(h=pretty(data[[1]]), col="black", lty=3)

plot(dates, data[[2]], type="l", ylim=c(0,max_y), xaxt="n", col='darkgreen')
axis(1, at=seq(as.Date("2015-01-01"), as.Date("2025-01-01"), by="year"), labels=2015:2025)
abline(v=seq(as.Date("2015-01-01"), as.Date("2025-01-01"), by="year"), col="black", lty=3)
abline(h=pretty(data[[2]]), col="black", lty=3)

plot(dates, data[[3]], type="l", ylim=c(0,20), xaxt="n", col="gold3")
axis(1, at=seq(as.Date("2015-01-01"), as.Date("2025-01-01"), by="year"), labels=2015:2025)
abline(v=seq(as.Date("2015-01-01"), as.Date("2025-01-01"), by="year"), col="black", lty=3)
abline(h=pretty(data[[3]]), col="black", lty=3)


plot(data[[1]], type="l", ylim=c(0,max_y), xlim=c(0, 4000), col="blue")
lines(data[[2]], col="darkgreen")
lines(data[[3]], col="gold3")
legend("topright", legend=stations, col=c("blue","darkgreen","gold3"), lty=1)

boxplot(data[[1]], data[[2]], data[[3]], names=stations, ylim=c(0, 20), outline=F)
boxplot(data[[1]], data[[3]], names=c(stations[1], stations[3]), ylim=c(0, 5), outline=F)

hist(data[[1]], xlim=c(0,10), ylim=c(0, 1000), las=1, xlab="Przepływ [m3/s]", ylab="Częstość", col="blue")
hist(data[[2]], xlim=c(0,40), ylim=c(0, 800), las=1, xlab="Przepływ [m3/s]", ylab="Częstość", col="darkgreen")
hist(data[[3]], xlim=c(0,20), ylim=c(0, 1400), las=1, xlab="Przepływ [m3/s]", ylab="Częstość", col="gold3")





