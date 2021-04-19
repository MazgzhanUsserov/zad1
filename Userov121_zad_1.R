#Усеров Магжан ПАЭ 121
#Регион 36 (Воронежская обл.)
#рассчитайте урожайность пшеницы в 2010 году, 
#взяв для рассчета средние суммы активных температур за предыдущие 3 года, 
#с 15 ближайших метеостанций.

#Активация библиотек и ввод исходных данных
library (tidyverse);library (rnoaa);library(lubridate)
afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
di=c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
y1=1.0;Kf=300;Qj=1600;Lj=2.2;Ej=25 
#Скачивание данных и создание из них датафрейма с инфой по 15 станциям 
station_data = read.csv("station_data20.csv")
voronezh = data.frame(id = "Voronezh", latitude = 51.661535,  longitude = 39.200287)
vor_around = meteo_nearby_stations(lat_lon_df = voronezh, station_data = station_data, limit = 15, var = c("TAVG"), year_min = 2007, year_max = 2009)
vor_id = vor_around[["Voronezh"]][["id"]][1]
vor_table = data.frame (vor_around)
all_pre_vor = data.frame()
all_vor_meteodata = data.frame()
for(i in 1:15)
{
  all_pre_vor=meteo_tidy_ghcnd(stationid =vor_id, var="TAVG", date_min = "2007-01-01", date_max = "2009-12-31") 
  all_vor_meteodata=rbind(all_vor_meteodata, all_pre_vor)
}
write.csv (all_vor_meteodata,"all_vor_meteodata.csv")

#Создание датафрейма с данными за нужный период времени 
all_vor_meteodata=read.csv("all_vor_meteodata.csv") 
all_vor_meteodata [,"year"]= year(all_vor_meteodata$date)
all_vor_meteodata [,"month"]= month(all_vor_meteodata$date) 
all_vor_meteodata [,"day_of_the_year"]= yday(all_vor_meteodata$date) 
y_vor_met= filter(all_vor_meteodata,year>2006 & year<2010)

#подсчет средних активных температур по месяцам за нужный период времени
y_vor_met[,"tavg"]= y_vor_met$tavg/10
y_vor_met [is.na(y_vor_met$tavg), "tavg"] = 0
y_vor_met [y_vor_met$tavg<5, "tavg"] = 0
alldays=group_by(y_vor_met,id,year,month)
sum_t=summarize(alldays, tsum = sum(tavg))
group_months=group_by(sum_t,month)
sum_t_months=summarize(group_months , St = mean(tsum))

#Рассчет урожая по формуле
sum_t_months = mutate(sum_t_months, Fi = afi+bfi*y1*St)
sum_t_months = mutate(sum_t_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))
Yield = sum(sum_t_months$Yi)
Yield

