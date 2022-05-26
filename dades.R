death_cause<-read.csv("Death Cause Reason by Country.csv")
country_var<-read.csv("country_profile_variables.csv")

library(dplyr)
library(tidyverse)
library(rworldmap)
library(ggplot2)

# Quines són les 10 malalties amb més mortalitat arreu del món?
mortality<-death_cause[,2:33]
mortality[is.na(mortality)]<-0
mean_mortality<-as.data.frame(colMeans(mortality))
top_mortality<-head(arrange(mean_mortality,desc(mean_mortality$`colMeans(mortality)`)),10)

#Densitat de cada pais en un mapa
world_density<-country_var%>%select(country,Population.in.thousands..2017.)%>%arrange(desc(Population.in.thousands..2017.))
mapDevice('x11')
spdf<-joinCountryData2Map(world_density, joinCode = "NAME",
                          nameJoinColumn = "country")
mapCountryData(spdf, nameColumnToPlot = "Population.in.thousands..2017.",
               catMethod = "fixedWidth", mapTitle = "World Population in thousands 2017")

#Economia per sectors
world_money<-country_var[,c(1,10:12)]
world_money[is.na(world_money)]<-0

agricultura<-world_money%>%select(country,Economy..Agriculture....of.GVA.)
agricultura$Economy..Agriculture....of.GVA.<-as.numeric(as.character(agricultura$Economy..Agriculture....of.GVA.))
agricultura<-agricultura%>%arrange(desc(Economy..Agriculture....of.GVA.))
mapDevice('x11')
spdf<-joinCountryData2Map(agricultura, joinCode = "NAME",
                          nameJoinColumn = "country")
mapCountryData(spdf, nameColumnToPlot = "Economy..Agriculture....of.GVA.",
               catMethod = "fixedWidth", mapTitle = "World Economy in Agriculture")

industria<-world_money%>%select(country,Economy..Industry....of.GVA.)
industria$Economy..Industry....of.GVA.<-as.numeric(as.character(industria$Economy..Industry....of.GVA.))
industria<-industria%>%arrange(desc(Economy..Industry....of.GVA.))
mapDevice('x11')
spdf<-joinCountryData2Map(industria, joinCode = "NAME",
                          nameJoinColumn = "country")
mapCountryData(spdf, nameColumnToPlot = "Economy..Industry....of.GVA.",
               catMethod = "fixedWidth", mapTitle = "World Economy in Industry")

serveis<-world_money%>%select(country,Economy..Services.and.other.activity....of.GVA.)
serveis$Economy..Services.and.other.activity....of.GVA.<-as.numeric(as.character(serveis$Economy..Services.and.other.activity....of.GVA.))
serveis<-serveis%>%arrange(desc(Economy..Services.and.other.activity....of.GVA.))
mapDevice('x11')
spdf<-joinCountryData2Map(serveis, joinCode = "NAME",
                          nameJoinColumn = "country")
mapCountryData(spdf, nameColumnToPlot = "Economy..Services.and.other.activity....of.GVA.",
               catMethod = "fixedWidth", mapTitle = "World Economy in Services")

#Top 10 països afectas per la Covid-19
covid_top_country<-head(death_cause%>%select(Country.Name,Covid.19.Deaths)%>%arrange(desc(Covid.19.Deaths)),10)

#Contaminació
pollution<-country_var%>%select(country,CO2.emission.estimates..million.tons.tons.per.capita.)%>%arrange(desc(CO2.emission.estimates..million.tons.tons.per.capita.))
mapDevice('x11')
spdf<-joinCountryData2Map(pollution, joinCode = "NAME",
                          nameJoinColumn = "country")
mapCountryData(spdf, nameColumnToPlot = "CO2.emission.estimates..million.tons.tons.per.capita.",
               catMethod = "fixedWidth", mapTitle = "World Pollution Levels")

#Malaltia respiratoria
malaltia_respiratoria<-death_cause%>%select(Country.Name,Respiratory.diseases)%>%arrange(desc(Respiratory.diseases))
mapDevice('x11')
spdf<-joinCountryData2Map(malaltia_respiratoria, joinCode = "NAME",
                          nameJoinColumn = "Country.Name")
mapCountryData(spdf, nameColumnToPlot = "Respiratory.diseases",
               catMethod = "fixedWidth", mapTitle = "World Mortality caused by Respiratory diseases")
