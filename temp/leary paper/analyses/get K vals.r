####
rm(list=ls())

library(here)

prelim.data = read.csv(here("temp/leary paper/data/data.csv"))


prelim.data = transform(prelim.data, per.ml=count/weight.1*weight.2/weight.3*weight.4/weight.5)
prelim.data = transform(prelim.data, log.per.ml=log10(per.ml+1))
prelim.data = transform(prelim.data, ln.per.ml=log(per.ml))
prelim.data = subset(prelim.data, temperature<28)


biomass = read.csv("volume.csv")


mean.biomass <- tapply(biomass$volume.mg.,list(biomass$species),mean)

##preparing and adding biomass info 

mean.biomass <- data.frame(mean.biomass)
species <- cbind(unique(biomass$species))
species <- data.frame(species)
biomass <- data.frame(mean.biomass, species)

##adding biomass to pop data

prelim.data <- data.frame(prelim.data,
                          biomass=biomass[match(prelim.data[,4], biomass[,2]),1])

##calculating biomass of each pop count

prelim.data <- transform(prelim.data, pop.biomass=biomass*per.ml)

prelim.data <- transform(prelim.data, log.biomass=log10(pop.biomass))

####remove microcosm 81, day 42

edit = prelim.data$microcosm==81 & prelim.data$day==42

prelim.data = prelim.data[!edit,]



get.K <- function(X, num=1)
    mean(sort(X)[length(X):(length(X)-num+1)])

K = aggregate(prelim.data$per.ml,
    list(species=prelim.data$species,
         jar=prelim.data$microcosm,
         temperature=prelim.data$temperature,
         replicate=prelim.data$replicate),
    function(X) get.K(X))

readr::write_csv(K, here("data/Leary_and_Petchey_K_values.csv"))

