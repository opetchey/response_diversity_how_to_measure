####
rm(list=ls())

library(here)

prelim.data = read.csv(here("temp/leary paper/data/data.csv"))

prelim.data = transform(prelim.data, per.ml=count/weight.1*weight.2/weight.3*weight.4/weight.5)
prelim.data = transform(prelim.data, log.per.ml=log10(per.ml))
prelim.data = transform(prelim.data, ln.per.ml=log(per.ml))
prelim.data = subset(prelim.data, temperature<28)



biomass = read.csv(here("temp/leary paper/data/volume.csv"))



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



r.days <- read.csv(here("temp/leary paper/data/r days.csv"))

spp <- sort(unique(prelim.data$species))
sppppp <- c("Colpidium", "Loxocephallus", "Paramecium", "Tetrahymena")

temps <- sort(unique(prelim.data$temperature))

all.jars <- unique(prelim.data$microcosm)

tp <- prelim.data$per.ml[prelim.data$per.ml!=-Inf]

ylims <- range(log10(tp+1))



counter=1
##par(ask=T)
for(i in 1:length(all.jars)) {
    these <- prelim.data$microcosm==all.jars[i]
    
    y <- log(prelim.data$pop.biomass[these])
    x <- prelim.data$day[these]
    
    o <- order(x)
    x <- x[o]
    y <- y[o]
    
    x <- x[y!=-Inf]
    y <- y[y!=-Inf]
    
    x <- x[1:r.days[r.days[,1]==all.jars[i] ,2]]
    y <- y[1:r.days[r.days[,1]==all.jars[i] ,2]]
    
    
    ##plot(x,y, main=all.jars[i])
    
    x <- coef(lm(y~x))[2]
    temp <- unique(prelim.data$temperature[these])
    species <- as.character(unique(prelim.data$species[these]))
    rep <- unique(prelim.data$replicate[these])
    jar <- all.jars[i]
    
    if(counter==1)
        rs <- data.frame(species=species,
                         jar=jar,
                         temperature=temp,
                         replicate=rep,
                         x=x)
    if(counter>1)
        rs <- rbind(rs, data.frame(species=species,
                                   jar=jar,
                                   temperature=temp,
                                   replicate=rep,
                                   x=x))
    counter=counter+1
}

rs[is.na(rs$x),"x"] = 0

K <- rs
readr::write_csv(rs, here("data/Leary_and_Petchey_r_values.csv"))
