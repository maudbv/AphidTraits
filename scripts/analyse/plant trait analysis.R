# plant and colony traits

plant_traits$Plot <- stringr::str_replace(pattern = "SÃ¼dkreuz",replacement =  "Sk_01",string = plant_traits$Plot)

colnames(plant_traits) = make.names(colnames(plant_traits))

# add column with urban rural category
plant_traits$urban_rural = plot_data[match(plant_traits$Plot, plot_data$ID_plot),
                                          "urban_rural"]

# explore differences between urban and rural

par(mar = c(2,4,1,1))

# significantly bigger aphid colonies in urban areas.
boxplot(SLA.mean ~ urban_rural, data = plant_traits,
         ylab = "Specific leaf area",
         xlab = "")
summary(f <- lm(SLA.mean ~ urban_rural,
                 data = plant_traits))
performance::r2(f) # this is not working for some reason, R2 = 1 is not possible
r2beta(f,  method = 'lm') # this one works now, different package

# Leaf dry matter content
boxplot( LDMC.mean ~ urban_rural, data = plant_traits,
         ylab = "Number of ants",
         xlab = "")
summary(lm(LDMC.mean ~ urban_rural, data = plant_traits))

#Stem DMC
boxplot( SDMC.mean ~ urban_rural, data = plant_traits,
         ylab = "SDMC(mg.g-1)",
         xlab = "")
