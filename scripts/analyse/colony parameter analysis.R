# plant and colony traits

colony_parameters$Plot <- stringr::str_replace(pattern = "SÃ¼dkreuz",replacement =  "Sk_01",string = colony_parameters$Plot)

colnames(colony_parameters) = make.names(colnames(colony_parameters))

# add column with urban rural category
colony_parameters$urban_rural = plot_data[match(colony_parameters$Plot, plot_data$ID_plot),
                                          "urban_rural"]

# explore differences between urban and rural

par(mar = c(2,4,1,1))

# significantly bigger aphid colonies in urban areas.
boxplot( X.aphids.in.colony ~ urban_rural, data = colony_parameters,
         ylab = "Number of aphids per colony",
         xlab = "")
summary(f <- glm(X.aphids.in.colony ~ urban_rural,
                 data = colony_parameters,
                 family = poisson))
performance::r2(f) # this is not working for some reason, R2 = 1 is not possible
r2beta(f,  method = 'lm') # this one works now, different package

# average and sd

tapply(X = colony_parameters$X.aphids.in.colony,
       INDEX = colony_parameters$urban_rural,
       FUN = mean,
       na.rm = TRUE)

tapply(X = colony_parameters$X.aphids.in.colony,
       INDEX = colony_parameters$urban_rural,
       FUN = sd,
       na.rm = TRUE)




# no difference in number of colonies per plant
boxplot( X.colonies.plant ~ urban_rural, data = colony_parameters,
         ylab = "Number of colonies per plant",
         xlab = "")
summary(glm(X.colonies.plant ~ urban_rural, data = colony_parameters, family = poisson))

# average and sd

?mean
mean(colony_parameters$X.colonies.plant,
     na.rm = TRUE)

sd(colony_parameters$X.colonies.plant,
   na.rm = TRUE)

tapply(X = colony_parameters$X.colonies.plant,
       INDEX = colony_parameters$urban_rural,
       FUN = mean,
       na.rm = TRUE)

tapply(X = colony_parameters$X.colonies.plant,
       FUN = mean,
       na.rm = TRUE)

tapply(X = colony_parameters$X.colonies.plant,
       INDEX = colony_parameters$urban_rural,
       FUN = sd,
       na.rm = TRUE)


# marginally more ants in urban areas too:
boxplot( X.ants ~ urban_rural, data = colony_parameters,
         ylab = "Number of ants",
         xlab = "")
summary(glm(X.ants ~ urban_rural, data = colony_parameters, family = poisson))

summary(f <- glm(X.ants ~ urban_rural, data = colony_parameters, family = poisson))

performance::r2(f)  # can i do this ?


colony_parameters$parasitism_binary = as.numeric(colony_parameters$perc.parasitized>0)
table(colony_parameters$parasitism_binary, colony_parameters$urban_rural)
table(colony_parameters$urban_rural)

summary(f <- glm(parasitism_binary ~ urban_rural,
                 data = colony_parameters,
                 family = binomial))
#urban colonies tend to be more often parasitized, but its not significant

boxplot( perc.parasitized ~ urban_rural, data = colony_parameters,
         ylab = "percentage parasitized",
         xlab = "")
summary(f <- lm(perc.parasitized ~ urban_rural, data = colony_parameters))

#no difference in parasitism

# significantly taller plants
boxplot( max.repro.hight ~ urban_rural, data = colony_parameters,
         ylab = "maximal reproductive hight",
         xlab = "")
summary(lm(max.repro.hight ~ urban_rural, data = colony_parameters))

boxplot( max.vege.hight ~ urban_rural, data = colony_parameters,
         ylab = "maximal vegetative hight",
         xlab = "")
summary(lm(max.vege.hight ~ urban_rural, data = colony_parameters))


# size of the plant and number of flowering stems :
boxplot( X.stems ~ urban_rural, data = colony_parameters,
         ylab = "number of stems",
         xlab = "")
summary(lm(X.stems ~ urban_rural, data = colony_parameters))

boxplot( X.capitula ~ urban_rural, data = colony_parameters,
         ylab = "number of capitula",
         xlab = "")
summary(glm( X.capitula ~ urban_rural, data = colony_parameters, family = poisson))

# score the phenology of plant
colony_parameters$budding[19] <- NA
colony_parameters$budding[which(colony_parameters$budding == "")]  <- NA

colony_parameters$pheno = 
  rowSums(apply(colony_parameters[,c("budding","flowering","vilted")], 2,as.numeric) *c(0.2, 0.5, 1),
          na.rm =TRUE)/colony_parameters$X.stems

colony_parameters$pheno[is.na(colony_parameters$pheno)] = 0

boxplot( pheno ~ urban_rural, data = colony_parameters)
summary(lm(pheno ~ urban_rural, data = colony_parameters))

boxplot( budding ~ urban_rural, data = colony_parameters)


