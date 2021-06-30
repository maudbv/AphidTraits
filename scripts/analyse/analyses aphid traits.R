# Run statistical analyses for Aphid traits

## 2.1 Linear models to partition variance among colonies/plots: ####
options(contrasts = rep("contr.sum",2))
#Example for one trait : femur length

tmp <- aphid_traits$femur_length
tmp <- tmp[ tmp < 1 ]

hist(tmp, breaks = 20)
qqnorm(tmp)
shapiro.test(tmp) # normal yay!!


# LINEAR MODEL:
f <- lm(femur_length ~ 
          collector +    # us or Judith
          ID_plot +      # plot effect
          Colony%in%ID_plot, # colony within plot,
        data = aphid_traits) 



summary(f)
a = anova(f)
# add the partial R2 for each predictor:
a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)

# look at table of analysis of variance:
print(a) 
# 8% explained by plot, 30% by colonies wihtin plots,
# and 7% by year of collection(ie the "collector" effect")

hist(f$residuals, breaks = 40)

## 2.2 Linear models testing for difference between urban and rural for aphid traits  ####

## 2.3 Linear models testing for a relationship between aphid traits and % Sealing ####
