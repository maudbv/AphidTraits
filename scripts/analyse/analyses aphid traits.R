# Run statistical analyses for Aphid traits

## 2.1 Linear models to partition variance among colonies/plots:

#Example for one trait : femur length left

# LINEAR MODEL:
f <- lm(sqrt(femur_length_left) ~ 
          collector +    # us or Judith
          ID_plot +      # plot effect
          Colony%in%ID_plot,  # colony within plot
        data = aphid_traits) 
summary(f)
a = anova(f)
# add the partial R2 for each predictor:
a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
r2(f)

# look at table of analysis of variance:
print(a) 
# 8% explained by plot, 30% by colonies wihtin plots,
# and 7% by year of collection(ie the "collector" effect")

plot(DHARMa::simulateResiduals(f)) # looks pretty good, significance is likely only due to the large number of data points



## 2.1 Linear models testing for a relationship between aphid traits and % Sealing
