# Run statistical analyses for Aphid traits

options(contrasts = c("contr.sum", "contr.treatment"))

# Create a data frame with aphid traits and plot data 
df <- data.frame(
  aphid_traits,
  Seal_500 = plot_data[aphid_traits$ID_plot, "Seal_500"],
  urban_rural = plot_data[aphid_traits$ID_plot, "urban_rural"]
)


## 2.1 Linear models to partition variance among colonies/plots: ####

#Example for one trait : femur length

tmp <-df$femur_length


hist(tmp, breaks = 20)
qqnorm(tmp)
shapiro.test(tmp) # normal yay!!


# LINEAR MODEL:
f <- lm(femur_length ~ 
          collector +    # us or Judith
          ID_plot +      # plot effect
          Colony%in%ID_plot , # colony within plot,
        data = df) 


summary(f)
a = anova(f)
# add the partial R2 for each predictor:
a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)

# look at table of analysis of variance:
print(a) 
# 18% explained by plot, 39% by colonies wihtin plots,
# and 15% by year of collection(ie the "collector" effect")

hist(f$residuals, breaks = 40)


#Example for one trait : femur length

tmp <- df$abdomen_length

hist(tmp, breaks = 20)
qqnorm(tmp)
shapiro.test(tmp) # normal yay!!


# LINEAR MODEL:
f <- lm(abdomen_length ~ 
          collector +    # us or Judith
          ID_plot +      # plot effect
          Colony%in%ID_plot + # colony within plot
          urban_rural,
        data = aphid_traits) 


summary(f)
a = anova(f)
# add the partial R2 for each predictor:
a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)

## 2.2 Linear models testing for difference between urban and rural for aphid traits  ####

# Mixed models using lmer package lme4
library(lme4)
library(lmerTest)
f <- lmer(femur_length ~ collector + urban_rural +
          (1|ID_plot/Colony),
        data = df) 

summary(f)
anova(f) # =>similar to summary of fixed effects
r2(f) # conditional = random + fixed effects; marginal  = only fixed effects
r2beta(f, method = 'nsj' ) # Nakagawa and Schielzeth approach.  


## 2.3 Linear models testing for a relationship between aphid traits and % Sealing ####

# Mixed models using lmer package lme4
library(lme4)
library(lmerTest)
f <- lmer(femur_length ~ collector + Seal_500 +
            (1|ID_plot/Colony),
          data = df) 

summary(f)
anova(f) # =>similar to summary of fixed effects
r2(f) # conditional = random + fixed effects; marginal  = only fixed effects
r2beta(f, method = 'nsj' ) # Nakagawa and Schielzeth approach.  

#illustrate
plot(femur_length ~ Seal_500, df)
abline(lm(femur_length ~ Seal_500, df))
