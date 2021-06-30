# Run statistical analyses for Aphid traits

# Setting up contrast options for the coefficients of the linear models:
# In the end, there is no perfect solution, 
# so I used the simple default mode for unordered factors, ie "contr.treatment".

options(contrasts = c( "contr.treatment","contr.sum"))

# This means the intercept of a model corresponds to the 1st level of each factor
# Example: 
# for the first model (2.1), the "intercept" corresponds to the estimated mean trait value for the individuals which are:
# - collected by Judith (1st level factor of factor "collector")
# - in plot Nh_10 (1st level of factor "ID_plot")
# - in colony Nh_10-1 (1st level of factor "Colony" in Nh_10)
# all other coefficients are calculated relative to this first combination of factors.
# In any case, there is no need for you to worry much about these coefficients, 
# and you don't need to report them. The only interesting one might be the 
# collector effect (or year effect!), which shows that our collected individuals tended on average to be smaller than Judith's.


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

# LINEAR MODEL: = it is a nested ANOVA, as one factor (colony) is nested within another (ID_plot)
f <- lm(femur_length ~ 
          collector +    # us or Judith
          ID_plot/Colony , # colony within plot,
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
