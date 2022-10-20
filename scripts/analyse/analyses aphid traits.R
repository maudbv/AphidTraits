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
  prop_neo = plot_data[aphid_traits$ID_plot, "prop.neo"],
  Night_temp = plot_data[aphid_traits$ID_plot, "mean_tempNight_summer"],
  urban_rural = plot_data[aphid_traits$ID_plot, "urban_rural"]
)

# remove Judith's samples for now
df <- filter(df, collector == 'Maud + Elena')


## 2.1 Linear models to partition variance among colonies/plots: ####

#Example for one trait : femur length
    tmp <-df$femur_length
    
    hist(tmp, breaks = 20)
    qqnorm(tmp)
    shapiro.test(tmp) # normal yay!!
    
    # LINEAR MODEL: = it is a nested ANOVA, as one factor (colony) is nested within another (ID_plot)
    f <- lm(femur_length ~
              ID_plot/Colony , # colony within plot,
            data = df) 
    
    #check residuals
    hist(residuals(f), breaks = 20)
    qqnorm(residuals(f))
    shapiro.test(residuals(f)) # normal yay!!
    
    
    
    summary(f)
    a = anova(f)
    # add the partial R2 for each predictor:
    a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
    
    # look at table of analysis of variance:
    print(a) 
    # 18% explained by plot, 39% by colonies wihtin plots,
    # and 15% by year of collection(ie the "collector" effect")
    
    hist(f$residuals, breaks = 40)

#Example for 2 trait : tibia length
    tmp <-df$tibia_length
    
    hist(tmp, breaks = 20)
    qqnorm(tmp)
    shapiro.test(tmp) 
    
    
    f <- lm(tibia_length ~ 
              ID_plot/Colony , 
            data = df) 
    
    summary(f)
    a = anova(f)
    
    a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
    
    
    print(a) 
    
    
    hist(f$residuals, breaks = 40)

#Example for 3 trait : abdomen length
    tmp <-df$abdomen_length
    
    hist(tmp, breaks = 20)
    qqnorm(tmp)
    shapiro.test(tmp) 
    
    
    f <- lm(abdomen_length ~ 
              ID_plot/Colony , 
            data = df) 
    
    summary(f)
    a = anova(f)
    
    a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
    
    
    print(a) 
    
    
    hist(f$residuals, breaks = 40)
    
#Example for 4 trait : body length
    tmp <-df$body_length
    
    hist(tmp, breaks = 20)
    qqnorm(tmp)
    shapiro.test(tmp) 
    
    
    f <- lm(body_length ~ 
              ID_plot/Colony , 
            data = df) 
    
    summary(f)
    a = anova(f)
    
    a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
    
    
    print(a) 
    
    
    hist(f$residuals, breaks = 40)
    
#Example for 5 trait : body width
    tmp <-df$body_width
    
    hist(tmp, breaks = 20)
    qqnorm(tmp)
    shapiro.test(tmp) 
    
    
    f <- lm(body_width ~ 
              ID_plot/Colony , 
            data = df) 
    
    summary(f)
    a = anova(f)
    
    a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
    
    
    print(a) 
    
    
    hist(f$residuals, breaks = 40)
    
#Example for 6 trait : tarsus length
    tmp <-df$tarsus_length
    
    hist(tmp, breaks = 20)
    qqnorm(tmp)
    shapiro.test(tmp) 
    
    
    f <- lm(tarsus_length ~ 
              ID_plot/Colony , 
            data = df) 
    #check residuals
    hist(residuals(f), breaks = 20)
    qqnorm(residuals(f))
    shapiro.test(residuals(f)) # normal yay!!
  
    
    summary(f)
    a = anova(f)
    
    a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
    
    
    print(a) 
    
    
    hist(f$residuals, breaks = 40)
    
#Example for 7 trait : rostrum length
    tmp <-df$rostrum_length
    
    hist(tmp, breaks = 20)
    qqnorm(tmp)
    shapiro.test(tmp) 
    
    
    f <- lm(rostrum_length ~ 
              ID_plot/Colony , 
            data = df) 
    
    summary(f)
    a = anova(f)
    
    a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
    
    
    print(a) 
    
    
    hist(f$residuals, breaks = 40)
    
#Example for 8 trait : head width
    tmp <-df$head_width
    
    hist(tmp, breaks = 20)
    qqnorm(tmp)
    shapiro.test(tmp) 
    
    
    f <- lm(head_width ~ 
              ID_plot/Colony , 
            data = df) 
    
    summary(f)
    a = anova(f)
    
    a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
    
    
    print(a) 
    
    
    hist(f$residuals, breaks = 40)
    
#Example for 9 trait : ant3 length
    tmp <-df$ant3_length
    
    hist(tmp, breaks = 20)
    qqnorm(tmp)
    shapiro.test(tmp) 
    
    
    f <- lm(ant3_length ~ 
              ID_plot/Colony , 
            data = df) 
    
    summary(f)
    a = anova(f)
    
    a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
    
    
    print(a) 
    
    
    hist(f$residuals, breaks = 40)
    
#Example for 10 trait : head length
    tmp <-df$head_length
    
    hist(tmp, breaks = 20)
    qqnorm(tmp)
    shapiro.test(tmp) 
    
    
    f <- lm(head_length ~ 
              ID_plot/Colony , 
            data = df) 
    
    summary(f)
    a = anova(f)
    
    a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
    
    
    print(a) 
    
    
    hist(f$residuals, breaks = 40)
    
#Example for 11 trait : thorax width
    tmp <-df$thorax_width
    
    hist(tmp, breaks = 20)
    qqnorm(tmp)
    shapiro.test(tmp) 
    
    
    f <- lm(thorax_width ~ 
              ID_plot/Colony , 
            data = df) 
    
    summary(f)
    a = anova(f)
    
    a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
    
    
    print(a) 
    
    
    hist(f$residuals, breaks = 40)
    
#Example for 12 trait : rhinaria
    tmp <-df$Rhinaria.mean
    
    hist(tmp, breaks = 20)
    qqnorm(tmp)
    shapiro.test(tmp) 
    
    
    f <- lm(Rhinaria.mean ~ 
              ID_plot/Colony , 
            data = df) 
    
    summary(f)
    a = anova(f)
    
    a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
    
    
    print(a) 
    
    
    hist(f$residuals, breaks = 40)
## 2.2 Linear models testing for difference between urban and rural for aphid traits  ####

# Mixed models using lmer package lme4
library(lme4)
library(lmerTest)
    
trait_vector <- c("abdomen_length","body_length","body_width",
 "rostrum_length",      "head_width", "head_length",         "thorax_width",
 "Rhinaria.mean" ,
"ant3_length" , "tarsus_length" ,"femur_length",        "tibia_length" )

for (i in trait_vector) {
  print(paste("now testing trait:", i))
  f <- lmer(as.formula(paste(i, " ~ urban_rural + 
              (1|Colony)")),
            data = df) 
  print(summary(f))
  print(anova(f))
  print(r2(f))
  
  rm(f)
}


# testing absolute asymetry

asym_vec <- c("Rhinaria.asym","ant3_length_asym",
              "tarsus_length_asym",  "femur_length_asym" ,
              "tibia_length_asym")

for (i in asym_vec) {
  print(paste("now testing trait:", i))

  # fit the mixed model with the log of absolute asymetry:
  f <- lmer(as.formula(paste("log(abs(", i, ") + 1) ~ urban_rural + (1|Colony)")), 
          data = df) 
hist(residuals((f)))
qqnorm(residuals((f)))
summary(f)
anova(f) # =>similar to summary of fixed effects
r2(f) # conditional = random + fixed effects; marginal  = only fixed effects
rm(f)
}

## 2.3 Linear models testing for a relationship between aphid traits and % Sealing ####

# Mixed models using lmer package lme4
library(lme4)
library(lmerTest)

# 1 femur length
  f <- lmer(femur_length ~  Seal_500 +
            (1|Colony),
          data = df) 

  summary(f)
    
  anova(f) # =>similar to summary of fixed effects
  r2(f) # conditional = random + fixed effects; marginal  = only fixed effects

# 2 tibia length
  f <- lmer(tibia_length ~  Seal_500 +
              (1|Colony),
            data = df) 
  
  summary(f)
  
  anova(f)
  r2(f)
  
# 3 abdomen length
  f <- lmer(abdomen_length ~  Seal_500 +
              (1|Colony),
            data = df) 
  
  summary(f)
  
  anova(f)
  r2(f)
  
# 4 body length
  f <- lmer(body_length ~  Seal_500 +
              (1|Colony),
            data = df) 
  
  summary(f)
  
  anova(f)
  r2(f)
  
# 5 body width
  f <- lmer(body_width ~  Seal_500 +
              (1|Colony),
            data = df) 
  
  summary(f)
  
  anova(f)
  r2(f)
  
# 6 tarsus length
  f <- lmer(tarsus_length ~  Seal_500 +
              (1|Colony),
            data = df) 
  
  summary(f)
  
  anova(f)
  r2(f)
  

# 7 rostrum length
  f <- lmer(rostrum_length ~  Seal_500 +
              (1|Colony),
            data = df) 
  
  summary(f)
  
  anova(f)
  r2(f)
  
# 8 head width
  f <- lmer(head_width ~  Seal_500 +
              (1|Colony),
            data = df) 
  
  summary(f)
  
  anova(f)
  r2(f)
  
# 9 ant3 length
  f <- lmer(ant3_length ~  Seal_500 +
              (1|Colony),
            data = df) 
  
  summary(f)
  
  anova(f)
  r2(f)
  
# 10 head length
  f <- lmer(head_length ~  Seal_500 +
              (1|Colony),
            data = df) 
  
  summary(f)
  
  anova(f)
  r2(f)
  
# 11 thorax width
  f <- lmer(thorax_width ~  Seal_500 +
              (1|Colony),
            data = df) 
  
  summary(f)
  
  anova(f)
  r2(f)
  
# 12 Rhinaria Mean
  f <- lmer(Rhinaria.mean ~  Seal_500 +
              (1|Colony),
            data = df) 
  
  summary(f)
  
  anova(f)
  r2(f)
  
#illustrate
plot(femur_length ~ Seal_500, df)
abline(lm(femur_length ~ Seal_500, df))
