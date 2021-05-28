### Exploratory analyses of aphid trait distributions

# quick focus on one trait
trait = "femur_length"
aphid_traits %>%
  filter(Trait.type==trait) %>%
  ggplot( aes(x = Length.mm, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 
aphid_traits
  filter(Trait.type==trait)
  ggplot( aes(x = Length.mm, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 

Y<- filter(aphid_traits, Trait.type==trait)
Y %>%
  group_by(collector,ID_plot,Colony, Individual) %>%
  summarise(count = n())


out <- DescTools::Outlier(
  Y$Length.mm,
  na.rm = TRUE,
  value = FALSE
)
Y$Length.mm[out] <- NA

# lme
f <- lm(Length.mm ~ collector + ID_plot + Colony ,
        data = Y) 
summary(f)
a = anova(f)
a$r2 <- a$`Sum Sq`/sum(a$`Sum Sq`)
r2(f)
a
plot(DHARMa::simulateResiduals(f)) # looks pretty good, significance is likely only due to the large number of data points

# LMER
library(lme4)
library(lmerTest)

f <- lmer(Length.mm ~ collector + (1|ID_plot/Colony) ,
          data = Y) 
summary(f)
anova(f)
r = r2(f)
v = as.data.frame(VarCorr(f))
v$r2 <- (v$sdcor/sum(v$sdcor))*(1 - r$R2_marginal)
v # pretty consistent with fixed effect model R2
plot(DHARMa::simulateResiduals(f)) # pretty good (many points = easily signif)

