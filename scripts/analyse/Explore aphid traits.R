### Exploratory analyses of aphid trait distributions

# quick focus on one trait
trait = "femur_length"

# Visualize the distribution per plot + per collector:
aphid_traits_long %>%
  filter(Trait.type==trait) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 

# Select subset of data:
Y<- filter(aphid_traits_long, Trait.type==trait) %>%
  group_by(collector,ID_plot,Colony, Individual) %>%
  summarise(count = n())

# RUN ANALYSES lme
f <- lm(Length.mean ~ collector + ID_plot + Colony ,
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

f <- lmer(Length.mean ~ collector + (1|ID_plot/Colony) ,
          data = Y) 
summary(f)
anova(f)
r = r2(f)
v = as.data.frame(VarCorr(f))
v$r2 <- (v$sdcor/sum(v$sdcor))*(1 - r$R2_marginal)
v # pretty consistent with fixed effect model R2
plot(DHARMa::simulateResiduals(f)) # pretty good (many points = easily signif)


## Plant traits ###
tmp <- merge(x = plant_traits, y = plot_data, 
             by.x = "Plot", by.y = "ID_plot", all.x = TRUE)

# No apparent trends with sealing
plot(SLA.mean ~ Seal_500, tmp)
plot(SLA.mean ~ prop.neo, tmp)
plot(SLA.mean ~ SVF, tmp)
plot(SLA.mean ~ N, tmp)


plot(LDMC.mean ~ SDMC.mean, tmp)
plot(SDMC.mean ~ Seal_500, tmp)
plot(stem.density.mean ~ Seal_500, tmp)

                       