### Exploratory analyses of aphid trait distributions

# Example: quick focus on one trait ####
trait = "femur_length"

# Visualize the distribution per plot + per collector:
aphid_traits_long %>%
  filter(Trait.type==trait) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 
# we see a clear outlier!

# Look at all the measured traits together: ####

quartz()
aphid_traits_long %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector) )) +
  geom_boxplot() + 
  facet_wrap(~ Trait.type, scale="free")

# => will need to remove outliers for each trait

# Additional graph for Rhinaria counts
quartz()
aphid_traits %>%
  ggplot( aes(x = Rhinaria.mean, y = ID_plot,
              fill = as.factor(collector) )) +
  geom_boxplot()

#=> will need to remove the "zeros"

## Plant traits ###
# Additional graph for Rhinaria counts
plant_traits %>%
  ggplot( aes(x = SLA.mean, y = Plot)) +
  geom_boxplot()

plant_traits %>%
  ggplot( aes(x = LDMC.mean, y = Plot)) +
  geom_boxplot()

plant_traits %>%
  ggplot( aes(x = SDMC.mean, y = Plot)) +
  geom_boxplot()
