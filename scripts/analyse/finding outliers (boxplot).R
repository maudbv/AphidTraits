###focus on one trait (boxplot) to find outliers###

# Explore all trait correlations ####
library(FactoMineR)

# Create a data frame with aphid traits and plot data 
tmp <- data.frame(
  aphid_traits,
  Seal_500 = plot_data[aphid_traits$ID_plot, "Seal_500"],
  urban_rural = plot_data[aphid_traits$ID_plot, "urban_rural"]
)

tmp <- tmp[order(tmp$Seal_500),]
tmp[is.na(tmp$urban_rural), "urban_rural"] <- "urban"

tmp <- tmp[ ,c("ID_plot", "urban_rural",
               "abdomen_length","body_length","body_width",
               "head_width","rostrum_length","head_length",
               "thorax_width",
               "tarsus_length","femur_length","tibia_length",
               "Rhinaria.mean")]

pca_aphid_traits <- PCA(
  tmp,
  quali.sup = 1:2,  # this is to have "plot" and "urban_rural" as a category
  graph = FALSE
)


trait = "body_length"

# Visualize the distribution per plot + per collector:
aphid_traits_long %>%
  filter(Trait.type==trait) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 
# we see a clear outlier!

#Example 2: look at femur-length
trait2 = "femur_length"

aphid_traits_long %>%
  filter(Trait.type==trait2) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 

#Example 3: look at abdomen length
trait3 = "abdomen_length"

aphid_traits_long %>%
  filter(Trait.type==trait3) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 

#Example 4: look at ant3_length
trait4 = "ant3_length"

aphid_traits_long %>%
  filter(Trait.type==trait4) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 

#Example 5: look at body width
trait5 = "body_width"

aphid_traits_long %>%
  filter(Trait.type==trait5) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 

#Example 6: look at head length
trait6 = "head_length"

aphid_traits_long %>%
  filter(Trait.type==trait6) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 

#Example 7: look at rostrum length
trait7 = "rostrum_length"

aphid_traits_long %>%
  filter(Trait.type==trait7) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 

#Example 8: look at siphunculi length
trait8 = "siph_length"

aphid_traits_long %>%
  filter(Trait.type==trait8) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 

#Example 9: look at tarsus length
trait9 = "tarsus_length"

aphid_traits_long %>%
  filter(Trait.type==trait9) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 

#Example 10: look at thorax width
trait10 = "thorax_width"

aphid_traits_long %>%
  filter(Trait.type==trait10) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 

#Example 11:tibia length
trait11 = "tibia_length"

aphid_traits_long %>%
  filter(Trait.type==trait11) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 

### IMPORT ERROR SHEET FROM EXCEL ###
