### Exploratory analyses of aphid trait distributions

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

# represent the ordination of variables in 2 first dimensions:
plot(pca_aphid_traits, choix = "var")

plot(pca_aphid_traits, choix = "ind",
    habillage = 1,
    ylim = c(-5,5), graph.type = "classic" )

plotellipses(pca_aphid_traits,level = .95,
             keepvar = 2, ylim = c(-5,5), 
             label = "none")

# Example: quick focus on one trait ####
trait = "body_length"

# Visualize the distribution per plot + per collector:
aphid_traits_long %>%
  filter(Trait.type==trait) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 


#Example 2: look at femur-length
trait2 = "femur_length"

aphid_traits_long %>%
  filter(Trait.type==trait2) %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector))) +
  geom_boxplot() 

# Look at all the measured traits together: ####

x11()          #for windows
aphid_traits_long %>%
  ggplot( aes(x = Length.mean, y = ID_plot,
              fill = as.factor(collector) )) +
  geom_boxplot() + 
  facet_wrap(~ Trait.type, scale="free")


# Additional graph for Rhinaria counts
aphid_traits %>%
  ggplot( aes(x = Rhinaria.mean, y = ID_plot,
              fill = as.factor(collector) )) +
  geom_boxplot()

#=> will need to remove the "zeros"



# Look at fluctuating assymetry: ####
# We look at the standard deviation of individual assymetry within each colony,
# so we are using the aphid_SD data frame:
ggplot(aphid_SD,
       aes(x = tarsus_length_asym, y = ID_plot,
           fill = as.factor(collector) )) +
  geom_boxplot()
