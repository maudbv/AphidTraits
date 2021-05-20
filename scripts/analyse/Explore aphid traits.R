### Exploratory analyses of aphid trait distributions

# Explore all trait correlations 
library(FactoMineR)

tmp <-   data.frame(
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

aa <- cbind.data.frame(urban_rural = tmp[,2],pca_aphid_traits$ind$coord)
bb <- coord.ellipse(aa)
plot(pca_aphid_traits, choix = "ind",
    habillage = 1,ellipse = TRUE,
    ylim = c(-5,5), graph.type = "classic" )

plotellipses(pca_aphid_traits,level = .95,
             keepvar = 2,ylim = c(-5,5), 
             label = "none")

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
