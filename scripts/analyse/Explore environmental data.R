# Explore environmental data

# Ordination of plot data in a Principal Component Analysis (PCA)
library(FactoMineR) 

pca_environment <- PCA(
  plot_data[ ,c("urban_rural", # category of plots
                "Seal_500", # % sealing in 500m radius
                "Pop_500", # Human population density
                "ShHerb_500", # % of herbacous biotope areas
                "ShDry_500", # % of dry grasslands areas
                 # "N", "P", "SVF", "Wc",
                 # "SR", "prop.neo",
                "TreeCover_patch", # Tree cover in the patch of grassland
                "Urb_clim")], # urban microclimate category
  quali.sup = 1,  # this is to have "urban_rural" as a category
  graph = FALSE
)

# represent the ordination of variables in 2 first dimensions:
plot(pca_environment, choix = "var")
plot(pca_environment, choix = "ind", habillage = 1)

## Differences in specific factors:
# The difference is in urbanisation factors:
boxplot(Seal_500 ~ urban_rural, data = plot_data)
boxplot(Pop_500 ~ urban_rural, data = plot_data)
boxplot(mean_temp_summer ~ urban_rural, data = plot_data)
boxplot(mean_tempNight_summer ~ urban_rural, data = plot_data)

boxplot(ShHerb_500 ~ urban_rural, data = plot_data)
# increase in herbaceous connectivity in urban areas!

# Increase in soil Phosphorous with urban area
boxplot(P ~ urban_rural, data = plot_data)

#Increase in plant invasions in community:
boxplot(prop.neo ~ urban_rural, data = plot_data)

# No difference in plant species richness:
boxplot(SR ~ urban_rural, data = plot_data)
# No difference in soil Nitrogen:
boxplot(N ~ urban_rural, data = plot_data)

# slightly more open sky/sunlight in urban areas!!
#SVF = Sky View Factor, % of open sky visible from grassland
boxplot(SVF ~ urban_rural, data = plot_data)
