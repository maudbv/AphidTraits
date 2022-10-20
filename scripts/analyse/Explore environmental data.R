# Explore environmental data

# Create table for Methods section:
site_description_table <- plot_data[ ,c("ID_plot",
                "Long",
                "Lat",
                "urban_rural", # category of plots
                "Seal_500" )] # % sealing in 500m radius

colony_sampled <- table(colony_parameters$Plot)
names(colony_sampled)[names(colony_sampled)=="Südkreuz"] <- "Sk_01"
site_description_table$colony_sampled <- colony_sampled[rownames(site_description_table)]

site_description_table <- site_description_table[order(site_description_table$Seal_500),]

write.csv(site_description_table, "results/site_description_table.csv")

# Ordination of plot data in a Principal Component Analysis (PCA)
library(FactoMineR) 

pca_environment <- PCA(
  plot_data[ ,c("urban_rural", # category of plots
                "Seal_500", # % sealing in 500m radius
                "Pop_500", # Human population density
                "ShHerb_500", # % of herbacous biotope areas
                "Urb_clim")], # urban microclimate category
  quali.sup = 1,  # this is to have "urban_rural" as a category
  graph = FALSE
)

# represent the ordination of variables in 2 first dimensions:
par(mfrow = c(1,2))
plot(pca_environment, choix = "var",
     graph.type = "classic")
plot(pca_environment, choix = "ind", habillage = 1,
     ,graph.type = "classic")

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
