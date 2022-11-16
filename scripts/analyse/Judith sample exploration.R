# Exploring Judith's aphid traits

df <- aphid_traits %>%
  filter(collector == "Judith", )

# identify roudns of transplantation
df$round <- 1
df$round[grep(pattern = "30",df$date_collected)] <- 2
df$round[grep(pattern = "29",df$date_collected)] <- 2
df$round <- as.factor(df$round)

# Create a data frame with aphid traits and plot data
df <- data.frame(
  df,
  Seal_500 = plot_data[df$ID_plot, "Seal_500"],
  prop_neo = plot_data[df$ID_plot, "prop.neo"],
  Night_temp = plot_data[df$ID_plot, "mean_tempNight_summer"],
  urban_rural = plot_data[df$ID_plot, "urban_rural"]
)

tmp <- df[order(df$Seal_500),]
tmp[is.na(tmp$urban_rural), "urban_rural"] <- "urban"

tmp <- tmp[ ,c("ID_plot", "urban_rural",
               "abdomen_length","body_length","body_width",
               "head_width","rostrum_length","head_length",
               "thorax_width",
               "tarsus_length","femur_length","tibia_length",
               "Rhinaria.mean")]

pca_aphid_traits <- FactoMineR::PCA(
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



# Between plot differences
fig <- ggplot(data = df, 
              mapping = aes( x = body_length, y = ID_plot, colour = round)) +
  geom_point()
fig

# Effect of urbanisation
cor.test(~prop_neo + Seal_500, data = df)

# Body size
fig <- ggplot(data = df, 
              mapping = aes( x = ,
                             y = body_width, 
                             color = round)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(facets = ~ round)
fig

# Legs
fig <- ggplot(data = df, 
              mapping = aes( x = Seal_500,
                             y = femur_length, 
                             color = round)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(facets = ~ round)
fig

# Rhinaria
fig <- ggplot(data = df, 
              mapping = aes( x = Seal_500,
                             y = rostrum_length, 
                             color = round)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(facets = ~ round)
fig
