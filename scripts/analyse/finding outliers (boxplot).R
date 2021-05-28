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

# Additional graph for Rhinaria counts
x11()
aphid_traits %>%
  ggplot( aes(x = Rhinaria.mean, y = ID_plot,    #KLAPPT NICHT :(
              fill = as.factor(collector) )) +
  geom_boxplot()

### IMPORT ERROR SHEET FROM EXCEL ###


### Look at normal distributions ###
#ABDOMEN LENGTH
#by using histogram
aphid_traits$abdomen_length
hist(aphid_traits$abdomen_length,breaks=40,xlab="Abdomen Length",
     main="Histogram of abdomen length")

#by using kernel density plot
d <- density(aphid_traits$abdomen_length)
plot(d)

ks.test(aphid_traits$abdomen_length,"pnorm",
        mean=mean(aphid_traits$abdomen_length),
        sd=sd(aphid_traits$abdomen_length))
#p-value > alpha (alpha=0.05) means we have a normal distribution

#plotting of quantiles
qqnorm(aphid_traits$abdomen_length)
#plotting of line where the quantiles should be
qqline(aphid_traits$abdomen_length)

#ANT3 LENGTH
aphid_traits$ant3_length
hist(aphid_traits$ant3_length,breaks=40,xlab="ant3_length",
     main="Histogram of ant3_length")

d2 <- density(aphid_traits$ant3_length)
plot(d2)

ks.test(aphid_traits$ant3_length,"pnorm",      #NA lässt test nicht zu
        mean=mean(aphid_traits$ant3_length),
        sd=sd(aphid_traits$ant3_length))

qqnorm(aphid_traits$ant3_length)
qqline(aphid_traits$ant3_length)

#BODY LENGTH
aphid_traits$body_length
hist(aphid_traits$body_length,breaks=40,xlab="bodylength",
     main="Histogram of body_length")

d3 <- density(aphid_traits$body_length)
plot(d3)

ks.test(aphid_traits$body_length,"pnorm",     
        mean=mean(aphid_traits$body_length),
        sd=sd(aphid_traits$body_length))

qqnorm(aphid_traits$body_length)
qqline(aphid_traits$body_length)

#HEAD WIDTH
aphid_traits$head_width
hist(aphid_traits$head_width,breaks=40,xlab="head_width",
     main="Histogram of head_width")

d4 <- density(aphid_traits$head_width)
plot(d4)

ks.test(aphid_traits$head_width,"pnorm",     
        mean=mean(aphid_traits$head_width),
        sd=sd(aphid_traits$head_width))

qqnorm(aphid_traits$head_width)
qqline(aphid_traits$head_width)
