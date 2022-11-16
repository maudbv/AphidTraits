# Exploring Judith's aphid traits


df <- aphid_traits %>%
  filter(collector == "Judith", )

# identify roudns of transplantation
df$round <- 1
df$round[grep(pattern = "30",df$date_collected)] <- 2
df$round[grep(pattern = "29",df$date_collected)] <- 2



fig <- ggplot(data = df, 
              mapping = aes( x = body_length, y = ID_plot, colour = round)) +
  geom_point()
fig
