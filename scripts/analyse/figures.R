

# visualize trait distributions per plot with boxplots
par(mar = c(4,4,1,1),las = 2, cex.axis = 0.8) #parameters of plotting area

boxplot (femur_length ~ ID_plot, data = df)

boxplot (tibia_length ~ ID_plot, data = df)

boxplot (abdomen_length ~ ID_plot, data = df)

boxplot (body_length ~ ID_plot, data = df)

boxplot (body_width ~ ID_plot, data = df)

boxplot (tarsus_length ~ ID_plot, data = df)

boxplot (rostrum_length ~ ID_plot, data = df)

boxplot (head_width ~ ID_plot, data = df)

boxplot (ant3_length ~ ID_plot, data = df)

boxplot (head_length ~ ID_plot, data = df)

boxplot (thorax_width ~ ID_plot, data = df)

boxplot (Rhinaria.mean ~ ID_plot, data = df)


# visualize trait distributions along sealing gradient
par(mar = c(4,4,1,1),las = 1, cex.axis = 0.8) #parameters of plotting area

#1 femur length
  plot(femur_length ~  Seal_500, data = df)
  abline(lm(femur_length ~ Seal_500, df))
  
#2 tibia length

  plot(tibia_length ~  Seal_500, data = df)
  abline(lm(tibia_length ~ Seal_500, df))

#3 abdomen length
  
  plot(abdomen_length ~  Seal_500, data = df)
  abline(lm(abdomen_length ~ Seal_500, df))
  
#4 body length
  
  plot(body_length ~  Seal_500, data = df)
  abline(lm(body_length ~ Seal_500, df))
  
#5 body width
  
  plot(body_width ~  Seal_500, data = df)
  abline(lm(body_width ~ Seal_500, df))
  
#6 tarsus length
  
  plot(tarsus_length ~  Seal_500, data = df)
  abline(lm(tarsus_length ~ Seal_500, df))
  
#7 rostrum length
  
  plot(rostrum_length ~  Seal_500, data = df)
  abline(lm(rostrum_length ~ Seal_500, df))
  
#8 head width
  
  plot(head_width ~  Seal_500, data = df)
  abline(lm(head_width ~ Seal_500, df))
  
#9 ant3 length
  
  plot(ant3_length ~  Seal_500, data = df)
  abline(lm(ant3_length ~ Seal_500, df))
  
#10 head length
  
  plot(head_length ~  Seal_500, data = df)
  abline(lm(head_length ~ Seal_500, df))
  
#11 thorax width
  
  plot(thorax_width ~  Seal_500, data = df)
  abline(lm(thorax_width ~ Seal_500, df))

#12 rhinaria mean
  
  plot(Rhinaria.mean ~  Seal_500, data = df)
  abline(lm(Rhinaria.mean ~ Seal_500, df))
  
  
# urbqn rural grouping ####
  boxplot (body_width ~ urban_rural, data = df)
  boxplot (tarsus_length ~ urban_rural, data = df)
  