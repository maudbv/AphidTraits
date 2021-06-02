#how to fix wrong magnifications -> change in aphid-df file
aphid_df
                            #call it aphid_dftest
aphid_dftest <- aphid_df
#changing magn. from 0.63 to 2 in Oh_01_3_E femur_length_left for all three measurments
aphid_dftest[4165,19] <- "2"
aphid_dftest[4166,19] <- "2"
aphid_dftest[4167,19] <- "2"

#changing magn. from 0.63 to 2 in Oh_01_3_E femur_length_right for all three measurments
aphid_dftest[4178,19] <- "2"
aphid_dftest[4179,19] <- "2"
aphid_dftest[4180,19] <- "2"

#should be possible to change all measurements for right / left leg
aphid_dftest[4177:4185,19] <- "2"       #for right legs
aphid_dftest[4162:4170,19] <- "2"       #for left legs

#changing magn. from 0.63 to 2 in Oh_01_3_E ant3_length_left for all three measurments
aphid_dftest[4159:4161,19] <- "2"

#changing magn. from 0.63 to 2 in Oh_01_3_E ant3_length_right for all three measurments
aphid_dftest[4174:4176,19] <- "2"

#changing magn. from 2 to 1 in Oh_01_3_D ventral (head_length+thorax_width) for all six measurments
aphid_dftest[c(4144:4149),19] <- "1" 

#changing magn. from 2 to 1 in Nh_10_1_B ventral(head_length+thorax_width) for all six measurments
aphid_dftest[c(1048:1053),19] <- "1" 

#changing magn. from 3 to 4 in Oh_01_2_B rostrum_length_left for all three measurments
aphid_dftest[3808:3810,19] <- "4"

#changing magn. from 3 to 4 in Oh_01_2_C rostrum_length_left for all three measurments
aphid_dftest[3847:3849,19] <- "4" 

# import outlier sheet with the columns from Maud

library(readr)
outliers <- read_csv("data/outliers.csv")   #csv is weird

library(readxl) #inserting excel sheet
Kopie_von_outliers_maud_1 <- read_excel("data/Kopie von outliers_maud-1.xlsx")
View(Kopie_von_outliers_maud_1)

###aphid_dftest1=cbind(aphid_dftest,Kopie_von_outliers_maud_1$column_to_change)###

aphid_dftest[]