#how to fix wrong magnifications -> change in aphid-df file
aphid_df
                            #call it aphid_dftest
aphid_dftest <- aphid_df
#changing magn. from 0.63 to 2 in Oh_01_3_E femur_length_left for all three measurments

which(aphid_dftest$Individual == "Oh_01-3-E" & aphid_dftest$Trait == "femur_length_left")


aphid_dftest[
    which(aphid_dftest$Individual == "Oh_01-3-E" & 
    aphid_dftest$Trait == "femur_length_left"),
    "Magnification"] <- "2"

aphid_dftest %>%
  filter(Individual == "Oh_01-3-E",
         Trait == "femur_length_left")


# trying to change whole leg magn. at once

#Changing magnification of outliers in femur+tarsus+tibia+ant3 length of Oh_01-3-E
which(aphid_dftest$Individual == "Oh_01-3-E" & aphid_dftest$Trait.type == "femur_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Oh_01-3-E" & 
          aphid_dftest$Trait.type == "femur_length"),
  "Magnification"] <- "2"

which(aphid_dftest$Individual == "Oh_01-3-E" & aphid_dftest$Trait.type == "tarsus_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Oh_01-3-E" & 
          aphid_dftest$Trait.type == "tarsus_length"),
  "Magnification"] <- "2"

which(aphid_dftest$Individual == "Oh_01-3-E" & aphid_dftest$Trait.type == "tibia_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Oh_01-3-E" & 
          aphid_dftest$Trait.type == "tibia_length"),
  "Magnification"] <- "2"

which(aphid_dftest$Individual == "Oh_01-3-E" & aphid_dftest$Trait.type == "ant3_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Oh_01-3-E" & 
          aphid_dftest$Trait.type == "ant3_length"),
  "Magnification"] <- "2"

#Changing magnification of outliers in rostrum length of Oh_01-2-B and Oh_01-2-C
which(aphid_dftest$Individual == "Oh_01-2-B" & aphid_dftest$Trait.type == "rostrum_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Oh_01-2-B" & 
          aphid_dftest$Trait.type == "rostrum_length"),
  "Magnification"] <- "4"

which(aphid_dftest$Individual == "Oh_01-2-C" & aphid_dftest$Trait.type == "rostrum_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Oh_01-2-C" & 
          aphid_dftest$Trait.type == "rostrum_length"),
  "Magnification"] <- "4"

#Changing magnification of outliers in ventral.jpg of Oh_01-3-D and Nh_10-1-B
which(aphid_dftest$Individual == "Oh_01-3-D" & aphid_dftest$PhotoType == "ventral.jpg")

aphid_dftest[
  which(aphid_dftest$Individual == "Oh_01-3-D" & 
          aphid_dftest$PhotoType == "ventral.jpg"),
  "Magnification"] <- "1"

which(aphid_dftest$Individual == "Nh_10-1-B" & aphid_dftest$PhotoType == "ventral.jpg")

aphid_dftest[
  which(aphid_dftest$Individual == "Nh_10-1-B" & 
          aphid_dftest$PhotoType == "ventral.jpg"),
  "Magnification"] <- "1"

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






#Changing outliers to NA

which(aphid_dftest$Individual == "Oh_01-1.2-E" & aphid_dftest$Trait.type == "abdomen_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Oh_01-1.2-E" & 
          aphid_dftest$Trait.type == "abdomen_length"),
  "Length.mm"] <- "NA"

which(aphid_dftest$Individual == "Oh_02-3-B" & aphid_dftest$Trait.type == "body_width")

aphid_dftest[
  which(aphid_dftest$Individual == "Oh_02-3-B" & 
          aphid_dftest$Trait.type == "body_width"),
  "Length.mm"] <- "NA"

which(aphid_dftest$Individual == "Oh_01-2-C" & aphid_dftest$Trait.type == "head_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Oh_01-2-C" & 
          aphid_dftest$Trait.type == "head_length"),
  "Length.mm"] <- "NA"

which(aphid_dftest$Individual == "Om_06-1-D" & aphid_dftest$Trait.type == "rostrum_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Om_06-1-D" & 
          aphid_dftest$Trait.type == "rostrum_length"),
  "Length.mm"] <- "NA"

which(aphid_dftest$Individual == "Om_06-1-C" & aphid_dftest$Trait.type == "rostrum_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Om_06-1-C" & 
          aphid_dftest$Trait.type == "rostrum_length"),
  "Length.mm"] <- "NA"

which(aphid_dftest$Individual == "Om_02-1-B" & aphid_dftest$Trait == "ant3_length_right")

aphid_dftest[
  which(aphid_dftest$Individual == "Om_02-1-B" & 
          aphid_dftest$Trait == "ant3_length_right"),
  "Length.mm"] <- "NA"

which(aphid_dftest$Individual == "Oh_01-3-E" & aphid_dftest$Trait.type == "rostrum_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Oh_01-3-E" & 
          aphid_dftest$Trait.type == "rostrum_length"),
  "Length.mm"] <- "NA"

# Changing outliers by giving correct name to measurement
#(in "Trait" and "Trait.type" but not in "TraitLabel", because that's the original label)

which(aphid_dftest$Individual == "Nl_55-1-A" & aphid_dftest$Trait.type == "siph_length" &
        aphid_dftest$Trait == "siph_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Nl_55-1-A" & 
          aphid_dftest$Trait.type == "siph_length" &
          aphid_dftest$Trait == "siph_length"),
  "Trait.type" ] <- "head_width"

aphid_dftest[
  which(aphid_dftest$Individual == "Nl_55-1-A" & 
          aphid_dftest$Trait == "siph_length"),
  "Trait" ] <- "head_width"

# WS9_3_A is individual Ol_55-A-A
which(aphid_dftest$Individual == "Ol_55-A-A" & aphid_dftest$Trait.type == "siph_length" &
        aphid_dftest$Trait == "siph_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Ol_55-A-A" & 
          aphid_dftest$Trait.type == "siph_length" &
          aphid_dftest$Trait == "siph_length"),
  "Trait.type" ] <- "head_width"

aphid_dftest[
  which(aphid_dftest$Individual == "Ol_55-A-A" & 
          aphid_dftest$Trait == "siph_length"),
  "Trait" ] <- "head_width"


# SS_U3_1_C is individual Nl_55-1-C

which(aphid_dftest$Individual == "Nl_55-1-C" & aphid_dftest$Trait.type == "siph_length" &
        aphid_dftest$Trait == "siph_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Nl_55-1-C" & 
          aphid_dftest$Trait.type == "siph_length" &
          aphid_dftest$Trait == "siph_length"),
  "Trait.type" ] <- "head_width"

aphid_dftest[
  which(aphid_dftest$Individual == "Nl_55-1-C" & 
          aphid_dftest$Trait == "siph_length"),
  "Trait" ] <- "head_width"

# MS_R3B_E is individual Ol_11-E-E

which(aphid_dftest$Individual == "Ol_11-E-E" & aphid_dftest$Trait.type == "siph_length" &
        aphid_dftest$Trait == "siph_length")

aphid_dftest[
  which(aphid_dftest$Individual == "Ol_11-E-E" & 
          aphid_dftest$Trait.type == "siph_length" &
          aphid_dftest$Trait == "siph_length"),
  "Trait.type" ] <- "head_width"

aphid_dftest[
  which(aphid_dftest$Individual == "Ol_11-E-E" & 
          aphid_dftest$Trait == "siph_length"),
  "Trait" ] <- "head_width"







##### changing aphid_dftest to aphfid_df

#Changing magnification of outliers in femur+tarsus+tibia+ant3 length of Oh_01-3-E
which(aphid_df$Individual == "Oh_01-3-E" & aphid_df$Trait.type == "femur_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-3-E" & 
          aphid_df$Trait.type == "femur_length"),
  "Magnification"] <- "2"

which(aphid_df$Individual == "Oh_01-3-E" & aphid_df$Trait.type == "tarsus_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-3-E" & 
          aphid_df$Trait.type == "tarsus_length"),
  "Magnification"] <- "2"

which(aphid_df$Individual == "Oh_01-3-E" & aphid_df$Trait.type == "tibia_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-3-E" & 
          aphid_df$Trait.type == "tibia_length"),
  "Magnification"] <- "2"

which(aphid_df$Individual == "Oh_01-3-E" & aphid_df$Trait.type == "ant3_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-3-E" & 
          aphid_df$Trait.type == "ant3_length"),
  "Magnification"] <- "2"

#Changing magnification of outliers in rostrum length of Oh_01-2-B and Oh_01-2-C
which(aphid_df$Individual == "Oh_01-2-B" & aphid_df$Trait.type == "rostrum_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-2-B" & 
          aphid_df$Trait.type == "rostrum_length"),
  "Magnification"] <- "4"

which(aphid_df$Individual == "Oh_01-2-C" & aphid_df$Trait.type == "rostrum_length")

aphid_df[
  which(aphid_df$Individual == "Oh_01-2-C" & 
          aphid_df$Trait.type == "rostrum_length"),
  "Magnification"] <- "4"

#Changing magnification of outliers in ventral.jpg of Oh_01-3-D and Nh_10-1-B
which(aphid_df$Individual == "Oh_01-3-D" & aphid_df$PhotoType == "ventral.jpg")

aphid_df[
  which(aphid_df$Individual == "Oh_01-3-D" & 
          aphid_df$PhotoType == "ventral.jpg"),
  "Magnification"] <- "1"

which(aphid_df$Individual == "Nh_10-1-B" & aphid_df$PhotoType == "ventral.jpg")

aphid_df[
  which(aphid_df$Individual == "Nh_10-1-B" & 
          aphid_df$PhotoType == "ventral.jpg"),
  "Magnification"] <- "1"