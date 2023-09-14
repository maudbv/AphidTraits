# Import colony trait parameters for each location


colony_parameters <- fread("data/field data 2020.csv",
                   data.table = FALSE)

# "Südkreuz" is written weird

# Correct % parasitized column
# substitute comma by points
colony_parameters$`%parasited` <- sub(colony_parameters$`%parasited`,
    pattern = ",",replacement = ".")
    
# substitute comma by points
colony_parameters$`%parasited` <- sub(colony_parameters$`%parasited`,
                                      pattern = "%",replacement = "")

# substitute comma by points
colony_parameters$`%parasited` <- sub(colony_parameters$`%parasited`,
                                      pattern = "<",replacement = "")

# substitute comma by points
colony_parameters$`%parasited` <- sub(colony_parameters$`%parasited`,
                                      pattern = "<",replacement = "")

# transform into a numeric column
colony_parameters$`%parasited` <- as.numeric(colony_parameters$`%parasited`)

# transform into a numeric column
colnames(colony_parameters)[colnames(colony_parameters) == "%parasited"] <- 
  "perc.parasitized"

