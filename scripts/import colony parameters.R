# Import colony trait parameters for each location


colony_parameters <- fread("data/field data 2020.csv",
                   data.table = FALSE)
colony_parameters

# "Südkreuz" is written weird