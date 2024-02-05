
 library(dplyr)
 ddt <- read.csv(file.choose())
 #How many LMBASS have a WEIGHT larger than 1200?
   ddt %>% filter(SPECIES == "LMBASS" & WEIGHT > 1200) %>% summarize( n = n())

   #Find the number of LMBASS on the TRM river
   ddt %>% filter(SPECIES == "LMBASS" & RIVER == "TRM" )






   #What is the mean WEIGHT of SMBUFFALO fish
   ddt %>% filter(SPECIES == "SMBUFFALO" & WEIGHT) %>%
     summarize(SMBUFFALOMEAN = mean(WEIGHT))

   #Find the mean squared LENGTH for fish on the TRM (mean of the squared LENGTH)

   ddt %>%
     filter(RIVER == "TRM") %>%
     mutate(SQRLENGTH = LENGTH * LENGTH) %>%
     summarize(mnSqrLength = mean(SQRLENGTH))

