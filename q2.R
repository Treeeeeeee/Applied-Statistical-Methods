ddt = read.csv("DDT.csv")

#What is the mean WEIGHT of SMBUFFALO fish
ddt %>%
  summarize(SMBUFFALOMEAN = mean(LENGTH))

ddt %>%
  summarize(avgWeight = sd(WEIGHT))

ddt %>% filter(LENGTH > 50)

ddt %>% filter(SPECIES == "SMBUFFALO" & LENGTH > 50)


ddt %>% filter(SPECIES == "CCATFISH" &

usethis::create_github_token()
