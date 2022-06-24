library(tidyverse)

dd <- read_csv("temp/leary paper/data/leary_community_stats_temp.csv")

t.s.map <- data.frame(temp.ranges=c(1,2,3),
                      s=c(0.9, 0.5, 0.1),
                      lab=c("18-22C", "20-24C", "22-26C"))

dd <- full_join(dd, t.s.map) %>%
  select(-1, -temp.ranges) %>%
  rename(microcosm_ID = microcosm, species1 = spp1, species2 = spp2, composition_code = m.code,
         temperature_treatment = lab, r_cor = rrrs, k_cor = kkks) %>%
  mutate(microcosm_ID = paste0("M-", microcosm_ID),
         min_temperature = as.numeric(str_sub(temperature_treatment, 1, 2)),
         max_temperature = as.numeric(str_sub(temperature_treatment, 4, 5)))

write_csv(dd, "data/leary_community_stats.csv")
