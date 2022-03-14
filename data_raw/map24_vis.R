library(ComplexHeatmap)
library(pracma)
library(tidyverse)

param_file <- "data_raw/Data and results for spatial SFS - unnormalized/parameter_all.xlsx"
vaf_file <- "data_raw/map2-WES-landscape-BC-02-mutFreq-all-heatmap-map24-rmLowFreq=FALSE-data-----------.xlsx"

model_params <- openxlsx::read.xlsx(param_file, sheet = 2)
map24_params <- model_params %>%
  filter(str_detect(Map.area.with.max.SFS, "MAP24")) %>%
  mutate(area_max = str_replace_all(Map.area.with.max.SFS, "MAP24-", "")) %>%
  select(mutation_id = Mutation.ID, a, b, c, area_max)

map24_vafs <- openxlsx::read.xlsx(vaf_file, sheet = 2)
map24_vafs <- map24_vafs %>%
  rename(mutation_id = X1) %>%
  mutate(
    mutation_id = str_c("MAP24-", mutation_id) %>%
      str_replace_all(":", "-")
  ) %>%
  semi_join(map24_params) %>%
  pivot_longer(cols = -mutation_id, names_to = "sample", values_to = "VAF") %>%
  separate(sample, into = c("sample_nr", "map", "region")) %>%
  group_by(mutation_id) %>%
  arrange(mutation_id, desc(VAF)) %>%
  select(-sample_nr, -map) %>%
  mutate(
    region = case_when(
      region == "T1" ~ "G9",
      region == "T2" ~ "F8",
      TRUE ~ region
    )
  )

map24_vis <- init_mapvis(map24_params, map24_vafs)

saveRDS(map24_params, file = "data/map24_params.Rds")
saveRDS(map24_vafs, file = "data/map24_vafs.Rds")

for (mut_id in map24_vis$mutation_params$mutation_id) {
  map24_vis <- generate_mutation_history(map24_vis, mut_id)
  message(mut_id)
}
saveRDS(map24_vis, file = "data/map24_vis.Rds")
saveRDS(map24_vis, file = "shiny_vis/data/map24_vis.Rds")
