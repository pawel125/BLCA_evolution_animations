library(ComplexHeatmap)
library(pracma)
library(tidyverse)

param_file <- "data_raw/Data and results for spatial SFS - unnormalized/parameter_all.xlsx"
vaf_file <- "data_raw/map2-WES-landscape-BC-02-mutFreq-all-heatmap-map19-rmLowFreq=FALSE-data---------.xlsx"

model_params <- openxlsx::read.xlsx(param_file, sheet = 2)
map19_params <- model_params %>%
  filter(str_detect(Map.area.with.max.SFS, "MAP19")) %>%
  mutate(area_max = str_replace_all(Map.area.with.max.SFS, "MAP19-", "")) %>%
  select(mutation_id = Mutation.ID, a, b, c, area_max)

map19_vafs <- openxlsx::read.xlsx(vaf_file)
map19_vafs <- map19_vafs %>%
  rename(mutation_id = X1) %>%
  mutate(
    mutation_id = str_c("MAP19-", mutation_id) %>%
      str_replace_all(":", "-")
  ) %>%
  semi_join(map19_params) %>%
  pivot_longer(cols = -mutation_id, names_to = "sample", values_to = "VAF") %>%
  separate(sample, into = c("sample_nr", "map", "region")) %>%
  group_by(mutation_id) %>%
  arrange(mutation_id, desc(VAF)) %>%
  select(-sample_nr, -map) %>%
  mutate(
    region = case_when(
      region == "T1" ~ "H8",
      TRUE ~ region
    )
  )

map19_vis <- init_mapvis(map19_params, map19_vafs)

saveRDS(map19_params, file = "data/map19_params.Rds")
saveRDS(map19_vafs, file = "data/map19_vafs.Rds")

for (mut_id in map19_vis$mutation_params$mutation_id) {
  map19_vis <- generate_mutation_history(map19_vis, mut_id)
  message(mut_id)
}
saveRDS(map19_vis, file = "data/map19_vis.Rds")
saveRDS(map19_vis, file = "shiny_vis/data/map19_vis.Rds")
