model = read_model_input(scientificname = "Placopecten magellanicus")

brickman_all <- read_brickman(
  db_mon,
  add = "depth"
)


monthly_points <-
  model |>
  group_by(month, class) |>
  slice_sample(n = 1) |>
  ungroup() |>
  group_by(month) |>
  mutate(point = ifelse(class == "presence", "p1", "p2")) |>
  ungroup()

brickman_values <-
  extract_brickman(
    x = brickman_all,
    y = monthly_points,
    form = "wide"
  )

final_table <-
  brickman_values |>
  select(
    month,
    class,
    point,
    depth,
    SSS,
    SST
  ) |>
  arrange(month, class)

print(final_table, n = 24)

final_table |>
  ggplot() +
  geom_sf(aes(color = class), size = 2) +
  facet_wrap(~ month) +
  theme_minimal() +
  labs(
    title = "Random monthly presence/background points",
    color = "Point type"
  )
