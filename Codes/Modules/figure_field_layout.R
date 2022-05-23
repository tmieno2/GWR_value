plot_sf <-
  field_sf %>%
  nest_by(plot_id) %>%
  mutate(data = list(
    st_union(data)
  )) %>%
  unnest() %>%
  st_as_sf()

block_sf <-
  field_sf %>%
  nest_by(block_id) %>%
  mutate(data = list(
    st_union(data)
  )) %>%
  unnest() %>%
  st_as_sf()

block_text_sf <- st_centroid(block_sf)

g_field <-
  ggplot() +
  geom_sf(data = plot_sf, size = 0.3) +
  geom_sf(data = filter(plot_sf, plot_id == 260), fill = "red", alpha = 0.5) +
  geom_sf(data = block_sf, fill = NA, size = 1.5) +
  geom_segment(
    aes(x = 540, xend = 540, y = 30, yend = -80),
    arrow = arrow(length = unit(0.5, "cm")),
    size = 1.2,
    color = "red"
  ) +
  geom_sf_text(
    data = block_text_sf,
    aes(label = paste0("block ", block_id)),
    size = 9
  ) +
  theme_void() +
  ggtitle("Panel (a): Plots and blocks of an experimental field")

## inside a plot
plot_sf_focus <- filter(field_sf, plot_id == 1)

subplot_sf <-
  plot_sf_focus %>%
  mutate(label = ifelse(
    buffer == 1,
    "buffer",
    paste0("subplot-", aunit_id - min(aunit_id) + 1)
  )) %>%
  nest_by(label) %>%
  mutate(data = list(
    st_union(data)
  )) %>%
  unnest() %>%
  st_as_sf() %>%
  mutate(buf_or_not = ifelse(label == "buffer", "buffer", "subplot"))

g_inside_plot <-
  ggplot() +
  geom_sf(data = plot_sf_focus, size = 0.4) +
  geom_sf(data = subplot_sf, aes(fill = buf_or_not), size = 1.2, alpha = 0.3) +
  scale_fill_discrete(name = "") +
  theme_void() +
  theme(
    legend.position = "bottom"
  ) +
  ggtitle("Panel (b): Cells, subplot, buffers in a single plot")

g_field / g_inside_plot