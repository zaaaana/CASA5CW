# load data
files <- list.files("us_fires")

us_fires <- data_frame()

for (f in files) {
  tmp <- read_csv(paste0("us_fires/",f), col_types = cols(
    .default = col_character(),
    stat_cause_code = col_double(),
    cont_date = col_datetime(format = ""),
    discovery_date = col_datetime(format = ""),
    cont_doy = col_integer(),
    cont_time = col_integer(),
    fire_size = col_double(),
    latitude = col_double(),
    longitude = col_double()
  ))
  us_fires <- bind_rows(us_fires,tmp)
}
rm(tmp)

# assign fires to main causes
us_fires <- us_fires %>%
  mutate(cause = case_when(stat_cause_code == 1  ~ "Natural",
                           stat_cause_code == 13 | is.na(stat_cause_code) ~ "Unknown",
                           stat_cause_code >= 2 | stat_cause_code <= 12 ~ "Human"),
         date = as.Date(case_when(is.na(discovery_date) ~ cont_date,
                                  !is.na(discovery_date) ~ discovery_date)))

# assign fires to a grid with half-degree latitude and longitude resolution
cells <- function(xy, origin = c(0,0), cellsize = c(0.5,0.5)) {
  t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
}
centroids <- cells(cbind(us_fires$latitude, us_fires$longitude))
us_fires$x <- centroids[, 2]
us_fires$y <- centroids[, 1]
us_fires$cell <- paste(us_fires$x, us_fires$y)

# total area burned per cell
grid_us_fires_total <- us_fires %>%
  group_by(x,y,cell) %>%
  summarize(total_acres = sum(fire_size))

# area burned per cell for natural fires
grid_us_fires_natural <- us_fires %>%
  filter(cause == "Natural") %>%
  group_by(cause,x,y,cell) %>%
  summarize(natural_acres = sum(fire_size)) %>%
  ungroup() %>%
  select(-cause)

# area burned per cell for human-caused fires
grid_us_fires_human <- us_fires %>%
  filter(cause == "Human") %>%
  group_by(cause,x,y,cell) %>%
  summarize(human_acres = sum(fire_size)) %>%
  ungroup() %>%
  select(-cause)

# combine into a single data frame and replace NAs with zeros
grid_us_fires <- left_join(grid_us_fires_total, grid_us_fires_natural) %>%
  left_join(grid_us_fires_human)
grid_us_fires[is.na(grid_us_fires)] <- 0

# calculate % acres burned in fires cause by humans (where cause is known)
grid_us_fires <- grid_us_fires %>%
  mutate(pc_human_acres = human_acres/(human_acres+natural_acres)*100)

# for cells in which all fires are of unknown cause, assign a value of 50%
grid_us_fires$pc_human_acres[is.nan(grid_us_fires$pc_human_acres)] <- 50

# filter for continental US and remove cells with less than 50 acres burned per year
grid_us_fires <- grid_us_fires %>%
  filter(x < -65 & x > -125 & y > 24 & y < 50 & total_acres > 1200)

# plot
ggplot(grid_us_fires) +
  geom_point(aes(x = x, y = y, size = total_acres, color = pc_human_acres), alpha = 0.7) +
  borders("state", xlim = c(-125, -65), ylim = c(24, 50), size = 0.2) +
  scale_size_area(max_size = 4, guide = FALSE) +
  scale_color_gradient2(low = "#950fdf", mid = "#ffffff", high = "#ffff00", midpoint = 50, guide = "legend", name = "% burned in human-caused fires") +
  coord_map("mercator") +
  theme_map(base_size = 16, base_family = "ProximaNova-Semibold") +
  theme(axis.line = element_blank(),axis.text.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#2c2c2d"),
        legend.background = element_rect(fill = "#2c2c2d"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.text = element_text(color = "#ffffff"),
        legend.title = element_text(color = "#ffffff"),
        legend.key = element_rect(fill = "#2c2c2d")) +
  guides(color = guide_legend(title.position="top", title.hjust = 0.5))

tmaptools::palette_explorer()


