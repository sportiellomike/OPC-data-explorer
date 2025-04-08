
#PLOTTING
theme_set(theme(axis.text = element_text(size=8),
                axis.title = element_text(size=8),
                strip.text = element_text(size=8),
                axis.text.x = element_text(angle=0),
                strip.text.x = element_text(size = 8,margin = margin(.1, 0, .1, 0, "cm")),
                legend.text = element_text(size=7),
                legend.title = element_text(size=7),
                legend.position = 'bottom',
                panel.border=element_blank(),
                panel.background = element_blank(),
                panel.grid.major = element_line(color='light grey'))
)


us_states<-us_map(
  regions = c("states")
)
us_counties<-us_map(
  regions = c("counties")
)

us_counties$fips
counties_with_merge<-us_counties |>
  left_join(finalmergesubtwopointfive$fips, by = join_by(fips))


counties_with_merge<-merge(us_counties,
                           finalmergesubtwopointfive,
                            by = 'fips',
                            all.x = T)

counties_with_merge$log2cumulativedeathsaverted<-log2(1+counties_with_merge$cumulativelivessaved)
# Get x-axis limits of the bounding box for the state data
xlim_current <- st_bbox(us_states)$xlim

# Add 540ish km (or 10% of the US) to the bounds (thus shifting the window over)
xlim_expanded <- c(
  xlim_current[1] + (0.1 * diff(xlim_current)), 
  xlim_current[2] + (0.1 * diff(xlim_current))
)

interior_state_borders <- st_intersection(us_states) |>
  filter(n.overlaps > 1) |> 
  # Remove weird points that st_intersection() adds
  filter(!(st_geometry_type(geom) %in% c("POINT", "MULTIPOINT")))


library(viridis)
plot1<-ggplot() +
  # Add counties filled with unemployment levels
  geom_sf(
    data = counties_with_merge, aes(fill = log2cumulativedeathsaverted), linewidth = 0
  ) +
  # Add interior state boundaries
  geom_sf(
    data = interior_state_borders, color = "white", linewidth = 0.25
  ) +
  scale_fill_viridis()+
   coord_sf(crs = st_crs("ESRI:102003"), xlim = xlim_expanded) +
  theme(
    panel.background =  element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.86, 0.32),
    legend.direction = "vertical",

  )




# Convert the county polygons into single points
counties_with_merge_points <- counties_with_merge |> 
  st_point_on_surface()

plot2<-ggplot() +
  # Use a gray background
  geom_sf(data = us_states, fill = "gray90", linewidth = 0) +
  geom_sf(data = interior_state_borders, linewidth = 0.25, color = "white") +
  # Include semi-transparent points with shape 21 (so there's a border)
  geom_sf(
    data = counties_with_merge_points, 
    aes(size = deathsper100k, fill = cumulativelivessaved), 
    pch = 21, color = "white", stroke = 0.25, alpha = 0.5
  ) +
  scale_size_continuous(
    range = c(1, 9), labels = scales::label_comma(), 
    breaks = c(10000, 100000, 1000000),
    guide = guide_legend(override.aes = list(pch = 19, color = "black"))
  ) +
  scale_fill_viridis()+
  
  
  theme(
    panel.background =  element_blank(),
    legend.position.inside = c(0.86, 0.32),
    legend.direction = "vertical",
    legend.position = "right",
  )+
  scale_size_continuous(
    # range = c(1, 11), labels = scales::label_comma(),
    breaks = c(1, 200),
    guide = guide_circles(
      text_position = "right",
      override.aes = list(
        fill = "grey20", alpha = 0.65
      )
    )
  )

