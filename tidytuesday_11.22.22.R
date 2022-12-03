# Tidy Tuesday 11.22.22 challenge

install.packages("tmap")
install.packages("tidycensus")
install.packages("mapview")
install.packages("maps")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggh4x")

library(tidytuesdayR)
library(tidyverse)
library(monochromeR)
library(ggtext)
library(ragg)
library(systemfonts)
library(textshaping)
library(sf)
library(raster)
library(tmap)
library(tidycensus)
library(mapview)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(mapproj)
library(ggh4x)
library(monochromeR)
library(scales)
library(openxlsx)



# Load this week's data
tuesdata <- tidytuesdayR::tt_load(2022, week = 47)

museums <- tuesdata$museums

# Potential data viz ideas:
## Try to make a map of where all of these musuems are located (potentially use different colors)
## Perhaps something with the various subject matters covered?

museums$Subject_Matter

# Let's take a quick look at the subject matters
museums %>% group_by(Subject_Matter) %>%
  summarise(subject_count = n()) %>% view()

# Looks like there is larger subject matter group at the beginning of each subject
# Let's grab that


subjects <- museums %>%
  mutate(subject = str_extract(string = Subject_Matter, pattern = "^\\w+")) %>%
  group_by(subject) %>%
  summarise(subject_count = n()) %>%
  arrange(desc(subject_count))
# Okay the 5 most common musuem subjects are:
# 1. Local Histories
# 2. Buildings
# 3. Wars and conflict
# 4. Arts
# 5. Transport

ggplot(subjects, aes(x = subject, y = subject_count)) +
  geom_col()

education <- museums %>%
  mutate(subject = str_extract(string = Subject_Matter, pattern = "^\\w+")) %>%
  group_by(subject) %>%
  summarise(educ_avg = mean(Area_Deprivation_index_education, na.rm = TRUE))

ggplot(education, aes(x = subject, y = educ_avg)) +
  geom_col() +
  coord_flip()
# This works, but I would prefer that it is a scatter plot with the averages as a bar



ggplot(education, aes(x = subject, y = educ_avg)) +
  geom_col() +
  coord_flip()

# Okay, let's try just looking at one subject matter for now
buildings <- museums %>%
  mutate(subject = str_extract(string = Subject_Matter, pattern = "^\\w+")) %>%
  filter(subject == "Buildings")

ggplot(buildings, aes(x = Area_Deprivation_index_education, y = mean())) +
  geom_point()


### Maps

# this doesn't work because of some API thing
us_state_pop <- get_acs(
  geography = "state",
  year = 2019, # this is what year of census info you're pulling
  variables = c("population" = "B01001_001"),
  geometry = TRUE) # tidycensus will download us the shape files for the states


world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf()

uk <- map_data(map = "world", region = "UK")



# This works, but is just a map of the UK, with different colors for each country within
ggplot(uk, aes(x = long, y = lat, group = group, fill = subregion)) +
  geom_polygon() +
  coord_map()

# there is an outlier that is really far north, gotta drop that [figured out this issue from npechl on Github]
ggplot() +
  geom_polygon(data = uk, aes(x = long, y = lat, group = group)) +
  geom_point(data = museums, aes(x = Longitude, y = Latitude, fill = Area_Deprivation_index))

clean_museums <- museums %>% filter(Latitude <= max(uk$lat)) %>%
  mutate(subject = str_extract(string = Subject_Matter, pattern = "^\\w+")) %>%
  filter(subject == "Local_Histories" | subject == "Buildings" | subject == "War_and_conflict" |
  subject == "Arts" | subject == "Transport")

font <- "Calibri"

museum_colors <- list(War_and_conflict = "#6f4c47",
                      Arts = "#8d7b8f",
                      Local_histories = "#cfa88b")

view_palette(museum_colors)

dark_red <- generate_palette(museum_colors$War_and_conflict, modification = "go_darker",
                 n_colours = 3)[2]

dark_purple <- generate_palette(museum_colors$Arts, modification = "go_darker",
                             n_colours = 3)[2]


view_palette(dark_purple)

museum_colors <- list(War_and_conflict = "#53191b",
                      Arts = "#8d7b8f",
                      Local_Histories = "#cfa88b",
                      Transport = "#3c8481",
                      Buildings = "#a26115")

view_palette(museum_colors)

"#c4541c"

maroon "#6f4c47"
purple "#8d7b8f"
tan "#cfa88b"
black "#242126"
teal "#3c8481"


ggplot() +
  geom_polygon(data = uk, aes(x = long, y = lat, group = group)) +
  geom_point(data = clean_museums, aes(x = Longitude, y = Latitude, color = subject)) +
  coord_map() +
  theme_minimal() +
  labs(title = "Out of over 4,000 musuems in the UK, Local Histories is the most common subject",
       subtitle = "We've mapped the 5 most common musuem subjects below",
       caption = "Source: Mapping Musuems project, #TidyTuesday Week 47") +
  theme(axis.text = element_blank(),
        plot.title = element_text(size = 18,
                                  family = font,
                                  face = "bold",
                                  hjust = 0.4,
                                  color = "#242126"),
        plot.subtitle = element_text(size = 11,
                                     family = font,
                                     hjust = 0.4),
        plot.caption = element_text(size = 8,
                                    family = font),
        panel.grid.major = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size = 11,
                                    family = font)) +
  scale_color_manual(values = museum_colors)

# Nice, this looks better

# Maybe we should only select the top 5 most common musuem subjects?
# Yes, way better

# To do:
## Fix names in legend
## Make title into element markdown, changes local histories to tan color
## Make plot more vertical
