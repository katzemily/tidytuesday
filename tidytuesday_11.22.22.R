# Tidy Tuesday 11.22.22 challenge

rm(list=ls())

# install.packages("tmap")
# install.packages("tidycensus")
# install.packages("mapview")
# install.packages("maps")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("ggh4x")

library(tidytuesdayR)
library(tidyverse)
library(monochromeR)
library(ggtext)
library(sf)
library(mapview)
library(scales)

library(ragg)
library(systemfonts)
library(textshaping)

library(raster)
library(tmap)
library(tidycensus)

library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(mapproj)
library(ggh4x)
library(monochromeR)

library(openxlsx)



# Load this week's data
tuesdata <- tidytuesdayR::tt_load(2022, week = 47)

museums <- tuesdata$museums

# Potential data viz ideas:
## Try to make a map of where all of these musuems are located (potentially use different colors)
## Perhaps something with the various subject matters covered?

# museums$Subject_Matter

# Let's take a quick look at the subject matters
# museums %>% group_by(Subject_Matter) %>%
#   summarise(subject_count = n()) %>% view()

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

# ggplot(subjects, aes(x = subject, y = subject_count)) +
#   geom_col()
#
# education <- museums %>%
#   mutate(subject = str_extract(string = Subject_Matter, pattern = "^\\w+")) %>%
#   group_by(subject) %>%
#   summarise(educ_avg = mean(Area_Deprivation_index_education, na.rm = TRUE))
#
# ggplot(education, aes(x = subject, y = educ_avg)) +
#   geom_col() +
#   coord_flip()
# This works, but I would prefer that it is a scatter plot with the averages as a bar



# ggplot(education, aes(x = subject, y = educ_avg)) +
#   geom_col() +
#   coord_flip()
#
# # Okay, let's try just looking at one subject matter for now
# buildings <- museums %>%
#   mutate(subject = str_extract(string = Subject_Matter, pattern = "^\\w+")) %>%
#   filter(subject == "Buildings")
#
# ggplot(buildings, aes(x = Area_Deprivation_index_education, y = mean())) +
#   geom_point()


### Maps

# this doesn't work because of some API thing
# us_state_pop <- get_acs(
#   geography = "state",
#   year = 2019, # this is what year of census info you're pulling
#   variables = c("population" = "B01001_001"),
#   geometry = TRUE) # tidycensus will download us the shape files for the states
#
#
# world <- ne_countries(scale = "medium", returnclass = "sf")
#
# ggplot(data = world) +
#   geom_sf()
#
uk <- map_data(map = "world", region = "UK")
#
#
#
# # This works, but is just a map of the UK, with different colors for each country within
# ggplot(uk, aes(x = long, y = lat, group = group, fill = subregion)) +
#   geom_polygon() +
#   coord_map()
#
# # there is an outlier that is really far north, gotta drop that [figured out this issue from npechl on Github]
# ggplot() +
#   geom_polygon(data = uk, aes(x = long, y = lat, group = group)) +
#   geom_point(data = museums, aes(x = Longitude, y = Latitude, fill = Area_Deprivation_index))

clean_museums <- museums %>% filter(Latitude <= max(uk$lat)) %>%
  mutate(subject = str_extract(string = Subject_Matter, pattern = "^\\w+")) %>%
  filter(subject == "Local_Histories" | subject == "Buildings" | subject == "War_and_conflict" |
  subject == "Arts" | subject == "Transport")

font <- "Calibri"

# museum_colors <- list(War_and_conflict = "#6f4c47",
#                       Arts = "#8d7b8f",
#                       Local_histories = "#cfa88b")
#
# view_palette(museum_colors)
#
# dark_red <- generate_palette(museum_colors$War_and_conflict, modification = "go_darker",
#                  n_colours = 3)[2]
#
# dark_purple <- generate_palette(museum_colors$Arts, modification = "go_darker",
#                              n_colours = 3)[2]
#
#
# view_palette(dark_purple)



# "#c4541c"
#
# maroon "#6f4c47"
# purple "#8d7b8f"
# tan "#cfa88b"
# black "#242126"
# teal "#3c8481"


legend_names <- list(War_and_conflict = "3. War and Conflict",
                     Arts = "4. Arts",
                     Local_Histories = "1. Local Histories",
                     Transport = "5. Transport",
                     Buildings = "2. Buildings")

museum_colors <- list(War_and_conflict = "#53191b",
                      Arts = "#8d7b8f",
                      Local_Histories = "#cfa88b",
                      Transport = "#3c8481",
                      Buildings = "#a26115")

# background <- generate_palette(museum_colors$Arts, modification = "go_lighter", n_colours = 4)[2]
#
# view_palette(background)

view_palette(museum_colors)

secondary_text <- generate_palette("#242126", modification = "go_lighter", n_colours = 4)[1]

view_palette(secondary_text)

# levels(clean_museums$subject) <- c("1. Local Histories",
#                                    "2. Buildings",
#                                    "3. War and Conflict",
#                                    "4. Arts",
#                                    "5. Transport")

museum_plot <- ggplot() +
  geom_polygon(data = uk, aes(x = long, y = lat, group = group)) +
  geom_point(data = clean_museums, aes(x = Longitude, y = Latitude, color = subject)) +
  coord_map() +
  theme_minimal() +
  labs(title = paste0("Throughout the UK's 4,000+ Museums,<br> <span style=\"color:",
                      museum_colors$Local_Histories, "\">Local Histories</span> is the most common subject."),
       subtitle = "We've mapped the 5 most common musuem subjects below.",
       caption = "\nDataviz by @katzemily \nSource: Mapping Musuems project, #TidyTuesday Week 47") +
  theme(axis.text = element_blank(),
        text = element_text(size = 14,
                            family = font,
                            color = secondary_text),
        plot.title = element_markdown(size = 24,
                                  family = font,
                                  face = "bold",
                                  color = "#242126",
                                  margin = margin(5,0,5,0)),
        plot.caption = element_text(size = 8,
                                    family = font,
                                    lineheight = 1.3),
        plot.caption.position = "plot",
        panel.grid.major = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(face = "bold")) +
  scale_color_manual(name = "Subject",
                     labels = legend_names,
                     values = museum_colors,
                     limits = c("Local_Histories",
                                'Buildings',
                                'War_and_conflict',
                                'Arts',
                                'Transport'))

museum_plot

ggsave("tidytuesday_11.22.22.png", plot = museum_plot,
       width = 7, height = 10, units = "in")

ggsave("tidytuesday_11.22.22.jpg", plot = museum_plot,
       width = 7, height = 10, units = "in")



# Nice, this looks better

# Maybe we should only select the top 5 most common musuem subjects?
# Yes, way better

# To do:
## Fix names in legend
## Make title into element markdown, changes local histories to tan color
## Make plot more vertical
