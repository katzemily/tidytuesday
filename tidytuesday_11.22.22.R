# Tidy Tuesday 11.22.22 challenge

rm(list=ls())


library(tidytuesdayR)
library(tidyverse)
library(monochromeR)
library(ggtext)
library(sf)
library(mapview)
library(scales)


# Load this week's data
tuesdata <- tidytuesdayR::tt_load(2022, week = 47)

museums <- tuesdata$museums


# Looks like there is larger subject matter group at the beginning of each subject
# Let's grab that


subjects <- museums %>%
  mutate(subject = str_extract(string = Subject_Matter, pattern = "^\\w+")) %>%
  group_by(subject) %>%
  summarise(subject_count = n()) %>%
  arrange(desc(subject_count))
# Okay the 5 most common museum subjects are:
# 1. Local Histories
# 2. Buildings
# 3. Wars and conflict
# 4. Arts
# 5. Transport


### Let's build a map

# First get a map of the UK
uk <- map_data(map = "world", region = "UK")

# There is an outlier that is really far north, gotta drop that [figured out this issue from @npechl on Github]
# Also, filter on the top 5 most common subjects

clean_museums <- museums %>% filter(Latitude <= max(uk$lat)) %>%
  mutate(subject = str_extract(string = Subject_Matter, pattern = "^\\w+")) %>%
  filter(subject == "Local_Histories" | subject == "Buildings" | subject == "War_and_conflict" |
  subject == "Arts" | subject == "Transport")

# Set some of our aesthetics

font <- "Calibri"

# Make the names look nicer in the legend
legend_names <- list(War_and_conflict = "3. War and Conflict",
                     Arts = "4. Arts",
                     Local_Histories = "1. Local Histories",
                     Transport = "5. Transport",
                     Buildings = "2. Buildings")

# Assign the colors
museum_colors <- list(War_and_conflict = "#53191b",
                      Arts = "#8d7b8f",
                      Local_Histories = "#cfa88b",
                      Transport = "#3c8481",
                      Buildings = "#a26115")

view_palette(museum_colors)

secondary_text <- generate_palette("#242126", modification = "go_lighter", n_colours = 4)[1]

view_palette(secondary_text)

# Plot it!

museum_plot <- ggplot() +
  geom_polygon(data = uk, aes(x = long, y = lat, group = group)) +
  geom_point(data = clean_museums, aes(x = Longitude, y = Latitude, color = subject)) +
  coord_map() +
  theme_minimal() +
  labs(title = paste0("Throughout the UK's 4,000+ Museums,<br> <span style=\"color:",
                      museum_colors$Local_Histories, "\">Local Histories</span> is the most common subject."),
       subtitle = "We've mapped the 5 most common museum subjects below.",
       caption = "\nDataviz by @katzemily \nSource: Mapping Museums project, #TidyTuesday Week 47") +
  theme(axis.text = element_blank(),
        text = element_text(size = 14,
                            family = font,
                            color = secondary_text),
        plot.title = element_markdown(size = 18,
                                  family = font,
                                  face = "bold",
                                  color = "#242126",
                                  margin = margin(5,0,5,0)),
        plot.subtitle = element_text(size = 11),
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


# Save it out
ggsave("tidytuesday_11.22.22.jpg", plot = museum_plot,
       width = 5, height = 7, units = "in")

