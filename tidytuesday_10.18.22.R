
# First attempt at Tidy Tuesday

# install.packages("tidytuesdayR")

library(tidytuesdayR)
library(tidyverse)
library(monochromeR)
library(ggtext)

# Download this weeks data
tuesdata <- tidytuesdayR::tt_load(2022, week = 42)

episodes <- tuesdata$episodes

dialogue <- tuesdata$stranger_things_all_dialogue

# First, need to combine the datasets

merged <- dialogue %>% left_join(episodes, by = c("season","episode"))

# Yay! This worked.

# Ideas for the data viz:
# Number of times "Eleven" is said per episode?
# Number of times something is done over all seasons by writer
# something about steve and nancy?


# Establish a color scheme.(Thanks to Cara Thompson's Level Up Your Plots talk for help with my colors!)

stranger_colors <- list("Nancy" = "#5ca3af",
                        "Steve" = "#37526e",
                        "red" = "#632827")

# Build on it for text colors.

dark_text <- monochromeR::generate_palette(stranger_colors$Steve, "go_darker",
                                           n_colours = 2)[2]

light_text <- monochromeR::generate_palette(dark_text, "go_lighter",
                                            n_colours = 3)[2]

stranger_colors <- list("Nancy" = "#5ca3af",
                        "Steve" = "#37526e",
                        "red" = "#632827",
                        "dark_text" = dark_text,
                        "light_text" = light_text)



# Take a look at the palette we created
monochromeR::view_palette(stranger_colors)

# Let's look at the data a bit.

glimpse(merged)


# Let's add a vairable that combines both season and episode
merged <- merged %>% mutate(
  season_episode = paste0(season,"_",episode))


# Now let's add a variable that counts the number of times "Nancy" is said and the number of times "Steve" is said
merged <- merged %>% mutate(
  nancy_count = str_count(merged$dialogue, "Nancy"),
  steve_count = str_count(merged$dialogue, "Steve")
)

# Now, let's group by season_episode and get a sum of Nancy's and Steve's per episode
summary_df <- merged %>%
  group_by(season_episode, season) %>%
  summarise(Nancy = sum(nancy_count, na.rm = TRUE),
            Steve = sum(steve_count, na.rm = TRUE))


# Reshape the data so we can plot it better
summary_long <- summary_df %>%
  pivot_longer(cols = c("Nancy", "Steve"),
               names_to = "sum_type",
               values_to = "name_count")

# Make a line graph with one line for Nancy and one for Steve throughout the episodes
ggplot(summary_long, aes(x = season_episode, y = name_count, color = sum_type)) +
  geom_point() + # Adds the points
  geom_line(aes(group = sum_type)) + # Adds a line for each sum_type (i.e., Steve & Nancy)
  labs(title = paste0("Number of times <span style=\"color:", stranger_colors$Nancy,"\">Nancy</span>
                      & <span style=\"color:", stranger_colors$Steve, "\">Steve's</span> names are said<br> fluctuate significantly across episodes."),
       subtitle = "It's hard to say who is more popular.",
       y = "# of Times Name is Said",
       x = "Season & Episode") + # Adds a title
  theme_minimal(base_size = 12) + # Makes the background lighter
  theme(text = element_text(family = "DM Sans", color = stranger_colors$light_text),
        plot.title = element_markdown(size = 18, family = "DM Sans", color = stranger_colors$dark_text, face = "bold"),
        axis.text = element_text(size = 6),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("Nancy" = "#5ca3af",
                                "Steve" = "#37526e")) + # Manual change of colors
  guides(color = guide_legend(title = "Characters")) # Changes the legend title


# Trying again with different colors
ggplot(summary_long, aes(x = season_episode, y = name_count, color = sum_type)) +
  geom_point() + # Adds the points
  geom_line(aes(group = sum_type)) + # Adds a line for each sum_type (i.e., Steve & Nancy)
  labs(title = paste0("Number of times <span style=\"color:", stranger_colors$Nancy,"\">Nancy</span>
                      & <span style=\"color:", stranger_colors$red, "\">Steve's</span> names are said<br> fluctuate significantly across episodes."),
       subtitle = "It's hard to say who is more popular.",
       y = "# of Times Name is Said",
       x = "Season & Episode") + # Adds a title
  theme_minimal(base_size = 12) + # Makes the background lighter
  theme(text = element_text(family = "DM Sans", color = stranger_colors$light_text),
        plot.title = element_markdown(size = 18, family = "DM Sans", color = "white", face = "bold"),
        axis.text = element_text(size = 6),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_color_manual(values = c("Nancy" = "#5ca3af",
                                "Steve" = "#632827")) + # Manual change of colors
  guides(color = guide_legend(title = "Characters")) # Changes the legend title



# Can't get it work this way.
scale_colour_manual(values = stranger_colors, limits = force)
























