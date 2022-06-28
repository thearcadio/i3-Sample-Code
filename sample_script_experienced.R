####
## i3 Computational Thinking and Tools Module
## Intro to R for students with experience
## Sample Script: lesson help from https://datacarpentry.org/r-socialsci/
## Laura W. Dozal 6/28/2022
####

library(tidyverse)
# install.packages('here')
library(here)

interviews <- read_csv(here("SAFI_clean.csv"), na = "NULL")
interviews
glimpse(interviews)

# Note that read_csv() actually loads the data as a tibble. 
# A tibble is an extension of R data frames used by the tidyverse. 
# When the data is read using read_csv(), it is stored in an object of 
# class tbl_df, tbl, and data.frame. You can see the class of an object with
class(interviews)

######Indexing and Subsetting #########

## first element in the first column of the tibble
interviews[1, 1]

## first element in the 6th column of the tibble 
interviews[1, 6]

## first column of the tibble (as a vector)
interviews[[1]]

## first column of the tibble
interviews[1]

## first three elements in the 7th column of the tibble
interviews[1:3, 7]

## the 3rd row of the tibble
interviews[3, ]

## equivalent to head_interviews <- head(interviews)
head_interviews <- interviews[1:6, ]

# The whole tibble, except the first column
interviews[, -1]         



##### Data Wrangling with dplyr and tidyr ########
##
# We're going to learn some of the most common dplyr functions:
#   
# select(): subset columns
# filter(): subset rows on conditions
# mutate(): create new columns by using information from other columns
# group_by() and summarize(): create summary statistics on grouped data
# arrange(): sort results
# count(): count discrete values
##

# Selecting Columns and filtering rows
# now we'll use the select() function to pull data by name

# to select columns throughout the dataframe
select(interviews, village, no_membrs, months_lack_food)

# to do the same thing with subsetting
interviews[c("village","no_membrs","months_lack_food")]

# to select a series of connected columns
select(interviews, village:respondent_wall_type)

###
# To choose rows based on specific criteria, we can use the filter() function.

# filters observations where village name is "Chirodzo" 
filter(interviews, village == "Chirodzo")

# We can also specify multiple conditions within the filter() function. 
# We can combine conditions using either "and" or "or" statements. In an 
# "and" statement, an observation (row) must meet every criteria to be included 
# in the resulting dataframe. To form "and" statements within dplyr, we can pass 
# our desired conditions as arguments in the filter() function, separated by commas:

# filters observations with "and" operator (comma) similar to & operator
# output dataframe satisfies ALL specified conditions
filter(interviews, village == "Chirodzo", 
       rooms > 1, 
       no_meals > 2)

# filters observations with "|" logical operator
# output dataframe satisfies AT LEAST ONE of the specified conditions
filter(interviews, village == "Chirodzo" | village == "Ruaca")

## Temporary data frames and Pipes
interviews2 <- filter(interviews, village == "Chirodzo")
interviews_ch <- select(filter(interviews, village == "Chirodzo"),
                        village:respondent_wall_type)

# Pipes let you take the output of one function and send it directly to the next, 
#which is useful when you need to do many things to the same dataset. 
interviews %>%
  filter(village == "Chirodzo") %>%
  select(village:respondent_wall_type)

interviews_ch <- interviews %>%
  filter(village == "Chirodzo") %>%
  select(village:respondent_wall_type)

interviews_ch

## Mutate
# Frequently you'll want to create new columns based on the values in existing columns, 
# for example to do unit conversions, or to find the ratio of values in two columns.

interviews %>%
  mutate(people_per_room = no_membrs / rooms)

# To ignore cases, we could insert a filter() in the chain:
interviews %>%
  filter(!is.na(memb_assoc)) %>%
  mutate(people_per_room = no_membrs / rooms)


# The summarize() function
# group_by() is often used together with summarize(), which collapses each group 
# into a single-row summary of that group. group_by() takes as arguments the column 
# names that contain the categorical variables for which you want to calculate the 
# summary statistics. So to compute the average household size by village:

interviews %>%
  group_by(village) %>%
  summarize(mean_no_membrs = mean(no_membrs))

interviews %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs))

# When grouping both by village and membr_assoc, we see rows in our table for 
# respondents who did not specify whether they were a member of an irrigation 
# association. We can exclude those data from our table using a filter step.

interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs))
 
# Once the data are grouped, you can also summarize multiple variables at the same 
# time (and not necessarily on the same variable). For instance, we could add a column 
# indicating the minimum household size for each village for each group (members of 
# an irrigation association vs not):

interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs))

## arrange the members by desc:
interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarize(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs)) %>%
  arrange(desc(min_membrs))

## count
interviews %>%
  count(village, sort = TRUE)

#### Long and Wide Formats ####
# In the interviews data, each row contains the values of variables associated with 
# each record collected (each interview in the villages), where it is stated that the 
# key_ID was "added to provide a unique Id for each observation" and the instance_ID 
# "does this as well but it is not as convenient to use."
# 
# However, with some inspection, we notice that there are more than one row in the 
# dataset with the same key_ID (as seen below). However, the instanceIDs associated 
# with these duplicate key_IDs are not the same. Thus, we should think of instanceID 
# as the unique identifier for observations!


interviews %>%
  filter(village == "Chirodzo") %>%
  select(key_ID, village, interview_date, instanceID) %>%
  sample_n(size = 10)

# As seen in the code, for each interview date in each village no instanceIDs 
# are the same. Thus, this format is what is called a "long" data format, where each 
# observation occupies only one row in the dataframe.

## Back to Slides

# Once we we've created this new table, we can explore the relationship within and 
# between villages. The key point here is that we are still following a tidy data structure, 
# but we have reshaped the data according to the observations of interest.
# 
# Alternatively, if the interview dates were spread across multiple columns, and we were 
# interested in visualizing, within each village, how irrigation conflicts have changed over time. 
# This would require for the interview date to be included in a single column rather than spread 
# across multiple columns. Thus, we would need to transform the column names into values of a variable.


## Pivot Wider
interviews_wide <- interviews %>%
  mutate(wall_type_logical = TRUE) %>%
  pivot_wider(names_from = respondent_wall_type,
              values_from = wall_type_logical,
              values_fill = list(wall_type_logical = FALSE))

## Pivot Longer
interviews_long <- interviews_wide %>%
  pivot_longer(cols = c(burntbricks, cement, muddaub, sunbricks),
               names_to = "respondent_wall_type",
               values_to = "wall_type_logical") %>%
  filter(wall_type_logical) %>%
  select(-wall_type_logical)

#### Clean the Data with Pivots ####

interviews_items_owned <- interviews %>%
  separate_rows(items_owned, sep = ";") %>%
  replace_na(list(items_owned = "no_listed_items")) %>%
  mutate(items_owned_logical = TRUE) %>%
  pivot_wider(names_from = items_owned,
              values_from = items_owned_logical,
              values_fill = list(items_owned_logical = FALSE))

nrow(interviews_items_owned)

## Code discussed:

## Then we use the new function separate_rows() from the tidyr package to separate 
# the values of items_owned based on the presence of semi-colons (;). 
#The values of # this variable were multiple items separated by semi-colons, so 
# this action creates  a row for each item listed in a household's possession. 
# Thus, we end up with a long format version of the dataset, with multiple rows for each respondent

## We can use the replace_na() function to change these NA values to something more meaningful. 
# 
# The replace_na() function expects for you to give it a list() of columns that you 
# would like to replace the NA values in, and the value that you would like to replace the NAs

## Lastly, we use pivot_wider() to switch from long format to wide format. This 
# creates a new column for each of the unique values in the items_owned column, and 
# fills those columns with the values of items_owned_logical. We also declare that 
# for items that are missing, we want to fill those cells with the value of FALSE instead of NA.

##### extra pivoting #####
interviews_items_owned %>%
  filter(bicycle) %>%
  group_by(village) %>%
  count(bicycle)

interviews_items_owned %>%
  mutate(number_items = rowSums(select(., bicycle:car))) %>%
  group_by(village) %>%
  summarize(mean_items = mean(number_items))


#### Export our Data for Plotting ####

# write_csv (interviews_plotting, file = "data_output/interviews_plotting.csv")

interviews_plotting <- interviews %>%
  ## pivot wider by items_owned
  separate_rows(items_owned, sep = ";") %>%
  ## if there were no items listed, changing NA to no_listed_items
  replace_na(list(items_owned = "no_listed_items")) %>%
  mutate(items_owned_logical = TRUE) %>%
  pivot_wider(names_from = items_owned,
              values_from = items_owned_logical,
              values_fill = list(items_owned_logical = FALSE)) %>%
  ## pivot wider by months_lack_food
  separate_rows(months_lack_food, sep = ";") %>%
  mutate(months_lack_food_logical = TRUE) %>%
  pivot_wider(names_from = months_lack_food,
              values_from = months_lack_food_logical,
              values_fill = list(months_lack_food_logical = FALSE)) %>%
  ## add some summary columns
  mutate(number_months_lack_food = rowSums(select(., Jan:May))) %>%
  mutate(number_items = rowSums(select(., bicycle:car)))

write_csv (interviews_plotting, file = "data_output/interviews_plotting.csv")

## Read-in data if just beginning
interviews_plotting <- read_csv("data_output/interviews_plotting.csv")


### GGPLOT2 ####

# Assign plot to a variable
interviews_plot <- interviews_plotting %>%
  ggplot(aes(x = no_membrs, y = number_items))

# Draw the plot as a dot plot
interviews_plot +
  geom_point() #add points to show our continuous variables

# lets change the color because there's probably overlapping data
interviews_plot <- interviews_plotting %>%
  ggplot(aes(x = no_membrs, y = number_items)) +
  geom_point(alpha = 0.5)

## try fixing it with geom_jitter()
interviews_plot <- interviews_plotting %>%
  ggplot(aes(x = no_membrs, y = number_items)) +
  geom_jitter()

# The geom_jitter() function allows for us to specify the amount of random 
# motion in the jitter, using the width and height arguments.
# and we can add color!

interviews_plotting %>%
  ggplot(aes(x = no_membrs, y = number_items)) +
  geom_jitter(alpha = 0.5,
              color = "blue",
              width = 0.2,
              height = 0.2)

# now let's add color based on village
interviews_plotting %>%
  ggplot(aes(x = no_membrs, y = number_items)) +
  geom_jitter(aes(color = village), alpha = 0.5, width = 0.2, height = 0.2)

# now lets change our dot size based on count
interviews_plotting %>% 
  ggplot(aes(x = no_membrs, y = number_items, color = village)) +
  geom_count()

## Boxplots
# we can inclde some of the same configurations, just change our geom_point to geom_boxplot
# A boxplot is a graph that gives you a good indication of how the values in the data are spread out.
# more info here: https://towardsdatascience.com/understanding-boxplots-5e2df7bcbd51

interviews_plotting %>%
  ggplot(aes(x = respondent_wall_type, y = rooms)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.5,
              color = "tomato",
              width = 0.2,
              height = 0.2)


## Bar Plot

#stacked
interviews_plotting %>%
  ggplot(aes(x = respondent_wall_type)) +
  geom_bar(aes(fill = village))


#grouped
interviews_plotting %>%
  ggplot(aes(x = respondent_wall_type)) +
  geom_bar(aes(fill = village), position = "dodge")

#### proportions for bar charts! ####

percent_wall_type <- interviews_plotting %>%
  filter(respondent_wall_type != "cement") %>%
  count(village, respondent_wall_type) %>%
  group_by(village) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  ungroup()

percent_wall_type %>%
  ggplot(aes(x = village, y = percent, fill = respondent_wall_type)) +
  geom_bar(stat = "identity", position = "dodge")



## Labels and Titles
percent_wall_type %>%
  ggplot(aes(x = village, y = percent, fill = respondent_wall_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of wall type by village",
       fill = "Type of Wall in Home", 
       x = "Village",
       y = "Percent")

## Seperate categories (Faceting)
percent_wall_type %>%
  ggplot(aes(x = respondent_wall_type, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title="Proportion of wall type by village",
       x="Wall Type",
       y="Percent") +
  facet_wrap(~ village) +
  theme_bw() + # implement black and white grid theme
  theme(panel.grid = element_blank()) # change the background grid

## more theme options: https://ggplot2.tidyverse.org/reference/ggtheme.html

# What if we wanted to see the proportion of respondents in each village who 
# owned a particular item? We can calculate the percent of people in each village 
# who own each item and then create a faceted series of bar plots where each plot
# is a particular item. First we need to calculate the percentage of people in each 
# village who own each item:

percent_items <- interviews_plotting %>% 
  group_by(village) %>%
  summarize(across(bicycle:no_listed_items, ~ sum(.x) / n() * 100)) %>% 
  pivot_longer(bicycle:no_listed_items, names_to = "items", values_to = "percent")

# summarize and plot facet_grid() of items
percent_items %>%
  ggplot(aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  theme_bw() +
  theme(panel.grid = element_blank())

#customize
percent_items %>%
  ggplot(aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  labs(title = "Percent of respondents in each village \n who owned each item",
       x = "Village",
       y = "Percent of Respondents") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 45,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text = element_text(size = 16))


#### Save Plot ####
my_plot <- percent_items %>%
  ggplot(aes(x = village, y = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ items) +
  labs(title = "Percent of respondents in each village \n who owned each item",
       x = "Village",
       y = "Percent of Respondents") +
  theme_bw() +
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 45,
                                   hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(color = "grey20", size = 12),
        text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5))

ggsave("fig_output/name_of_file.png", my_plot, width = 15, height = 10)