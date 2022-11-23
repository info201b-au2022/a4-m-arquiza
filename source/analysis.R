library(tidyverse)
library(dplyr)
library(ggplot2)
library(usmap)

#The functions might be useful for A4
#source("a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Gets race by year, race by state, and gender trends
#----------------------------------------------------------------------------#
incarc_trends <- read.csv("~/info201/data/incarceration_trends.csv")
summary_info <- list()
 
# average populations of different race groups in and out of jail per year
race_year_ratios <- incarc_trends %>%
  select(year,
         aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop,
         aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64, white_pop_15to64) %>%
  group_by(year) %>%
  summarise(mean_aapi_jail_pop = mean(aapi_jail_pop, na.rm = TRUE),
            mean_black_jail_pop = mean(black_jail_pop, na.rm = TRUE),
            mean_latinx_jail_pop = mean(latinx_jail_pop, na.rm = TRUE),
            mean_native_jail_pop = mean(native_jail_pop, na.rm = TRUE),
            mean_white_jail_pop = mean(white_jail_pop, na.rm = TRUE),
            mean_aapi_nonjail_pop = mean(aapi_pop_15to64, na.rm = TRUE),
            mean_black_nonjail_pop = mean(black_pop_15to64, na.rm = TRUE),
            mean_latinx_nonjail_pop = mean(latinx_pop_15to64, na.rm = TRUE),
            mean_native_nonjail_pop = mean(native_pop_15to64, na.rm = TRUE),
            mean_white_nonjail_pop = mean(white_pop_15to64, na.rm = TRUE))

# average populations of different race groups in and out of jail per state in 2018
current_raceprops_perstate <- incarc_trends %>%
  filter(year == max(year)) %>%
  select(state,
         aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop,
         aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64, white_pop_15to64) %>%
  group_by(state) %>%
  summarise(mean_aapi_jail_pop = mean(aapi_jail_pop, na.rm = TRUE),
            mean_black_jail_pop = mean(black_jail_pop, na.rm = TRUE),
            mean_latinx_jail_pop = mean(latinx_jail_pop, na.rm = TRUE),
            mean_native_jail_pop = mean(native_jail_pop, na.rm = TRUE),
            mean_white_jail_pop = mean(white_jail_pop, na.rm = TRUE),
            mean_aapi_nonjail_pop = mean(aapi_pop_15to64, na.rm = TRUE),
            mean_black_nonjail_pop = mean(black_pop_15to64, na.rm = TRUE),
            mean_latinx_nonjail_pop = mean(latinx_pop_15to64, na.rm = TRUE),
            mean_native_nonjail_pop = mean(native_pop_15to64, na.rm = TRUE),
            mean_white_nonjail_pop = mean(white_pop_15to64, na.rm = TRUE))

# ratio of male to females in jail per year per state (too many 100%+? disregard?)
gender_props_peryear_perstate <- incarc_trends %>%
  mutate(total_female_jail_prop = female_jail_pop / total_jail_pop,
         total_male_jail_prop = male_jail_pop / total_jail_pop) %>%
  select(year, state, total_female_jail_prop, total_male_jail_prop) %>%
  filter(!is.na(total_female_jail_prop)) %>%
  filter(!is.na(total_male_jail_prop)) %>%
  group_by(year, state) %>%
  summarise(mean_female_pop = mean(total_female_jail_prop),
            mean_male_pop = mean(total_male_jail_prop)) %>%
  distinct()


#summary info calculating; race populations for 2018, for max race get state,
#                          ratio of male to female in 2018?

summary_info$max_year_aapi_pop <- race_year_ratios %>%
  filter(year == max(year)) %>%
  summarise(ratio_aapi = mean_aapi_jail_pop/mean_aapi_nonjail_pop) %>%
  pull(ratio_aapi)
summary_info$max_year_aapi_pop <- round(as.numeric(summary_info$max_year_aapi_pop)*100, 2)

summary_info$max_year_black_pop <- race_year_ratios %>%
  filter(year == max(year)) %>%
  summarise(ratio_black = mean_black_jail_pop/mean_black_nonjail_pop) %>%
  pull(ratio_black)
summary_info$max_year_black_pop <- round(as.numeric(summary_info$max_year_black_pop)*100, 2)

summary_info$max_year_latinx_pop <- race_year_ratios %>%
  filter(year == max(year)) %>%
  summarise(ratio_latinx = mean_latinx_jail_pop/mean_latinx_nonjail_pop) %>%
  pull(ratio_latinx)
summary_info$max_year_latinx_pop <- round(as.numeric(summary_info$max_year_latinx_pop)*100, 2)

summary_info$max_year_native_pop <- race_year_ratios %>%
  filter(year == max(year)) %>%
  summarise(ratio_native = mean_native_jail_pop/mean_native_nonjail_pop) %>%
  pull(ratio_native)
summary_info$max_year_native_pop <- round(as.numeric(summary_info$max_year_native_pop)*100, 2)

summary_info$max_year_white_pop <- race_year_ratios %>%
  filter(year == max(year)) %>%
  summarise(ratio_white = mean_white_jail_pop/mean_white_nonjail_pop) %>%
  pull(ratio_white)
summary_info$max_year_white_pop <- round(as.numeric(summary_info$max_year_white_pop)*100, 2)

sum_names <- c("max_year_aapi_pop", "max_year_black_pop","max_year_latinx_pop",
               "max_year_native_pop", "max_year_white_pop")
ratio_name <- sum_names[unlist(summary_info, use.names = FALSE) == max(unlist(summary_info, use.names = FALSE))]

if(str_detect(ratio_name, "aapi")){
  summary_info$max_ratio_location <- current_raceprops_perstate %>%
    filter(mean_aapi_jail_pop == max(mean_aapi_jail_pop, na.rm = TRUE)) %>%
    pull(state)
  summary_info$max_ratio_name <- "AAPI"
}else if(str_detect(ratio_name, "black")){
  summary_info$max_ratio_location <- current_raceprops_perstate %>%
    filter(mean_black_jail_pop == max(mean_black_jail_pop, na.rm = TRUE)) %>%
    pull(state)
  summary_info$max_ratio_name <- "Black"
}else if(str_detect(ratio_name, "latinx")){
  summary_info$max_ratio_location <- current_raceprops_perstate %>%
    filter(mean_latinx_jail_pop == max(mean_latinx_jail_pop, na.rm = TRUE)) %>%
    pull(state)
  summary_info$max_ratio_name <- "Latinx"
}else if(str_detect(ratio_name, "native")){
  summary_info$max_ratio_location <- current_raceprops_perstate %>%
    filter(mean_native_jail_pop == max(mean_native_jail_pop, na.rm = TRUE)) %>%
    pull(state)
  summary_info$max_ratio_name <- "Native American"
}else{
  summary_info$max_ratio_location <- current_raceprops_perstate %>%
    filter(mean_white_jail_pop == max(mean_white_jail_pop, na.rm = TRUE)) %>%
    pull(state)
  summary_info$max_ratio_name <- "AAPI"
}

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function filters, selects, and sums the total population of national
# jail population per year. Returns a dataframe of values.
get_year_jail_pop <- function() {
  df <- incarc_trends %>%
    select(year, total_jail_pop)
return(df)
}

# This function creates a chart of total population per year using a dataframe
# of values. Returns a barchart.
plot_jail_pop_for_us <- function()  {
  pl <- ggplot(
    data = get_year_jail_pop(),
    aes(x=year, y=total_jail_pop) 
  ) + geom_bar(stat="identity")+
    labs(x = "Years", y = "Total Jail Population", 
         title = "Increase of Jail Population in U.S (1970-2018)",
         caption = "The US prison population has increased since 1980, peaked around
         2010, and has continued on a slight downward trend since.")+
    scale_y_continuous(labels = scales::comma) +
    theme(plot.title = element_text(hjust = 0))
  return(pl)
}
## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
#----------------------------------------------------------------------------#

get_jail_pop_by_states <- function(states) {
  df <- incarc_trends %>%
    select(year, state, total_jail_pop) %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    group_by(year) %>%
    spread(state, total_jail_pop)
  return(df)
}

plot_jail_pop_by_states <- function(states) {
  if(length(states) > 10 || length(states) < 3){
    print("invalid number of states")
    return()
  }
  
  df <- get_jail_pop_by_states(states)

  colors <- c("red", "blue", "green", "purple", "orange", "pink", "yellow", 
              "cadetblue", "darkblue", "forestgreen")
    pl <- ggplot(data = df, aes(x = year)) + 
    labs(x = "Years", y = "Total Jail Population",
         title = "U.S. Jail population from 1970 to 2018 (per state)",
         caption = "US. Jail populations shown per state to emphasize differences
                    in jail population when related to population size of states.") +
    theme(plot.title = element_text(hjust = 0))
  for(i in 2:ncol(df)){
    pl <- pl + geom_line(aes_string(y = names(df)[i],  color = shQuote(colors[i])))
  }
  pl <- pl + scale_color_discrete(name = "State", labels = sort(states))
  return(pl)
}

## Section 5  ----
#----------------------------------------------------------------------------#
# Inequality Variable Comparison
#----------------------------------------------------------------------------#

# Accidentally made noncontinuous for one variable, keep just because I spent a lot of work on it ;(
get_race_pops <- function() {
  df <- incarc_trends %>%
    select(aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop,
           aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64, white_pop_15to64) %>%
    summarise(aapi_ratio = aapi_jail_pop/aapi_pop_15to64,
              black_ratio = black_jail_pop/black_pop_15to64,
              latinx_ratio = latinx_jail_pop/latinx_pop_15to64,
              native_ratio = native_jail_pop/native_pop_15to64,
              white_ratio = white_jail_pop/white_pop_15to64) %>%
    rename("AAPI" = aapi_ratio,
           "Black" = black_ratio,
           "Latinx" = latinx_ratio,
           "Native" = native_ratio,
           "White" = white_ratio)
  is.na(df)<-sapply(df, is.infinite)
  df[is.na(df)]<-0
  return(df)
}

plot_race_pops <- function() {
  df <- get_race_pops() %>%
    gather(type, ratio) %>%
    filter(ratio != 0)

  pl <- ggplot() + geom_point(data = df, aes(x = type, y = ratio, color = type)) + 
    labs(x = "Racial Type", y = "Ratio of Incarcerated Population to General Population",
         title = "Incarceration Ratios of Different Racial Types",
         color = "Racial Type",
         caption = ".") +
    theme(plot.title = element_text(hjust = 0))
  return(pl)
}



get_gender_pops <- function(){
  df <- incarc_trends %>%
  filter(year == max(year)) %>%
  select(female_adult_jail_pop, male_adult_jail_pop) %>%
  filter(male_adult_jail_pop < 10000) #filter out outlier  
  return(df)
}

plot_gender_pops <- function(){
  df = get_gender_pops()
  pl <- ggplot(data = df, aes(x = female_adult_jail_pop, y = male_adult_jail_pop)) +
    geom_point() + geom_smooth(method = "lm") +
    labs(x = "Female Incarcerated Population", y = "Male Incarcerated Population",
         title = "Variable Comparison of Incarcerated Genders (2018)",
         caption = "Men, on average, underwent a larger jailed population growth than women.")+
    theme(plot.title = element_text(hjust = 0))
  return(pl)
}

## Section 6  ----
#----------------------------------------------------------------------------#
# Map of the Incarcerated Black Population
#----------------------------------------------------------------------------#
get_black_ratio <- function(){
  df <- incarc_trends %>%
  filter(year == max(year)) %>%
  select(state, black_jail_pop, total_pop) %>%
  mutate(black_prop = black_jail_pop/total_pop * 100) %>%
  group_by(state) %>%
  summarise(black_prop = mean(black_prop, na.rm = TRUE))
  
  is.na(df)<-sapply(df, is.nan)
  df[is.na(df)]<-0
  return(df)
}

plot_black_ratio <- function(){
  df <- get_black_ratio() %>% filter(black_prop != is.na(black_prop))

  pl <- plot_usmap(region = "state", data = df, values = "black_prop") +
    scale_fill_continuous(na.value = "gray48", 
                          low = "white", 
                          high = "blue",
                          name = "Percentage Black People in Prison") +
    labs(title = "Black Prison Population Ratio per State (2018)",
         caption = "Lousiana has the highest percentage of incarcerated black population
         compared to the rest of the population.")
  return(pl)
}