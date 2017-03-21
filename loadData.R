library(foreign)
library(choroplethr)
library(dplyr)
library(ggplot2)




# get the names and abbreviations from the state object
state_name <- tolower(state.name)
state_abb <- state.abb


# DC is included in the data, so add a mapping for that so it doesnt show NA, even though
# its too small to really show on the map
state_name <- c(state_name[1:7], "district of columbia", state_name[8:length(state_name)])
state_abb <- c(state_abb[1:7], "DC", state_abb[8:length(state_abb)])


# create mapping
names(state_name) <- state_abb



pew <- read.spss("1987-2012_Values_Merge/1987-2012 Values Merge public.sav", to.data.frame=TRUE)




# how many people in each state
pew_by_state <- pew  %>% dplyr::group_by(state) %>% dplyr::summarise(value = n())

state_choropleth(pew_by_state) + ggtitle("Number resposnes")

# how many people each year
pew_by_year <- pew %>% group_by(year) %>% summarise(value = n())


num_responses <- table(pew$respid)

# plot
ggplot(pew_by_year, aes(x=year)) + geom_line(aes(y=num))


# who completely agrees with gods existence

pew$completely_agree_god <- pew$q16d == pew$q16d[1]

god_by_state <- pew  %>% dplyr::group_by(state) %>% dplyr::summarise(value = sum(completely_agree_god, na.rm = TRUE) / n())
god_by_state$region <-state_name[god_by_state$state]
state_choropleth(god_by_state) + ggtitle("Who is going to hell")

# pct born again christians throughout time
born_again_throughout_time <- pew %>% group_by(year) %>% summarise(pct_born_again = sum(born == "Yes", na.rm=TRUE) / n())
ggplot(born_again_throughout_time) + geom_line(aes(x=year, y=pct_born_again))

