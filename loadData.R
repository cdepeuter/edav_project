library(foreign)

pew <- read.spss("1987-2012_Values_Merge/1987-2012 Values Merge public.sav", to.data.frame=TRUE)


# how many people in each state
pew_by_state <- pew  %>% group_by(state) %>% summarise(people = n())

# how many people each year
pew_by_year <- pew %>% group_by(year) %>% summarise(num = n())

# plot
ggplot(pew_by_year, aes(x=year)) + geom_line(aes(y=num))