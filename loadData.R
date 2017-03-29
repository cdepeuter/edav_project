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


num_responses <- table(pew$respid)



# how many people each year
pew_by_year <- pew %>% group_by(year) %>% summarise(responses = n())

# plot
ggplot(pew_by_year, aes(x=year)) + geom_line(aes(y=responses)) + ggtitle("Number of responses each year") 


# who completely agrees with gods existence

pew$completely_agree_god <- pew$q16d == pew$q16d[1]

god_by_state <- pew  %>% dplyr::group_by(state) %>% dplyr::summarise(value = sum(completely_agree_god, na.rm = TRUE) / n())
god_by_state$region <-state_name[god_by_state$state]
state_choropleth(god_by_state) + ggtitle("Who is going to hell")

# pct born again christians throughout time
born_again_throughout_time <- pew %>% group_by(year) %>% summarise(pct_born_again = sum(born == "Yes", na.rm=TRUE) / n())
ggplot(born_again_throughout_time) + geom_line(aes(x=year, y=pct_born_again))




# plots for writeup
by_religion <- data.frame(table(pew$relign)[order(table(pew$relign), decreasing = TRUE)])



not_other <-by_religion$Var1  != "Other Religion"

by_religion <- rbind(by_religion[not_other,], by_religion[by_religion$Var1  == "Other Religion",])


# make V1 an ordered factor
by_religion$V1 <- factor(by_religion$Var1, levels = by_religion$Var1)
by_religion$V1 <- factor(by_religion$Var1, levels = levels(by_religion$V1)[length(by_religion$Var1):1])



ggplot(by_religion) + geom_bar(aes(x=V1, y=Freq), stat='identity') + coord_flip() + ggtitle("Religions in Pew data") + xlab("Religion") + ylab("Count")


# racism by year
whos_racist <- pew %>% group_by(year, q10k) %>% summarise(count = n())

whos_racist_start_end <- whos_racist[whos_racist$year == 1987 | whos_racist$year == 2012, ]
whos_racist_start_end <- whos_racist_start_end[1:10,]

year_sum <- data.frame(whos_racist_start_end %>% group_by(year) %>% summarise(tot = sum(count)))
rownames(year_sum) <- year_sum$year
to_divide <- year_sum[as.character(whos_racist_start_end$year),"tot"]
whos_racist_start_end$count <- whos_racist_start_end$count / to_divide


ggplot(whos_racist_start_end) + geom_bar(aes(x=q10k, y=count), stat='identity') + facet_grid(~year)+xlab("Response") + ylab("%")+ggtitle("I think it's all right for blacks and whites to date each other ")


