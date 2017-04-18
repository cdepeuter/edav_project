library(foreign)
library(choroplethr)

#library(tidyr)
library(tidyverse)
library(ggplot2)
library(viridis)
library(choroplethrMaps)


library(HH)
library(shinyLikert)
# get the names and abbreviations from the state object
state_name <- tolower(state.name)
state_abb <- state.abb


# DC is included in the data, so add a mapping for that so it doesnt show NA, even though
# its too small to really show on the map
state_name <- c(state_name[1:7], "district of columbia", state_name[8:length(state_name)])
state_abb <- c(state_abb[1:7], "DC", state_abb[8:length(state_abb)])


# create mapping
names(state_name) <- state_abb



pew <- read.spss("data/1987-2012_Values_Merge_public.sav", to.data.frame=TRUE)
religions <- unique(pew$relig)

# convert answers to integers
answer_vals <- c(1,-2,2,-1,0,NA_integer_)
names(answer_vals) <- unique(pew$q16a)




getFilteredData <- function(data, years, question, religions){
    
    valid_years_ <- seq(years[1], years[2])
    #print(valid_years_)
    #print(question)
    
    data <- data[data$year %in% valid_years_,]
    data <- data[data$relig %in% religions,]
    
    
    filtered_data <- customQuestionFilter(data, question)
    
    return(filtered_data)
}



likertData <- function(data,  years, question, religions){
    print(years)
    valid_years_ <- seq(years[1], years[2])
    #print(valid_years_)
    #print(question)
    
    data <- data[data$year %in% valid_years_,]
    data <- data[data$relig %in% religions,]
    
    x <- data %>% dplyr::select(respid, year, state, q16a,sex, age, race, relig) %>% filter(!(is.na(q16a))) %>% filter(!(is.na(relig)))
    x2 <- x %>% dplyr::select(relig, q16a) %>% group_by(relig, q16a) %>% dplyr::summarize(n()) %>% spread(key = q16a, value = `n()`) %>% filter(relig!="DK/Ref")
    x2
    x2 <- x2 %>% dplyr::select(-DK) #removing DK variable
    x2 <- x2[c(1,5,4,3,2)] #rearrange so responses go in right order
    
    
    return(x2) 
}




getQuestionInputs <- function(){
    qs <- list( 
             other=c( "School boards ought to have the right to fire teachers who are known homosexuals","Follow politics",
                     "We all will be called before God at the Judgment Day to answer for our sins",
             "Nazis deserve freedom of speech", "I have old fashioned views", "Women should return to their traditional roles in society",
             "We should restrict and control people coming into our country to live more than we do now"), 
             Religion = c( "I never doubt the existence of God", "Prayer is an important part of my daily life" ,  "AIDS is God's punishment for immoral behiavior",
                           "Even today miracles are performed by the power of God"), 
             
             Race = c( "I think it's all right for blacks and whites to date each other", "Discriminations against blacks are rare today", 
                       "We should make every possible effort to improve the position of blacks and other minorities, even if it means giving them preferential treatment",
                       "In the past few years there hasn't been much real improvement in the position of black people in this country",
            "Not much in common with other races")
             )
    
    return(qs)
}

customQuestionFilter <- function(data, question){

    if(question == "I never doubt the existence of God"){
        data$answer <- as.character(data$q16d) 
        
    }else if(question == "Prayer is an important part of my daily life"){
        data$answer <- as.character(data$q16a)
        
    }else if(question == "We all will be called before God at the Judgment Day to answer for our sins"){
        data$answer <- as.character(data$q16b)
    }
    else if(question == "We should restrict and control people coming into our country to live more than we do now"){
        data$answer <- as.character(data$q10n)
    }else if(question == "Not much in common with other races"){
       data$answer <- as.character(data$q16s)
       
    }else if(question == "Follow politics"){
        data$answer <- as.character(data$q16x) 
        
    }else if(question == "I think it's all right for blacks and whites to date each other"){
        data$answer <- as.character(data$q10k) 
    }else if(question == "In the past few years there hasn't been much real improvement in the position of black people in this country"){
        data$answer <- as.character(data$q10j) 
    } else if(question == "Discriminations against blacks are rare today"){
        data$answer <- as.character(data$q10m)   
    } else if(question == "We should make every possible effort to improve the position of blacks and other minorities, even if it means giving them preferential treatment"){
        data$answer <- as.character(data$q10l)        
    } else if(question == "Nazis deserve freedom of speech"){
        data$answer <- as.character(data$q16h)   
    } else if(question == "AIDS is God's punishment for immoral behiavior"){
        data$answer <- as.character(data$q16l) 
    } else if(question == "I have old fashioned views"){
        data$answer <- as.character(data$q16m) 
    } else if(question == "Women should return to their traditional roles in society"){
        data$answer <- as.character(data$q16j)
    } else if(question == "School boards ought to have the right to fire teachers who are known homosexuals"){
        data$answer <- as.character(data$q16e)
    }
    
    
    data_ <- data %>% group_by(state) %>% summarize(value = sum(answer_vals[answer], na.rm = TRUE) / sum(!is.na(answer)), count = n())
    data_$region <- state_name[data_$state]
    print(data.frame(data_))
    print(typeof(data_$region))
    print(typeof(data_$value))
    print(dim(data))
    return(data_)
}


#function to bin ages
binage <- function(agevec){
    newvec <- vector(mode="character", length=length(agevec))
    ops <- c("18-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76+")
    newvec[18 <= agevec & agevec <= 25] <- ops[1]
    newvec[25 < agevec & agevec <= 35] <- ops[2]
    newvec[35 < agevec & agevec <= 45] <- ops[3]
    newvec[45 < agevec & agevec <= 55] <- ops[4]
    newvec[55 < agevec & agevec <= 65] <- ops[5]
    newvec[65 < agevec & agevec <= 75] <- ops[6]
    newvec[75 < agevec] <- ops[7]
    return(newvec)
}



#function to bin ages
binage <- function(agevec){
    newvec <- vector(mode="character", length=length(agevec))
    ops <- c("18-25", "26-35", "36-45", "46-55", "56-65", "66-75", "76+")
    newvec[18 <= agevec & agevec <= 25] <- ops[1]
    newvec[25 < agevec & agevec <= 35] <- ops[2]
    newvec[35 < agevec & agevec <= 45] <- ops[3]
    newvec[45 < agevec & agevec <= 55] <- ops[4]
    newvec[55 < agevec & agevec <= 65] <- ops[5]
    newvec[65 < agevec & agevec <= 75] <- ops[6]
    newvec[75 < agevec] <- ops[7]
    return(newvec)
}

getLikertData <- function(data_){
    x <- data_ %>% dplyr::select(respid, year, state, q16a, q16b,sex, age, race, relig) %>% filter(!(is.na(q16a))) %>% filter(!(is.na(relig)))
    x2 <- x %>% dplyr::select(relig, q16a) %>% group_by(relig, q16a) %>% dplyr::summarize(n()) %>% spread(key = q16a, value = `n()`) %>% filter(relig!="DK/Ref")
    x2
    x2 <- x2 %>% dplyr::select(-DK) #removing DK variable
    x2 <- x2[c(1,5,4,3,2)] #rearrange so responses go in right order
    return(x2) 
}
##q16a


question.txt <- "Prayer is an Important Part of my Daily Life"

lk_data <- getLikertData(pew)

#plot.likert(relig~., data=lk_data, as.percent = T, ylab="religion", main= paste("Responses to:", question.txt))
# 
# ##gotta bin the ages
# ###and then separate out based on religion to see how much age effects prayer by religion
# x3 <- x %>% dplyr::select(age, q16a) %>% filter(!is.na(age)) %>% mutate(age_bin = binage(age)) %>% dplyr::select(-age) %>% group_by(age_bin, q16a) %>% dplyr::summarize(n()) %>% spread(key = q16a, value = `n()`)
# x3 <- x3 %>% dplyr::select(-DK) #removing DK variable
# x3
# x3 <- x3[c(1,5,4,3,2)] #rearrange so responses go in right order
# 
# t <- plot.likert(age_bin~., data=x3, as.percent = T, ylab="age", main= paste("Responses to:", question.txt))
# 
# 
