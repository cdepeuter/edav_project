getFilteredData <- function(data, years, question){
    
    valid_years_ <- seq(years[1], years[2])
    #print(valid_years_)
    #print(question)
    
    data <- data[data$year %in% valid_years_,]
    
    filtered_data <- customQuestionFilter(data, question)
    
    return(filtered_data)
}


getQuestionInputs <- function(){
    qs <- c("Believe in God", "Daily prayer important", "Not much in common with other races", "Follow politics", "Technology complicating")
    
    return(qs)
}

customQuestionFilter <- function(data, question){
    print(paste("qs ", question))
    if(question == "Believe in God"){
        
        data$completely_agree_god <- data$q16d == "Completely agree"
        
        data_ <- data  %>% dplyr::group_by(state) %>% dplyr::summarise(value = sum(completely_agree_god, na.rm = TRUE) / n())
        
        data_$region <-state_name[data_$state]
        
    }else if(question == "Daily prayer important"){
        data$completely_agree_prayer <- data$q16a == "Completely agree"
        
        data_ <- data  %>% dplyr::group_by(state) %>% dplyr::summarise(value = sum(completely_agree_prayer, na.rm = TRUE) / n())
        
        data_$region <-state_name[data_$state]
    }else if(question == "Not much in common with other races"){
       
    }else if(question == "Follow politics"){
        data$politics <- data$q16x == "Completely agree"
        
        data_ <- data  %>% dplyr::group_by(state) %>% dplyr::summarise(value = sum(politics, na.rm = TRUE) / n())
        
        data_$region <-state_name[data_$state]
    }else if(question == "Technology complicating"){
        data$tech <- data$q16x == "Completely agree"
        
        data_ <- data  %>% dplyr::group_by(state) %>% dplyr::summarise(value = sum(tech, na.rm = TRUE) / n())
        
        data_$region <-state_name[data_$state]
    }
    
    return(data_)
}