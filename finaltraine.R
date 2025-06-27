

#################################################################################
#################################################################################
#################################################################################

library(ollamar)
library(dplyr)
install.packages("progress")
library(progress)
# Read data
data <- read.csv("location.csv", header = TRUE)
names(data)
# Functions needed
apply_prompt <- function(text, model , prompt, stream = F){
  resp <- generate(model, prompt = text, system = prompt, stream = stream, temperature = 0)
  resp_process(resp, "text")
}
split_answer <- function(response_test){
  parts <- strsplit(response_test, "</think>\n\n")[[1]]
  thinking <- gsub("^<think>\n*", "", parts[1])
  answer <- parts[2]
  return(list(thinking = thinking, answer = answer))
}

model_list <- c("deepseek-coder:6.7b", "llama3.2:latest" )

for (model_used in model_list) {
  cat("\n\n=== Running workflow for model:", model_used, "===\n\n")
  test_connection()
  
  # Read the original data fresh for each model iteration
  data <- read.csv("location.csv", header = TRUE)
  
  # Initialize new columns with the correct length
  data$activity <- character(nrow(data))
  data$explicit.location.indicator.chat <- character(nrow(data))
  data$explicit.location.chat <- character(nrow(data))
  data$explicit.people.indicator.chat <- character(nrow(data))
  data$explicit.name.indicator.chat <- character(nrow(data))
  data$explicit.names.chat <- character(nrow(data))
  data$explicit.people.relation.chat <- character(nrow(data))
  descriptions <- as.list(data$Description)
  
  
#################     Extracting Location       #########################
pb <- progress_bar$new(
    format = "  Activity extraction [:bar] :percent eta: :eta",
    total = length(descriptions), clear = FALSE, width = 60
  )
#activity
  for(i in seq_along(descriptions)) {
    description_text <- descriptions[[i]]
    description_name <- names(descriptions)[i]
    prompt_activity <- paste0("We provide you with a 'Description' containing a partcipant's qualitative description of their current activity. 
    It reads like this, Description: ", description_text, ".
    You are interested in identifying the activity of the participant.
  
    'Which activity is the participant engaging in?'
  
    Output Instructions:  
    If no activity is described, provide 'not reported'. 
    The output should have the following format where you insert your answer:
    'Activity: [insert answer]'"
    )
    activity <- apply_prompt(text = description_text,
                                     model = model_used,
                                     prompt = prompt_activity,
                                     stream =TRUE)
    activity_components <- strsplit(location_present, "Activity: ")[[1]]
    activity_components[2]
    
    target_row <- which(data$Description == description_text)
    data$activity[target_row] <- activity_components[2]
    cat("Finished", description_name, "\n")
    pb$tick()
  }
  
  
  
  
  
  
  
  
  
  
  
  
# Main loop for explicit.location.indicator.chat
for(i in seq_along(descriptions)) {
  description_text <- descriptions[[i]]
  description_name <- names(descriptions)[i]
  prompt_location_1 <- paste0("We provide you with a 'Description' containing a partcipant's qualitative description of their current activity. 
  It reads like this, Description: ", description_text, ".
  You are interested in identifying whether the 'Description' contains the description of a location.
  
  Can you answer the question: 'Where did the activity take place?'
  
  Output Instructions:  
  Your answer can only be 'Yes'  or 'No',  nothing else. 
  The output should have the following format where you insert your answer:
  'Location Indicator: [insert answer: 'Yes' OR 'No']'"
  )
  location_present <- apply_prompt(text = description_text,
                                   model = model_used,
                                   prompt = prompt_location_1,
                                   stream =TRUE)
  location_components <- strsplit(location_present, "Location Indicator: ")[[1]]
  location_components[2]
  
  target_row <- which(data$Description == description_text)
  data$explicit.location.indicator.chat[target_row] <- location_components[2]
  cat("Finished", description_name, "\n")
}


#the actual location
descriptions <- as.list(data$Description[data$explicit.location.indicator.chat == "Yes"])
for(i in seq_along(descriptions)) {
  description_text <- descriptions[[i]]
  description_name <- names(descriptions)[i]
  prompt_location_1 <- paste0("We provide you with a 'Description' containing a partcipant's qualitative description of their current activity. 
  It reads like this, Description: ", description_text, ".
  You are interested in identifying where the activity took place.
  
  'Where did the activity take place?'
  
  Output Instructions:  
  Your 'answer' should be a location (or multiple locations) described in the 'Description', nothing else. 
  Multiple locations should be seperated by a comma: ','
  'Location : 'Your answer' '"
  )
  location <- apply_prompt(text = description_text,
                           model = model_used,
                           prompt = prompt_location_1,
                           stream =TRUE)
  location_fi_components <- strsplit(location, "Location : ")[[1]]
  location_fi_components[2]
  
  target_row <- which(data$Description == description_text)
  data$explicit.location.chat[target_row] <- location_fi_components[2]
  cat("Finished", description_name, "\n")
}
#for the company 
#explicit.names.chat <- integer(80)
#explicit.people.relation.chat <- integer(80)

for(i in seq_along(descriptions)) {
  description_text <- descriptions[[i]]
  description_name <- names(descriptions)[i]
  prompt_people_1 <- paste0("We provide you with a 'Description' containing a partcipant's qualitative description of their current activity. 
  It reads like this, Description: ", description_text, ".
  You are interested in whether the 'Description' contains the description of other people than the author themself. 
  
  Can you answer the question: 'Were other people than the author present during the activity?'
  If the participant explicitly states they were alone, you can answer 'alone'. 
  
  Output Instructions:  
  Unless your answer is 'alone', your answer can only be 'Yes'  or 'No',  nothing else. 
  The output should have the following format where you insert your answer:
  'People Indicator: 'Yes' OR 'No' OR 'Alone'"
  )
  people <- apply_prompt(
    text = description_text,
    model = model_used,
    prompt = prompt_people_1,
    stream = TRUE
  )
  people_components <- strsplit(people, "People Indicator: ")[[1]]
  people_indicator <- if (length(people_components) > 1) trimws(people_components[2]) else NA
  target_row <- which(data$Description == description_text)
  if (length(target_row) > 0) {
    data$explicit.people.indicator.chat[target_row] <- people_indicator
  }
  cat("Finished", description_name, "\n")
}

#Are names mentioned in the text?
descriptions2 <- as.list(data$Description[data$explicit.people.indicator.chat == "Yes"])
for(i in seq_along(descriptions2)) {
  description_text <- descriptions2[[i]]
  description_name <- names(descriptions2)[i]
  prompt_people_2 <- paste0("We provide you with a 'Description' containing a partcipant's qualitative description of their current activity. 
  It reads like this, Description: ", description_text, ".
  You have previously indicated the 'Description' contains the description of other people than the author themself. 
  
  Can you answer the question: 'What are the names of the other people present during the activity?'. 
  Only answer 'Yes' if you can mention the names of the people described. 
  
  Output Instruction: 
  Your answer can only be 'Yes' or 'No', nothing else. 
  The output should have the following format where you insert your answer:
  'Name Indicator: 'Yes' OR 'No''"
  )
  people2 <- apply_prompt(text = description_text,
                          model = model_used,
                          prompt = prompt_people_2,
                          stream =TRUE)
  people_ti_components <- strsplit(people2, "Name Indicator: ")[[1]]
  people_ti_components[2]
  
  target_row <- which(data$Description == description_text)
  data$explicit.name.indicator.chat[target_row] <- people_ti_components[2]
  cat("Finished", description_name, "\n")
}

#Names of those people
descriptions2 <- as.list(data$Description[data$explicit.name.indicator.chat == "Yes"])
for(i in seq_along(descriptions2)) {
  description_text <- descriptions2[[i]]
  description_name <- names(descriptions2)[i]
  prompt_people_3 <- paste0("We provide you with a 'Description' containing a partcipant's qualitative description of their current activity.
  It reads like this, Description: ", description_text, ".
  You have previously indicated the 'Description' contains the description of other people than the author themself and their names.
  You are interested in identifying the names of the other people/person described. 
 
  Answer the question: 'What are the names of the other people described?'
  
  Output Instructions: 
  Your answer should be a name or multiple names described in the 'Description', nothing else. 
  Multiple names should be seperated by comma ','. 
  'Names: 'answer' '"
  )
  people2 <- apply_prompt(text = description_text,
                          model = model_used,
                          prompt = prompt_people_3,
                          stream =TRUE)
  people_ti_components <- strsplit(people2, "Names: ")[[1]]
  people_ti_components[2]
  
  target_row <- which(data$Description == description_text)
  data$explicit.names.chat[target_row] <- people_ti_components[2]
  cat("Finished", description_name, "\n")
}

descriptions2 <- as.list(data$Description[data$explicit.name.indicator.chat == "Yes"])
for(i in seq_along(descriptions2)) {
  description_text <- descriptions2[[i]]
  description_name <- names(descriptions2)[i]
  prompt_people_4 <- paste0("We provide you with a 'Description' containing a partcipant's qualitative description of their current activity.
  It reads like this, Description: ", description_text, ".
  You have previously indicated the 'Description' contains the description of other people than the author themself and their names. In addition, you have extracted the following names: ",people_ti_components[2],".
  You are interested in identifying the relationship between the author and the person associated with the extracted names.  
 
  Answer the question: 'What is the relationship of the author to the person associated with each of the names you have extracted from the Description?'
  
  Output Instructions: 
  Your answer should be a relationship label, nothing else. 
  Multiple relationship labels should be seperated by comma ','. 
  If you cannot identify the relationship of the author to the person associated with the name, answer 'not provided'.
  Your final answer should contain the same amount of answers as the intitally provided list of names. 
  'Relationship Label: 'answer' '"
  )
  people2 <- apply_prompt(text = description_text,
                          model = model_used,
                          prompt = prompt_people_4,
                          stream =TRUE)
  people_ti_components <- strsplit(people2, "Relationship Label: ")[[1]]
  people_ti_components[2]
  
  target_row <- which(data$Description == description_text)
  data$explicit.people.relation.chat[target_row] <- people_ti_components[2]
  cat("Finished", description_name, "\n")
}

model_file_tag <- gsub("[:]", "_", model_used)
write.csv(data, paste0("location_results_", model_file_tag, ".csv"), row.names = FALSE)

cat("Saved results for", model_used, "\n")
}



###################################################
data <- read.csv("location_results_deepseek-coder_6.7b.csv", header = TRUE)
clean_yes_no <- function(x) {
  x_clean <- gsub("[\\[\\]'\" ]", "", x)
  x_clean <- toupper(x_clean)
  result <- ifelse(x_clean == "YES", "Yes",
                   ifelse(x_clean == "NO", "No", NA))
  return(result)
}
data$explicit.location.indicator.chat.clean <- clean_yes_no(x)
data$explicit.location.indicator.chat.clean <- ifelse(data$explicit.location.indicator.chat.clean == "Yes", 1, 0)

data$explicit.people.indicator.chat.clean <- clean_yes_no(x)
data$explicit.people.indicator.chat.clean <- ifelse(data$explicit.people.indicator.chat.clean == "Yes", 1, 0)

data$explicit.name.indicator.chat.clean <- clean_yes_no(x)
data$explicit.name.indicator.chat.clean <- ifelse(data$explicit.name.indicator.chat.clean == "Yes", 1, 0)
