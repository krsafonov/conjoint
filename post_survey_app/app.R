library(shiny)
library(shinysurveys)
library(googledrive)
library(googlesheets4)

#once
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "conjoint894@gmail.com")
# gs4_auth(token = drive_token())

user_id <- "6666"
sheet_name <- paste("", user_id, sep="_")

# googlesheets4::gs4_create(name = "Response_sheet", 
#                          sheets = "main")
# Get the ID of the sheet for writing programmatically
# This should be placed at the top of your shiny app
# sheet_id <- googledrive::drive_get(sheet_name)$id

# Define questions in the format of a shinysurvey
survey_questions <- data.frame(
  question = c(rep("На каком курсе вы сейчас учитесь:", 4),
               rep("Пол:", 2) ,
               rep('Назовите ваш опыт работы:', 5), 
               rep('Назовите ваш текущий статус:', 2), 
               rep('Назовите ваш текущий заработок:', 6), 
               rep('Назовите индустрию, в которой вы работаете:', 9) #dependent on status - only if works
               ),
  
  option = c( '1', '2', '3', '4',
              'мужской', 'женский',
              'нет опыта', 'до 1 года','от 1 года до 3 лет','от 3 до 5 лет','более 5 лет',
              'работаю','не работаю',
              'нет постоянного заработка','<50','50-100','100-150','150-200','>200',
              'IT','банкинг','retail','финансы','консалтинг','образование','сельское хозяйство','другое',
              NA
  ),
  input_type = c(rep('mc', 27), 'text'),
  
  input_id = c(rep('course', 4),
               rep('sex', 2),
               rep('experience', 5),
               rep('status', 2),
               rep('income', 6),
               rep('industry', 8),
               'dif_industry'), 
  
  dependence = c(rep(NA, 19),
                 rep('status', 8),
                 'industry'), 
                 
  dependence_value = c(rep(NA, 19), rep('работаю', 8), 'другое'),
  
  required = TRUE
)

survey_page <- div(
  surveyOutput(survey_questions,
               survey_title = "Personal info",
               survey_description = "Please submit your personal information")
)

# Define shiny UI
ui <- fluidPage(
  survey_page
)

# Define shiny server
server <- function(input, output, session) {
  data.dir <- "surveys"
  
  renderSurvey()
  
  user_id <- as.integer(Sys.time())
  
  observeEvent(input$submit, {
    response_data <- getSurveyData()
    
    query <- parseQueryString(session$clientData$url_search)
    user_id <- query[['user_id']]
    
    print(user_id)
    numname <- sprintf("%s_num_data.txt", user_id)
    print(numname)
    
    
    utils::write.table(
      x = response_data,
      file = file.path(data.dir, numname), 
      row.names = TRUE, quote = FALSE, sep = "\t", col.names = NA
    )
    drive_upload(file.path(data.dir, numname),
                 file.path("surveys", numname)
    )
    stopApp()
  })
}

# Run the shiny application
shinyApp(ui, server)

