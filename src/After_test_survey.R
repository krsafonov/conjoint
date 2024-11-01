library(shiny)
library(shinysurveys)
library(googledrive)
library(googlesheets4)

# googledrive::drive_auth()
# googlesheets4::gs4_auth()

#once
options(gargle_oauth_cache = ".secrets")
gs4_auth(cache = ".secrets", email = "conjoint894@gmail.com")
drive_auth(cache = ".secrets", email = "conjoint894@gmail.com")

# googlesheets4::gs4_create(name = "Response_sheet", sheets = "main")

# Get the ID of the sheet for writing programmatically
# This should be placed at the top of your shiny app
sheet_id <- googledrive::drive_get("Response_sheet")$id

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

# Define shiny UI
ui <- fluidPage(
  surveyOutput(survey_questions,
               survey_title = "Personal info",
               survey_description = "Please submit your personal information")
)

# Define shiny server
server <- function(input, output, session) {
  renderSurvey()
  
  observeEvent(input$submit, {
    response_data <- getSurveyData()
    
    # Read our sheet
    values <- googlesheets4::read_sheet(ss = sheet_id, 
                                        sheet = "main")
    
    # Check to see if our sheet has any existing data.
    # If not, let's write to it and set up column names. 
    # Otherwise, let's append to it.
    
    if (nrow(values) == 0) {
      googlesheets4::sheet_write(data = response_data,
                                 ss = sheet_id,
                                 sheet = "main")
    } else {
      googlesheets4::sheet_append(data = response_data,
                                  ss = sheet_id,
                                  sheet = "main")
    }
    
    
  })
  
}

# Run the shiny application
shinyApp(ui, server)