library(shiny)
library(shinyWidgets)
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
  question = c(rep("На каком курсе вы сейчас учитесь:", 5),
               rep("Пол:", 3) ,
               rep('Назовите ваш опыт работы:', 6), 
               rep('Назовите ваш текущий статус:', 3), 
               rep('Назовите ваш текущий заработок после налогообложения:', 10), 
               rep('Назовите индустрию, в которой вы работаете:', 19) #dependent on status - only if works
  ),
  
  option = c( '1', '2', '3', '4', 'Предпочитаю не отвечать',
              'Мужской', 'Женский', 'Предпочитаю не отвечать',
              'Нет опыта', 'До 1 года','От 1 года до 3 лет','От 3 до 5 лет','Более 5 лет','Предпочитаю не отвечать',
              'Работаю','Не работаю', 'Предпочитаю не отвечать',
              'Нет постоянного заработка','<50k','50-75k','75-100k','100-125k','125-150k','150-175k','175-200k','>200k','Предпочитаю не отвечать',
              'Финансовые услуги','Индустрия общественного питания (HoReCa)','Технологии','Здравоохранение','Образование','Строительство', 'Производство',
              'Медиа и развлечения','Логистика','Розничная торговля','Товары народного потребления','Энергетика', 'Горнодобыча',
              'Сельское хозяйство', 'Консалтинг', 'Некоммерческий сектор (Non-profit)', 'Другое', 'Предпочитаю не отвечать',
              NA
  ),
  
  input_type = c(rep('mc', 45), 'text'),
  
  input_id = c(rep('course', 5),
               rep('sex', 3),
               rep('experience', 6),
               rep('status', 3),
               rep('income', 10),
               rep('industry', 18),
               'dif_industry'), 
  
  dependence = c(rep(NA, 27),
                 rep('status', 18),
                 'industry'), 
  
  dependence_value = c(rep(NA, 27), rep('Работаю', 18), 'Другое'),
  
  required = TRUE
)

survey_page <- div(
  div(
    h1("Расскажите о себе", style = "text-align: center; font-size: 250%; "), 
    p("Приветствуем вас в заключительной части нашего опроса! Основной блок, связанный с выявлением предпочтений gen Z на рынке труда, закончен и теперь мы хотим узнать о вас чуть больше. Ваши ответы помогут нам значительно улучшить качество нашего исследования. Помните, что данные собираются анонимно и будут анализироваться в агрегированной форме. Благодарим за участие!", style = "text-align: center; font-size: 120%; width: 60%; margin: 0 auto"),
    surveyOutput(survey_questions, style = " font-size: 100%;") 
  )
)


jscode <- "Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = 'https://www.nes.ru';});"
# Define shiny UI
# Define shiny UI
ui <- fluidPage(
  style = "background-color: ghostwhite;",
  tags$head(tags$script(jscode)),
  textOutput("user_id"),
  survey_page
)

# Define shiny server
server <- function(input, output, session) {
  data.dir <- "surveys"
  
  renderSurvey()
  
  user_id <- as.integer(Sys.time())
  
  output$user_id <- renderText({
    query <-parseQueryString(session$clientData$url_search)
    user_id <- query[['user_id']]
    if (!is.null(user_id)) {
      paste("User ID:", user_id)
    } else {
      "Мы потеряли по пути ваш уникальный идентификатор. Если Вы сохранили его на прошлом этапе, то перейдите
      по ссылке https://krsafonov.shinyapps.io/post_survey_app/?user_id=... , где вместо троеточия напишите свой номер."
    }
  })
  
  observeEvent(input$confirm, {
    stopApp()
  })
  
  observeEvent(input$submit, {
    response_data <- getSurveyData()
    
    query <- parseQueryString(session$clientData$url_search)
    user_id <- query[['user_id']]
    
    if (is.null(user_id)) {
      showModal(modalDialog(
        title = "Уппс",
        "Мы потеряли по пути ваш уникальный идентификатор. Если Вы сохранили его на прошлом этапе, то перейдите
      по ссылке https://krsafonov.shinyapps.io/post_survey_app/?user_id=... , где вместо троеточия напишите свой номер.",
        footer = actionButton("confirm", "Close")
      ))
    } else {
      
      numname <- sprintf("%s_num_data.txt", user_id)
      
      utils::write.table(
        x = response_data,
        file = file.path(data.dir, numname), 
        row.names = TRUE, quote = FALSE, sep = "\t", col.names = NA
      )
      drive_upload(file.path(data.dir, numname),
                   file.path("surveys", numname)
      )
      
      showModal(modalDialog(
        title = "Спасибо за прохождение опроса!",
        "Ваши ответы были сохранены.",
        footer = actionButton("confirm", "Close")
      ))
      
      #session$sendCustomMessage("mymessage", "msg")
    }
  })
}

# Run the shiny application
shinyApp(ui, server)
