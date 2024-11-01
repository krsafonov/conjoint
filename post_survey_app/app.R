library(shiny)
library(shinyWidgets)
library(shinysurveys)
library(googledrive)
library(googlesheets4)

#once
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "conjoint894@gmail.com")
# gs4_auth(token = drive_token())

#user_id <- "6666"
#sheet_name <- paste("", user_id, sep="_")

# googlesheets4::gs4_create(name = "Response_sheet", 
#                          sheets = "main")
# Get the ID of the sheet for writing programmatically
# This should be placed at the top of your shiny app
# sheet_id <- googledrive::drive_get(sheet_name)$id

# Define questions in the format of a shinysurvey
survey_questions <- data.frame(
  question = c(rep("Ваш возраст:", 1),
               rep("На какой программе вы сейчас учитесь:", 6),
               rep("На каком курсе вы сейчас учитесь:", 4),
               rep("Пол:", 2) ,
               rep('Назовите ваш опыт работы:', 6), 
               rep('Назовите ваш текущий статус:', 3), 
               rep('Назовите ваш текущий заработок после налогообложения (в месяц, в тысячах рублей):', 10), 
               rep('Назовите индустрию, в которой вы работаете:', 19) #dependent on status - only if works
  ),
  
  option = c( NA,
              'Бакалавр экономики (BAE)', 'Магистр экономики (MAE)', 'Экономика и анализ данных', 'Финансы, инвестиции, банки (MAF)', 'Мастер наук по финансам (MSF)', 'Мастер финансов (MiF)', 
              '1 курс', '2 курс', '3 курс', '4 курс', 
              'Мужской', 'Женский', 
              'Нет опыта', 'До 1 года','От 1 года до 3 лет','От 3 до 5 лет','Более 5 лет','Предпочитаю не отвечать',
              'Работаю','Не работаю', 'Предпочитаю не отвечать',
              'Нет постоянного заработка','<50','50-75','75-100','100-125','125-150','150-175','175-200','>200','Предпочитаю не отвечать',
              'Финансовые услуги','Индустрия общественного питания (HoReCa)','Технологии','Здравоохранение','Образование','Строительство', 'Производство',
              'Медиа и развлечения','Логистика','Розничная торговля','Товары народного потребления','Энергетика', 'Горнодобыча',
              'Сельское хозяйство', 'Консалтинг', 'Некоммерческий сектор (Non-profit)', 'Другое', 'Предпочитаю не отвечать',
              NA
  ),
  
  input_type = c('text', rep('mc', 49), 'text'),
  
  input_id = c('age',
               rep('program', 6),
               rep('course', 4),
               rep('sex', 2),
               rep('experience', 6),
               rep('status', 3),
               rep('income', 10),
               rep('industry', 18),
               'dif_industry'), 
  
  dependence = c(rep(NA, 32),
                 rep('status', 18),
                 'industry'), 
  
  dependence_value = c(rep(NA, 32), rep('Работаю', 18), 'Другое'),
  
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
  
  if (!dir.exists("surveys")) {
    dir.create("surveys")
  }
  
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
      по ссылке  https://milarsenteva.shinyapps.io/post_survey_app/?user_id=... , где вместо троеточия напишите свой номер."
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
      по ссылке  https://milarsenteva.shinyapps.io/post_survey_app/?user_id=... , где вместо троеточия напишите свой номер.",
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
