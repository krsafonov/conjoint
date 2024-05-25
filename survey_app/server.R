library(shiny)
library("googledrive")
library(DT)
library(bslib)

#_____HINTS

rowCallback <- c(
  "function(row, data, num, index){",
  '
  hints = {
    "Зарплата": "",
    "Full-time": "Пятидневка с 9 до 17. Работать необходимо оффлайн в офисе",
    "Гибрид": "2-3 дня удаленной работы, остальные дни рабочей недели необходимо лично присутствовать в офисе",
    "Формальная":"Строгая иерархия, отстраненное и не эмапатичное начальство, общение на “Вы” внутри коллектива, активный мониторинг сотрудников, строгий дресс-код. Пример: Роснефть",
    "Полуформальная":"Комфортная рабочая атмосфера, одновременно предполагающая определенные границы, связанные со статусом сотрудника; нейтральное начальство; классический дресс-код без строгих ограничений. Пример: Сбербанк",
    "Неформальная":"Низкая иерархия и развитые горизонтальные связи, вдохновляющее начальство, индивидуальный подход к каждому сотруднику, общение на “ты” внутри коллектива, минимальный мониторинг сотрудников или его полное отсутствие, требования о дресс-коде отсутствуют. Примеры: Тинькофф, Яндекс",
    "Современный":"Новый современный ремонт, оборудованные зоны отдыха, фитнес-зал, развитая инфраструктура вокруг офиса",
    "Классический":"Классический ремонт в хорошем состоянии, менее удобная планировка рабочего пространства, столовая для сотрудников",
    "Неблагоустроенный / устаревший":"Ремонт не обновлялся много лет, бедная инфраструктура как внутри офиса, так и вокруг него",
    "В течение 1 года":"Перспективы роста и развития внутри компании в течение 1 года",
    "В течение 3 лет":"Перспективы роста и развития внутри компании в течение 3 лет",
    "В течение 5 лет":"Перспективы роста и развития внутри компании в течение 5 лет",
    "20%":"20% вашего рабочего времени уделяется на выполнение задач, которые вы считаете неинтересными / скучными",
    "50%":"50% вашего рабочего времени уделяется на выполнение задач, которые вы считаете неинтересными / скучными",
    "70%":"70% вашего рабочего времени уделяется на выполнение задач, которые вы считаете неинтересными / скучными",
  }
  ',
  "  $('td:eq(0)', row).attr('title', hints[data[0]]).attr('data-toggle', 'tooltip').attr('data-placement', 'right').attr('data-html', 'true');",
  "  $('td:eq(1)', row).attr('title', hints[data[1]]).attr('data-toggle', 'tooltip').attr('data-placement', 'right').attr('data-html', 'true');",
  "  $('td:eq(2)', row).attr('title', hints[data[2]]).attr('data-toggle', 'tooltip').attr('data-placement', 'right').attr('data-html', 'true');",
  "  $('td:eq(3)', row).attr('title', hints[data[3]]).attr('data-toggle', 'tooltip').attr('data-placement', 'right').attr('data-html', 'true');",
  
  "}"  
)

initComplete <- "
function () {
  $('[data-toggle=tooltip]').tooltip();
}
"

function(input, output, session) {
  des = NULL
  alt.cte = NULL
  no.choice = NULL
  data.dir = NULL
  c.lvls = NULL
  prior.mean = NULL
  prior.covar = NULL
  cand.set = NULL
  n.draws = NULL
  lower = NULL
  upper = NULL
  parallel = TRUE
  reduce = TRUE
  
  des = exp$design
  n.total = n.sets
  alts = alternatives
  atts = attributes
  lvl.names = labels
  coding = code
  alt.cte = alt
  no.choice = 4
  buttons.text = b.text
  intro.text = i.text
  end.text = e.text
  c.lvls = list(c(90, 110, 140, 190), c(10, 30, 60))
  data.dir = "./responds/"
  
  user_id = substr(session$token, 1, 8)
  
  n.atts <- length(atts)
  n.alts <- length(alts)
  n.cte <- sum(alt.cte)
  
  Altspec <- function(alt.cte, n.sets) {
    if(!any(alt.cte == 0)){
      stop("'alt.cte' should at least contain 1 zero")
    }
    mat <- diag(length(alt.cte))
    n.zero <- which(alt.cte == 0)
    mat[n.zero, n.zero] <- 0
    del.col <- c(which(apply(mat, 2, function(x) all(x == 0))))
    mat <- mat[, -del.col]
    mat <- as.matrix(mat)
    cte.mat <- do.call(rbind, replicate(n.sets, mat, simplify = FALSE))
    return(cte.mat)
  }
  
  n.init <- nrow(des)/n.alts
  
  cte.des <- Altspec(alt.cte = alt.cte, n.sets = n.init)
  
  bs <- seq(1, (nrow(des) - n.alts + 1), n.alts)
  es <- c((bs - 1), nrow(des))[-1]
  
  Rcnames <- function(n.sets, n.alts, alt.cte, no.choice) {
    r.s <- rep(1:n.sets, each = n.alts)
    r.a <- rep(1:n.alts, n.sets)
    r.names <- paste(paste("set", r.s, sep = ""), paste("alt", r.a, sep = ""), sep = ".")
    if(no.choice){
      ncsek <- seq(n.alts, (n.sets * n.alts), n.alts)
      r.names[ncsek] <- "no.choice"
    }
    if(sum(alt.cte) > 0.2){
      cte.names <- paste(paste("alt", which(alt.cte == 1), sep = ""), ".cte", sep = "")
    } else {
      cte.names <- NULL
    }
    return(list(r.names, cte.names))
  }
  
  rowcol <- Rcnames(n.sets = n.init, n.alts = n.alts, alt.cte = alt.cte, no.choice = FALSE)
  rownames(des) <- rowcol[[1]]
  
  fulldes <- des
  
  n.atts <- length(atts)
  sdata <- vector(mode = "list")
  surveyData <- vector(mode = "list")
  y.bin <- vector("numeric")
  resp <- vector("character")
  fieldsMandatory <- c("survey")
  
  Charbin <- function(resp, alts, n.alts, no.choice = FALSE) {
    if (!all(resp %in% alts)) {
      stop("1 or more responses do not match the possible response options.")
    }
    if (length(alts) != (n.alts + no.choice)) {
      stop("Number of response options is not correct")
    }
    map <- match(resp, alts)
    l <- list()
    for(i in 1:length(map)){
      l[[i]] <- rep(0, n.alts)
      if (no.choice) {
        l[[i]][map[i] - 1] <- 1
      } else {
        l[[i]][map[i]] <- 1
      }
    }
    v <- unlist(l)
    return(v)
  }
  
  saveData <- function(data, data.dir, n.atts) {
    options(gargle_oauth_cache = ".secrets")
    drive_auth(cache = ".secrets", email = "conjoint894@gmail.com")
    
    d <- as.data.frame(cbind(data$design, resp = data$bin.responses))
    unc_resp <- rep(data$responses, each = n.atts)
    unc_setnr <- rep(1:length(data$responses), each = n.atts)
    unc_d <- cbind(set = unc_setnr, data$survey, resp = unc_resp, time_spent = data$time_spent)
    
    numname <- sprintf("%s_num_data.txt", user_id)
    charname <- sprintf("%s_char_data.txt", user_id)
    
    utils::write.table(
      x = d,
      file = file.path(data.dir, numname),
      row.names = TRUE, quote = FALSE, sep = "\t", col.names = NA
    )
    utils::write.table(
      x = unc_d,
      file = file.path(data.dir, charname),
      row.names = TRUE, quote = FALSE, sep = "\t", col.names = NA
    )
    drive_upload(file.path(data.dir, numname),
                 file.path("responds", numname)
    )
    drive_upload(file.path(data.dir, charname),
                 file.path("responds", charname)
    )
  }
  
  LoadData <- function(data.dir, type) {
    if(!type %in% c("num", "char")){
      stop("'type' must be either num or char")
    }
    if (!dir.exists(data.dir)) {
      stop("Directory 'data.dir' does not exist")
    }
    error <- character(0)
    if(identical(list.files(data.dir, full.names = TRUE, pattern = type), error)){
      stop("No files of the specified 'type' in 'data.dir'")
    }
    files <- list.files(data.dir, full.names = TRUE, pattern = type)
    data <- lapply(files, utils::read.table, stringsAsFactors = FALSE, sep = '\t', header = TRUE)
    ncols <- unlist(lapply(data, function(x) return(ncol(x))))
    if(!isTRUE(all.equal(min(ncols), max(ncols)))){
      stop("'data.dir'contains files with different number of columns")
    }
    id_rows <- sapply(data, nrow)
    id <- rep(1:length(id_rows), id_rows)
    data <- lapply(data, as.data.frame)
    data <- do.call(rbind, data)
    data <- cbind("ID" = id, data)
    return(data)
  }
  
  session$onFlushed(function() {
    session$sendCustomMessage(type = "activate_tooltips", "")
  })
  
  current_page <- reactiveVal(1)
  question_start_time <- reactiveVal(Sys.time())
  time_spent <- reactiveValues(list = list())
  
  output$button_ui <- renderUI({
    if (current_page() == 1) {
      actionButton("button_1", "OK")
    } else if (current_page() == 2) {
      actionButton("button_2", "OK")
    } else if (current_page() == 3) {
      actionButton("button_3", "Начать опрос")
    } else {
      actionButton("OK", "OK")
    }
  })
  
  output$intro <- renderUI({
    HTML(intro.text)
  })
  
  observeEvent(input$button_1, {
    current_page(2)
    output$intro <- renderUI({
      HTML(readChar('intro_2.txt', file.info('intro_2.txt')$size))
    })
  })
  
  observeEvent(input$button_2, {
    current_page(3)
    output$intro <- renderUI({
      HTML(readChar('intro_3.txt', file.info('intro_3.txt')$size))
    })
  })
  
  Select <- function() {
    if (sn <= n.init) {
      set <- des[bs[sn]:es[sn], ]
    } else {
      if (sn == 1) {
        s <- tmvtnorm::rtmvnorm(n = n.draws, mean = prior.mean,
                                sigma = prior.covar, lower = lower,
                                upper = upper)
        w <- rep(1, nrow(s)) / nrow(s)
        if (sum(alt.cte) > 0.2) {
          s <- list(as.matrix(s[, 1:sum(alt.cte)], ncol = sum(alt.cte)),
                    s[, -c(1:sum(alt.cte))])
        }
      } else if (sn > 1){
        sam <- ImpsampMNL(n.draws = n.draws, prior.mean = prior.mean,
                          prior.covar = prior.covar,
                          des = fulldes, n.alts = n.alts, y = y.bin,
                          alt.cte = alt.cte, lower = lower, upper = upper)
        s <- sam$sample
        w <- sam$weights
      }
      if (algorithm == "MOD") {
        setobj <- SeqMOD(des = des, cand.set = cand.set, n.alts = n.alts,
                         par.draws = s, prior.covar = prior.covar,
                         alt.cte = alt.cte, weights = w,
                         no.choice = no.choice, parallel = parallel,
                         reduce = reduce)
      } else if (algorithm == "CEA") {
        setobj <- SeqCEA(des = des, lvls = n.levels, coding = coding,
                         n.alts = n.alts, par.draws = s,
                         prior.covar = prior.covar, alt.cte = alt.cte,
                         weights = w, no.choice = no.choice,
                         parallel = parallel, reduce = reduce)
      }
      set <- setobj$set
      db <- setobj$db
      if (sn == 1) {
        rowcol <- Rcnames(n.sets = 1, n.alts = n.alts, alt.cte = alt.cte, no.choice = FALSE)
        rownames(set) <- rownames(set, do.NULL = FALSE, prefix = paste(paste("set", sn, sep = ""), "alt", sep = "."))
        colnames(set) <- c(rowcol[[2]], paste("par", 1:(ncol(set) - n.cte), sep = "."))
        fulldes <<- set
      } else if (sn > 1) {
        rowcol <- Rcnames(n.sets = 1, n.alts = n.alts, alt.cte = alt.cte, no.choice = FALSE)
        rownames(set) <- rownames(set, do.NULL = FALSE, prefix = paste(paste("set", sn, sep = ""), "alt", sep = "."))
        colnames(set) <- c(rowcol[[2]], paste("par", 1:(ncol(set) - n.cte), sep = "."))
        fulldes <<- rbind(fulldes, set)
      }
    }
    choice.set <- Decode(des = set, n.alts = n.alts, lvl.names = lvl.names, coding = coding,
                         alt.cte = alt.cte, c.lvls = c.lvls, no.choice = no.choice)[[1]]
    choice.set <- t(choice.set[, 1:n.atts])
    colnames(choice.set) <- alts
    rownames(choice.set) <- atts
    if (sn == 1) {
      choice.sets <<- choice.set
    } else if (sn > 1) {
      choice.sets <<- rbind(choice.sets, choice.set)
    }
    if (!is.null(no.choice)) {
      no.choice.set <- choice.set[, -no.choice]
      return(no.choice.set)
    } else {
      return(choice.set)
    }
  }
  
  sn <- 1
  
  observeEvent(input$button_3, {
    question_start_time(Sys.time())
    current_page(4)
    output$intro <- renderText(NULL)
    output$set.nr <- renderText(paste(c("Вопрос:", sn, "/", n.total)))
    output$buttons <- renderUI({
      radioButtons("survey", buttons.text, alts, inline = TRUE, selected = "None")
    })
    output$table1 <- renderDT({
      datatable(Select(),
                rownames = TRUE,
                selection = 'none',
                options = list(dom = 't', pageLength = 20,
                               scrollX = TRUE,
                               rowCallback = JS(rowCallback),
                               initComplete = JS(initComplete)))
    })
  })
  
  observeEvent(input$OK, {
    if(!is.null(input$survey)) {
      time_spent$list[[sn]] <- difftime(Sys.time(), question_start_time(), units = "secs")
      question_start_time(Sys.time())
      sn <<- sn + 1
    }
  })
  
  observeEvent(input$OK, {
    output$buttons <- renderUI({
      if (sn <= n.total) {
        radioButtons("survey", buttons.text, alts, inline = TRUE, selected = "None")
      }
    })
  })
  
  observeEvent(input$OK, {
    if (!is.null(input$survey)) {
      if (sn <= n.total) {
        output$set.nr <- renderText(paste(c("Вопрос:", sn, "/", n.total)))
      } else {
        output$set.nr <- renderText(NULL)
      }
    }
  })
  
  observeEvent(input$OK, {
    if(!is.null(input$survey)) {
      if (sn <= n.total) {
        output$table1 <- renderDT({
          datatable(Select(),
                    rownames = TRUE,
                    selection = 'none',
                    options = list(dom = 't', pageLength = 20,
                                   scrollX = TRUE,
                                   rowCallback = JS(rowCallback),
                                   initComplete = JS(initComplete)))
        })
      } else {
        output$table1 <- renderDT(NULL)
      }
    }
  })
  
  observeEvent(input$OK, {
    if (!is.null(input$survey)) {
      resp <<- c(resp, input$survey)
      y.bin <<- Charbin(resp = resp, alts = alts, n.alts = n.alts)
      sdata[["bin.responses"]] <- y.bin
      sdata[["responses"]] <- resp
      sdata[["design"]] <- fulldes
      sdata[["survey"]] <- choice.sets
      sdata[["time_spent"]] <- rep(unlist(time_spent$list), each = n.atts)
      surveyData <<- sdata
    }
  })
  
  observeEvent(input$OK, {
    if (sn == n.total + 1) {
      output$intro <- renderUI({
        HTML(sprintf(readChar('end.txt', file.info('end.txt')$size), user_id))
      })
    }
    if (sn == n.total + 2) {
      if (!is.null(data.dir)) {
        session$sendCustomMessage("mymessage", user_id)
        saveData(data = surveyData, data.dir = data.dir, n.atts = n.atts)
      }
      stopApp()
    }
  })
  
}
