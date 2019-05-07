library(shiny)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)
library(htmltools)
library(vembedr) # https://ijlyttle.github.io/vembedr/

source('global.R')
header <- dashboardHeader(title="Transcripción del juicio",
                          tags$li(class = 'dropdown',  
                                  tags$style(type='text/css', "#log_out {margin-right: 10px; margin-left: 10px; font-size:80%; margin-top: 10px; margin-bottom: -10px;}"),
                                  tags$style(type='text/css', "#quiero {margin-right: 20px; margin-left: 10px; font-size:80%; margin-top: 5px; margin-bottom: -15px;}"),
                                  tags$li(class = 'dropdown',
                                          uiOutput('quiero_ui')),
                                  tags$li(class = 'dropdown',
                                          uiOutput('log_out_ui'))))
sidebar <- dashboardSidebar(
  sidebarMenuOutput("menu")
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(
      tabName = 'transcribir',
      uiOutput('ui_transcribir')
    ),
    tabItem(
      tabName = 'log_in',
      uiOutput('ui_main_page')
    ),
    tabItem(
      tabName = 'informacion',
      fluidPage(
        fluidRow(column(12, align = 'center',
                        selectInput('language',
                                    'Lengua / Language / Llengua',
                                    choices = c('Español', 'Català', 'English')))),
        uiOutput('ui_include'))
    )
  )
)


# Function for authenticating password
user_exists <- function(user_name, 
                        users_table){
  user_name %in% users_table$user_email
}
authenticate <- function(user_name,
                         password,
                         users_table){
  this_data <- users_table %>%
    dplyr::filter(user_email == user_name)
  password == this_data$user_password
}

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output,session) {
  
  # User info
  logged_in <- reactiveVal(value = FALSE)
  user_name <- reactiveVal(value = '')
  password <- reactiveVal(value = '')
  log_in_text <- reactiveVal(value = '')
  quiero_reactive <- reactiveVal(value = 'Transcribir')
  segment_reactive <- reactiveVal(value = '')
  data <- reactiveValues(users = users,
                         transcriptions = transcriptions,
                         chunks = chunks, 
                         current = data.frame())
  show_cambiar <- reactiveVal(value = FALSE)
  the_number <- reactiveVal(1)
  
  # Observe the show cambiar button
  observeEvent(input$show_cambiar_button, {
    sc <- show_cambiar()
    show_cambiar(!sc)
  })
  
  # Obseve the log in
  observeEvent(input$log_in, {
    exists <- user_exists(user_name = input$user_name,
                          users_table = data$users)
    if(!exists){
      log_in_text('This user does not exist.')
    } else {
      auth <- authenticate(user_name = input$user_name,
                           password = input$password,
                           users_table = data$users)
      if(!auth){
        log_in_text('The password is incorrect')
      } else {
        log_in_text('Successfully logged in.')
        logged_in(TRUE)
        user_name(input$user_name) 
        password(input$password)
      }
    }
  })
  
  # Observe the segment selection. If it changes, change the data$current object
  observeEvent(input$segment,{
    is <- input$segment
    message('input$segment has changed to ', is)
    the_previous <- data$transcriptions %>% filter(chunk_url == is) %>%
      filter(number == 1)
    if(nrow(the_previous) != 1){
      # Either no previous transcription or more than 1 (ie, no need to verify again)
      data$current <- data.frame()
    } else {
      # There is exactly 1 previous transcription, and it needs to be verified
      one_previous <- data$transcriptions %>% filter(chunk_url == is)
      # Update the reactive object
      data$current <- one_previous
    }
  })
  
  # # Observe any addition of rows, and ensure that the old data does not get deleted
  observeEvent(input$plus1,{
    # Capture the old data
    nn <- the_number()
    whos <- whats <- c()
    for(i in 1:nn){
      whos[i] <- eval(parse(text = paste0('input$who', i)))
      whats[i] <- eval(parse(text = paste0('input$what', i)))
    }
    # Make a dataframe of the current data
    df <- tibble(who = whos,
                 what = whats)
    # Update the reactive objects
    data$current <- df
    message('data$current is ')
    print(data$current)
  })

  # Create a dynamic ui for adding and removing transcriptions
  output$dynamic <- renderUI({
    ok <- TRUE
    
    # See if there is a previous transcription selected
    previous <- data$current
    any_previous <- FALSE
    if(!is.null(previous)){
      if(nrow(previous) > 0){
        any_previous <- FALSE
      }
    }

    # Get the number
    nn <- the_number()
    if(ok){
      if(any_previous){
        # There is a previous transcription, update the number and load it
        the_previous <- data$current
        # Update number
        # Load previous rows
        the_rows <- NULL
        # Load the plus_minus sign
        pm <- plus_minus(n = nn)
      } else {
        # No previous, start a new transcription
        # Keep any currently entered data though
        df <- data$current
        the_rows <- eval(parse(text = make_rows(n = nn, who = df$who, what = df$what)))
        pm <- plus_minus(n = nn)
      }
      out <- fluidPage(the_rows,
                       pm)
    } else {
      out <- NULL
    }
    return(out)
  })
  
  # Observe the plus/minus button and delete or add rows
  observeEvent(input$plus1,{
    n <- the_number()
    the_number(n + 1)
    message('The number just went from ', n, ' to ', n+1, '.')
  })
  observeEvent(input$minus1,{
    n <- the_number()
    the_number(n -1)
    message('The number just went from ', n, ' to ', n-1, '.')
    
  })
  
  # Account creation
  observeEvent(input$crear_cuenta, {
    exists <- user_exists(user_name = input$user_name_crear,
                          users_table = data$users)
    if(exists){
      log_in_text('Este usuario ya existe.')
    } else {
      if(is_email(input$user_name_crear)){
        user_name <- reactiveVal(value = input$user_name_crear)
        password <- reactiveVal(value = 'password')
        log_in_text(paste0('Cuenta creada. Tu usuario es ',
                           user_name(),
                           '. Tu contraseña es password.'))
        old_users <- data$users
        this_user <- tibble(user_email = user_name(),
                            user_password = 'password')
        # Update the database
        update_db(data = this_user,
                  table_name = 'users',
                  connection_object = co)
        # Update the reactive data
        data$users <- bind_rows(data$users, this_user)
      } else {
        log_in_text('Este correo no es válido.')
      }
    }
  })
  
  # Password contraseña change
  observeEvent(input$cambiar, {
    exists <- user_exists(user_name = input$user_name_contra,
                          users_table = data$users)
    if(!exists){
      log_in_text('Este usuario no existe.')
    } else {
      
      # confirm the old password is correct
      auth <- authenticate(user_name = input$user_name_contra,
                           password = input$old_password,
                           users_table = data$users)
      
      # If authenticated, move on
      if(auth){
        # confirm the passwords match
        pass1 <- input$new_password
        pass2 <- input$new_password2
        if(pass1 != pass2){
          log_in_text('Las dos contraseñas nuevas no son las mismas.')
        } else {
          user_name <- reactiveVal(value = input$user_name_contra)
          u <- user_name()
          password <- reactiveVal(value = pass2)
          pass <- password()
          log_in_text(paste0('Contraseña cambiada. Tu usuario es ',
                             user_name(),
                             '. Tu contraseña es ', pass, '.'))
          # Rewrite the password
          RPostgreSQL::dbSendQuery(conn = co,
                                   statement = 
                                     paste0("UPDATE users SET user_password = '",
                                            pass, "' WHERE user_email = '",
                                            u, "';"))
          
          # Update the reactive data
          data$users <- get_data(query = 'SELECT * FROM users', connection_object = co)
        }
      } else {
        log_in_text('La contraseña actual que escribiste no corresponde con este usuario.')
      }
      
      
    }
  })
  
  
  # Observe the log out button
  observeEvent(input$log_out, {
    logged_in(FALSE)
    user_name('')
    password('')
    log_in_text('')
  })
  
  # Side bar menu
  output$menu <-
    renderMenu({
      # Logged in or not?
      li <- logged_in()
      if(!li){
        sidebarMenu(
          id = 'tabs',
          menuItem(
            text = 'Log-in',
            tabName = 'log_in',
            icon = icon('eye')
          ),
          menuItem(
            text = 'Información',
            tabName = 'informacion',
            icon = icon("cog", lib = "glyphicon")))
        
      } else {
        sidebarMenu(
          id="tabs",
          menuItem(
            text="Transcribir",
            tabName="transcribir",
            icon=icon("eye")),
          menuItem(
            text = 'Información',
            tabName = 'informacion',
            icon = icon("cog", lib = "glyphicon")))
      }
    })
  
  # # Update the previous transcription
  # observeEvent(c(input$quiero,
  #                input$segment), {
  #                  ok <- FALSE
  #                  is <- input$segment
  #                  iq <- input$quiero
  #                  # if(!is.null(iq)){
  #                  if(iq == 'Revisar'){
  #                    if(!is.null(is)){
  #                      the_previous <- data$transcriptions %>%
  #                        filter(chunk_url == is) 
  #                      
  #                      if(nrow(the_previous) > 0){
  #                        ok <- TRUE
  #                      }
  #                    }
  #                  }
  #                  # }
  #                  if(ok){
  #                    the_previous <- the_previous[nrow(the_previous),]
  #                    previous_transcription(the_previous$transcription)
  #                  } else {
  #                    previous_transcription('')
  #                  }
  #                })
  
  # Observe the segment selector and update the reactive object
  observeEvent(input$segment,{
    segment_reactive(input$segment)
  })
  
  # Which language to include the info
  output$ui_include <- renderUI({
    
    
    language <- input$language
    if(is.null(language)){
      language <- 'Español'
    }
    
    if(language == 'Español'){
      include_file <- "includes/include_es.md"
    } else if(language == 'English'){
      include_file <- "includes/include_en.md"
    } else if(language == 'Català'){
      include_file <- "includes/include_ca.md"
    }
    
    fluidPage(
      fluidRow(
        includeMarkdown(include_file)
      ),
      fluidRow(
        div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
        h4('A pro bono project of ',
           a(href = 'http://databrew.cc',
             target='_blank', 'Databrew'),
           align = 'center'),
        p('Empowering research and analysis through collaborative data science for social good.', align = 'center'),
        div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                           icon = icon("envelope", lib = "font-awesome")),
              href="mailto:info@databrew.cc",
              align = 'center')), 
        style = 'text-align:center;'
      )
    )
  })
  
  # Options for segments
  done_options <- reactiveVal(value = c())
  new_options <- reactiveVal(value = c())
  
  # Observe any submission or log in, and update the done/new options
  observeEvent(c(input$submit,
                 input$log_in),{
    # Read the chunks
    done_transcriptions <- data$transcriptions
    # If a transcription has been done AND checked by someone else, remove it from the list
    done_transcriptions <- done_transcriptions %>%
      filter(number == 1) %>% group_by(chunk_url) %>%
      summarise(n = n()) %>% ungroup %>% filter(n == 1) # ie, anything with 2 is out
    done_chunks <- chunks %>%
      filter(chunk_url %in% done_transcriptions$chunk_url)
    done_choices <- paste0(done_chunks$chunk_url)
    if(length(done_choices) > 0){
      done_labels <- paste0(done_chunks$video_title, ' minuto: ',
                            done_chunks$start_time / 60 + 1)
      names(done_choices) <- done_labels  
    } else {
      done_choices <- NULL
    }
    
    new_chunks <- chunks %>%
      filter(!chunk_url %in% done_transcriptions$chunk_url)
    new_choices <- paste0(new_chunks$chunk_url)
    if(length(new_choices) > 0){
      new_labels <- paste0(new_chunks$video_title, ' minuto: ',
                           new_chunks$start_time / 60 + 1)
      names(new_choices) <- new_labels  
      
    } else {
      new_choices <- NULL
    }

    # Update the reactive objects
    done_options(done_choices)
    new_options(new_choices)
    the_number(1)
  })
  
  
  
  # Transcription page
  output$ui_transcribir <- renderUI({
    ab <- NULL
    comment <- NULL
    
    # Get the choices
    done_choices <- done_options()
    new_choices <- new_options()
    
    input_quiero <- input$quiero
    if(is.null(input_quiero)){
      ht <- ''
      segment_select <- NULL
    } else {
      if(input_quiero == 'Transcribir'){
        quiero_reactive('Transcribir')
        ht <- 'Selecciona, a la izquierda, uno de los segmentos que todavía no se ha transcrito, y transcríbelo a la derecha.'
        sr <- segment_reactive()
        segment_select <- selectInput('segment',
                                      label = 'Segmento',
                                      choices = new_choices,
                                      selected = sr)
        ab <- actionButton('submit',
                           label = 'Somete la transcripción (y comentario si aplicable)')
        comment <- text_area_input('comment', 'Comentarios sobre la transcripción (opcional)',
                                   placeholder = 'Por ejemplo, "no se escucha bien lo que dice en el segundo 32", o "no hablan durante este minuto del juico", etc. Deja en blanco si no tienes comentarios.')
      } else {
        if(length(done_choices) > 0){
          ht <- 'Selecciona, a la izquierda, uno de los segmentos ya transcritos y somete, si hace falta, correcciones a la derecha. Si no hace falta ninguna corrección, somete la transcripción tal cual.' 
          selected_id <- input$segment
          ab <- actionButton('submit',
                             label = 'Somete la corrección (y comentario si aplicable)')
          comment <- text_area_input('comment', 'Comentarios sobre la transcripción (opcional)',
                                     placeholder = 'Por ejemplo, "no se escucha bien lo que dice en el segundo 32", o "no hablan durante este minuto del juico", etc. Deja en blanco si no tienes comentarios.')
          
          quiero_reactive('Revisar')
          sr <- segment_reactive()
          segment_select <- selectInput('segment',
                                        label = 'Segmento',
                                        choices = done_choices,
                                        selected = sr)
        } else {
          ab <- NULL
          comment <- NULL
          ht <- 'No hay segmentos por revisar. Seleccione "Transcribir" para transcribir un segmento nuevo.' 
          quiero_reactive('Revisar')
          segment_select <- NULL
        }
        
      }
    }
    
    fluidPage(
      fluidRow(
        column(12, align = 'center',
               helpText(ht))
      ),
      fluidRow(column(6,
                      align = 'center',
                      segment_select,
                      uiOutput('ui_video')),
               column(6,
                      align = 'center',
                      uiOutput('dynamic'), br(), br(),
                      comment, br(),
                      ab)
      ),
      fluidRow(
        tags$button(
          id = "web_button",
          class = "btn action-button",
          tags$img(src = "http://images.all-free-download.com/images/graphicthumb/button_play_89677.jpg",
                   height = "50px")
        )
      )
    )
  })
  
  
  # Observe the button presses and update the text
  observeEvent(input$web_button,{
    
  })
  
  
  # Observe the event transcription and update
  observeEvent(input$submit,{
    the_url <- input$segment
    the_user <- user_name()
    # Get the current data
    df <- data$current
    the_time <- Sys.time()
    the_comment <- input$comment
    new_row <- tibble(chunk_url = the_url,
                      number = 1:nrow(df),
                      user = the_user,
                      speaker = df$who,
                      transcription = df$what,
                      created_at = the_time,
                      comment = the_comment)
    # Update the reactive previous transcription (not done)
    
    # Update the database
    update_db(data = new_row,
              table_name = 'transcriptions',
              connection_object = co)
    # Update the reactive objects
    newt <- data$transcriptions %>%
      bind_rows(new_row)
    data$transcriptions <- newt
  })
  
  # video for watching
  output$video <- renderUI({
    ok <- FALSE
    selected_segment <- input$segment
    if(!is.null(selected_segment)){
      ok <- TRUE
    }
    if(ok){
      the_url <- 
        paste0(
          # 'https://www.youtube.com/embed/',
          selected_segment
        )
      embed_youtube(the_url)
    } else {
      NULL
    }
    # the_id <- '53m3xLaVC6o'
    # start_time <- 5246
    # end_time <- 5250
    # the_url <- paste0(
    #   # 'https://www.youtube.com/embed/',
    #   the_id,
    #   '?start=',
    #   start_time,
    #   '&end=',
    #   end_time
    # )
    
  })
  
  output$ui_video <- renderUI({
    htmlOutput('video')
  })
  
  output$ui_main_page <- renderUI({
    lit <- log_in_text()
    sc <- show_cambiar()
    
    # Make the cambiar contraseña row
    if(sc){
      scrow <- 
        fluidRow(
          h3('Cambiar contraseña'),
          column(6,
                 textInput('user_name_contra',
                           'Correo electrónico',
                           value = ''),
                 textInput('old_password',
                           'Contraseña actual',
                           value = '')),
          column(6,
                 textInput('new_password',
                           'Contraseña nueva',
                           value = ''),
                 textInput('new_password2',
                           'Confirmar contraseña nueva',
                           value = '')),
          actionButton('cambiar', 'Cambiar contraseña')
        )
    } else {
      scrow <- fluidRow()
    }
    
    fluidPage(
      fluidRow(
        column(6,
               h2('Ya tengo una cuenta'),
               textInput('user_name',
                         'Correo electrónico',
                         value = 'joe@databrew.cc'),
               passwordInput('password',
                             'Contraseña',
                             value = 'password'),
               actionButton('log_in', 'Entrar')
        ),
        column(6,
               h2('Quiero crear una cuenta'),
               textInput('user_name_crear',
                         'Correo electrónico',
                         value = '')),
        actionButton('crear_cuenta', 'Crear cuenta')
      ),
      fluidRow(
        column(12, align = 'center',
               h3(lit))
      ),
      fluidRow(
        column(12, align = 'center',
               actionButton('show_cambiar_button',
                            label = 'Cambiar contraseña'))
      ),
      scrow
    )
  })
  
  output$quiero_ui <-renderUI({
    li <- logged_in()
    
    if(li){
      the_choices <- c('Transcribir', 'Revisar')
      names(the_choices) <- paste0('Quiero ', the_choices)
      tags$li(class = 'dropdown',
              radioButtons('quiero',
                           label = '',
                           inline = TRUE,
                           choices = the_choices,
                           selected = quiero_reactive()))
      
    } else {
      NULL
    }
  })
  
  
  output$log_out_ui <- renderUI({
    li <- logged_in()
    
    if(li){
      tags$li(class = 'dropdown',
              actionButton('log_out', label = 'Log out', icon = icon('times')))
      
    } else {
      NULL
    }
  })
  
  
  session$onSessionEnded(function() {
    message('Session ended. Closing the connection pool.')
    tryCatch(RPostgreSQL::dbDisconnect(co), error = function(e) {message('')
      
    })
    
  })
}

shinyApp(ui, server)