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
  previous_transcription <- reactiveVal(value = '')
  quiero_reactive <- reactiveVal(value = 'Transcribir')
  segment_reactive <- reactiveVal(value = '')
  data <- reactiveValues(users = users,
                         transcriptions = transcriptions,
                         chunks = chunks)
  show_cambiar <- reactiveVal(value = FALSE)
  
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
            text = 'About',
            tabName = 'about',
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
  
  # Update the previous transcription
  observeEvent(c(input$quiero,
                 input$segment), {
    ok <- FALSE
    is <- input$segment
    iq <- input$quiero
    message('Just starting')
    message('input$segment is ', is)
    message('input$quiero is ', iq)
    # if(!is.null(iq)){
      if(iq == 'Repasar'){
        if(!is.null(is)){
          message('So far, so good. Here is the head of transcriptions')
          print(head(transcriptions))
          the_previous <- transcriptions %>%
            filter(chunk_url == is) %>%
            filter(!is.na(transcription),
                   transcription != '')
          message('After filtering, it is...')
          print(head(transcriptions))
          
          if(nrow(the_previous) > 0){
            ok <- TRUE
          }
        }
      }
    # }
    if(ok){
      the_previous <- the_previous[nrow(the_previous),]
      previous_transcription(the_previous$transcription)
    } else {
      previous_transcription('')
    }
  })
  
  # Observe the segment selector and update the reactive object
  observeEvent(input$segment,{
    segment_reactive(input$segment)
  })

  # Transcription page
  output$ui_transcribir <- renderUI({
    tai <- NULL
    ab <- NULL
    
    # Read the chunks
    done_transcriptions <- data$transcriptions
    new_chunks <- chunks %>%
      filter(!chunk_url %in% done_transcriptions$chunk_url)
    done_chunks <- chunks %>%
      filter(chunk_url %in% done_transcriptions$chunk_url)
    
    new_choices <- paste0(new_chunks$chunk_url)
    new_labels <- paste0(new_chunks$video_title, ' minuto: ',
                         new_chunks$start_time / 60 + 1)
    if(length(new_choices) > 0){
      names(new_choices) <- new_labels  
    } else {
      new_choices <- NULL
    }
    
    
    done_choices <- paste0(done_chunks$chunk_url)
    done_labels <- paste0(done_chunks$video_title, ' minuto: ',
                         done_chunks$start_time / 60 + 1)
    if(length(done_choices) > 0){
      names(done_choices) <- done_labels  
    } else {
      done_choices <- NULL
    }
    
    input_quiero <- input$quiero
    if(is.null(input_quiero)){
      ht <- ''
      segment_select <- NULL
    } else {
      if(input_quiero == 'Transcribir'){
        quiero_reactive('Transcribir')
        ht <- 'Selecciona uno de los segmentos que todavía no se ha transcrito, y transcríbelo.'
        sr <- segment_reactive()
        segment_select <- selectInput('segment',
                                      label = 'Segmento',
                                      choices = new_choices,
                                      selected = sr)
        tai <- textAreaInput('transcription_text',
                             label = 'Transcripción',
                             value = '',
                             width = '100%', 
                             placeholder = 'Escribe la transcripción aquí',
                             resize = 'both')
        ab <- actionButton('transcription_submit',
                           label = 'Somete la transcripción')
      } else {
        if(length(done_choices) > 0){
          ht <- 'Selecciona uno de los segmentos ya transcritos y somete, si hace falta, correcciones.' 
          selected_id <- input$segment
          pt <- previous_transcription()
          tai <- textAreaInput('transcription_text',
                               label = 'Transcripción',
                               value = pt,
                               width = '100%',                                          # cols = 1, 
                               # rows = 3,
                               placeholder = 'Escribe la transcripción aquí',
                               resize = 'both')
          ab <- actionButton('transcription_submit',
                             label = 'Somete la corrección')
          quiero_reactive('Repasar')
          sr <- segment_reactive()
          segment_select <- selectInput('segment',
                                        label = 'Segmento',
                                        choices = done_choices,
                                        selected = sr)
        } else {
          tai <- NULL
          ab <- NULL
          ht <- 'No hay segmentos por repasar. Seleccione "Transcribir" para transcribir un segmento nuevo.' 
          quiero_reactive('Repasar')
          segment_select <- NULL
        }
       
      }
    }
    
    fluidPage(
      fluidRow(
        column(12,
               align = 'center',
               selectInput('quiero',
                           label = 'Quiero...',
                           choices = c('Transcribir', 'Repasar'),
                           selected = quiero_reactive()))
      ),
      fluidRow(
        column(12, align = 'center',
               helpText(ht))
      ),
      fluidRow(
        column(12,
               align = 'center',
               segment_select
        )
      ),

    fluidRow(column(12,
                    align = 'center',
                    uiOutput('ui_video'))),
    fluidRow(
      column(12,
             align = 'center',
             tai)
    ),
    fluidRow(column(12,
                    align = 'center',
                    ab))
    )
  })
  
  
  # Observe the event transcription and update
  observeEvent(input$transcription_submit,{
    the_url <- input$segment
    the_user <- user_name()
    the_transcription <- input$transcription_text
    the_time <- Sys.time()
    new_row <- tibble(chunk_url = the_url,
                      user = the_user,
                      transcription = the_transcription,
                      created_at = the_time)
    # Update the database
    update_db(data = new_row,
              table_name = 'transcriptions',
              connection_object = co)
    # Update the reactive objects
    newt <- data$transcriptions %>%
      bind_rows(new_row)
    data$transcriptions <- newt
  })
  
  # Dynamic video for watching
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
                         'Nombre de usuario',
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
               h3('Entrar'),
               textInput('user_name',
                         'Nombre de usuario',
                         value = ''),
               passwordInput('password',
                             'Contraseña',
                             value = ''),
               actionButton('log_in', 'Entrar')
        ),
        column(6,
               h2('Quiero crear una cuenta'),
               textInput('user_name_crear',
                         'Nombre de usuario',
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