# Generate a row with a who and a what
make_row <- function(n = 1){
  fluidRow(
    column(4, align = 'center',
           textInput(paste0('who', n),
                     label = 'Quien',
                     placeholder = 'Quien habla?')),
    column(8,
           align = 'center',
           textAreaInput(paste0('que', n),
                           label = 'Qué dice?',
                           placeholder = 'Que dice?'))
  )
}

make_row_text <- function(n = 1, who = NULL, what = NULL, people = NULL){
  peeps <- as.character(people)
  np <- names(people)
  out <- paste0("c(", paste0("'",np, "' = '", people, "'", collapse = ','), ")")
  people <- out
  who_ok <- what_ok <- FALSE
  if(!is.null(who)){
    if(!is.na(who)){
      who_ok <- TRUE
    }
  }
  if(who_ok){
    who_part <- paste0("selected = '", who, "'")
    options_part <- ''
  } else {
    who_part <-  ''
    options_part <- "create = TRUE, placeholder = 'Selecciona o escribe el nombre de la persona'"
  }
  
  
  
  if(!is.null(what)){
    if(!is.na(what)){
      what_ok <- TRUE
    }
  }
  if(what_ok){
    what_part <- paste0("value = '", what, "'")
  } else {
    what_part <- "placeholder = 'Qué dice?'"
  }
  
  paste0("fluidRow(
    column(4, align = 'center',
          div(id = 'expr-container',
           selectizeInput(paste0('who', ", n, "),
                     label = NULL,
                    options = list(", options_part, ",
                              render = I(
                                \"{
                                  option: function(item, escape) {
                                    return '<div><img src=\\\"people/' + item.value + '\\\" width = 40 />' + escape(item.label) + '</div>'
                                  }
                                }\")
                        ),
                     ", who_part, ",
                      choices = ",people, "))),
    column(8,
           align = 'center',
           textAreaInput(paste0('what', ", n, "),
                           label = NULL,
                           ", what_part, "))
  )")
}

make_rows <- function(n, who = NULL, what = NULL, people = NULL){
  out_list <- list()
  for(i in 1:n){
    out_list[[i]] <- make_row_text(n = i, 
                                   who = who[i],
                                   what = what[i],
                                   people = people)
  }
  out <- paste0(out_list, collapse = ',\n')
  out <- paste0('fluidPage(', 
                out,
                ')')
  return(out)
}

experiment <- function(){
  
}