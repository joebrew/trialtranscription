# Generate a row with a who and a what
make_row <- function(n = 1){
  fluidRow(
    column(4, align = 'center',
           textInput(paste0('who', n),
                     label = 'Quien',
                     placeholder = 'Quien habla?')),
    column(8,
           align = 'center',
           text_area_input(paste0('que', n),
                           label = 'Qué dice?',
                           placeholder = 'Que dice?'))
  )
}

make_row_text <- function(n = 1, who = NULL, what = NULL){
  who_ok <- what_ok <- FALSE
  if(!is.null(who)){
    if(!is.na(who)){
      who_ok <- TRUE
    }
  }
  if(who_ok){
    who_part <- paste0("value = '", who, "'")
  } else {
    who_part <- "placeholder = 'Quien habla?'"
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
           textInput(paste0('who', ", n, "),
                     label = 'Quien',
                     ", who_part, ")),
    column(8,
           align = 'center',
           text_area_input(paste0('what', ", n, "),
                           label = 'Qué dice?',
                           ", what_part, "))
  )")
}

make_rows <- function(n, who = NULL, what = NULL){
  out_list <- list()
  for(i in 1:n){
    out_list[[i]] <- make_row_text(n = i, 
                                   who = who[i],
                                   what = what[i])
  }
  out <- paste0(out_list, collapse = ',\n')
  out <- paste0('fluidPage(', 
                out,
                ')')
  return(out)
}
