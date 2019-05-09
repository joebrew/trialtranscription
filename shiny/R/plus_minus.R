# Add row
plus_minus <- function(n = 1, the_language = 'Català'){
  
  if(the_language == 'Español'){
    otra_text <- "Si otra persona habla también, haz clic aquí para añadir otra fila"
    add_text <- "Añada otra fila"
    subtract_text <- "Borra la última fila"
  } else if(the_language == 'Català'){
    otra_text <- "Si una altre persona parla també, fès clic per afegir una altre fila"
    add_text <- "Afegeix una altre fila"
    subtract_text <- "Esborra la última fila"
  } else if(the_language == 'English'){
    otra_text <- "If another person also speaks, click here to add another row"
    add_text <- "Add another row"
    subtract_text <- "Delete the last row"
  }
  
  if(n > 1){
    fluidRow(column(12,
                    align = 'center',
                    actionButton("plus1", add_text,icon=icon("plus-circle")),
                    actionButton("minus1", subtract_text,icon=icon("minus-circle"))))
  } else {
    fluidRow(column(12,
                    align = 'center',
                    actionButton("plus1", otra_text,icon=icon("plus-circle"))))
  }
}
