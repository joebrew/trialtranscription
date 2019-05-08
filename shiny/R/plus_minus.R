# Add row
plus_minus <- function(n = 1){
  if(n > 1){
    fluidRow(column(12,
                    align = 'center',
                    actionButton("plus1", "Añada otra fila",icon=icon("plus-circle")),
                    actionButton("minus1", "Borra la última fila",icon=icon("minus-circle"))))
  } else {
    fluidRow(column(12,
                    align = 'center',
                    actionButton("plus1", "Si otra persona habla también, haz clic aquí para añadir otra fila",icon=icon("plus-circle"))))
  }
}
