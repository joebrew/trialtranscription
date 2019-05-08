library("shiny")

css <- 
  "
a#more-apps-by-dean {
display: none;
}
body {
background: #fff;
}
.container {
margin: 0;
padding: 15px;
}
.green {
background: green;
}
#expr-container .selectize-input {
font-size: 24px;
line-height: 24px;
}
#expr-container .selectize-dropdown {
font-size: 20px;
line-height: 26px;
}
#expr-container .selectize-dropdown-content {
max-height: 225px;
padding: 0;
}
#expr-container .selectize-dropdown-content .option {
border-bottom: 1px dotted #ccc;
}
#submitExpr {
font-weight: bold;
font-size: 18px;
padding: 5px 20px;
}
#helpText {
font-size: 18px;
}
#btn {
font-size: 20px;
}
"

people <- images <- dir('www/people')
people <- strsplit(people, split = '.', fixed = TRUE)
people <- unlist(lapply(people, function(x){x[1]}))
# people <- people[people != 'unknown']
# names(people) <- images
final <- images; names(final) <- people; people <- final

# images are displayed only in dropdown menu
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$style(css),
  br(),
  div(id = "expr-container",
      selectizeInput(
        'selected_person', '',
        choices = people,
        # selected = ,
        options = list(
          render = I(
            "{
            option: function(item, escape) {
            return '<div><img src=\"people/' + item.value + '\" width = 40 />' + escape(item.label) + '</div>'
            }
            }")
    )
  )),
  textOutput('the_text')
)

server <- function(input, output, session) {
  
  output$the_text <- renderText({
    input$selected_person
  })
  
  
}

shinyApp(ui = ui, server = server)