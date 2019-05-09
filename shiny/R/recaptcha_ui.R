#Based on recaptchaUI, but without the submit button and in spanish
recaptcha_ui <- function (id, sitekey = Sys.getenv("recaptcha_sitekey"), ...) 
{
  ns <- NS(id)
  tagList(tags$div(shiny::tags$script(src = "https://www.google.com/recaptcha/api.js?explicit&hl=es", 
                                      async = NA, defer = NA), tags$script(paste0("shinyCaptcha = function(response) {\n          Shiny.onInputChange('", 
                                                                                  ns("recaptcha_response"), "', response);\n      }")), 
                   tags$form(class = "shinyCAPTCHA-form", action = "?", 
                             method = "POST", tags$div(class = "g-recaptcha", 
                                                       `data-sitekey` = sitekey, `data-callback` = I("shinyCaptcha"))#, 
                             # tags$br(), tags$input(type = "submit", ...)
                             )))
}