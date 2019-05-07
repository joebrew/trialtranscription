# Based on embed_youtube
make_youtube <- function (id, width = 420, height = 315, frameborder = 0, allowfullscreen = TRUE, 
                          query = NULL) 
{
  url <- httr::parse_url("https://www.youtube.com/embed")
  url$path <- paste(url$path, id, sep = "/")
  url$query <- query
  embed <- htmltools::tags$iframe(src = httr::build_url(url), 
                                  width = width, height = height, frameborder = frameborder, 
                                  allowfullscreen = FALSE)
  class(embed) <- c("embed_youtube", class(embed))
  embed
}