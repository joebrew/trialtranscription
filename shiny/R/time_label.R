time_label <- function(df){
  title <- df$video_title
  start <- df$start_time
  end <- df$end_time
  
  start_minutes <- floor(start / 60)
  start_hours <- start_minutes %/% 60
  start_minutes <- start_minutes %% 60
  
  end_minutes <- floor(end / 60)
  end_hours <- end_minutes %/% 60
  end_minutes <- end_minutes %% 60
  
  
  start_time <- paste0(start_hours, ':', start_minutes, ':00')
  end_time <- paste0(end_hours, ':', end_minutes, ':00')

  
  paste0(title, ' de ',
         start_time, ' a ', end_time)
}