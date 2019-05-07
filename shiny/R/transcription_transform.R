transcription_transform <- function(x){
  # take a dataframe in the format of the transcription table and reformat to a simple
  # who / what two column df
  out <- NULL
  if(!is.null(x)){
    if(nrow(x) > 0){
      out <- x %>% dplyr::select(speaker, transcription) %>%
        dplyr::rename(who = speaker,
                      what = transcription)
    }
  }
  return(out)
}