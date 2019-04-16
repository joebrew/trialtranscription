library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gsheet)
library(mailR)
library(tuber)
library(yaml)
library(httr)

# Get responses of volunteers
url_responses<- 'https://docs.google.com/spreadsheets/d/1mjQr7zMNvD6eGO9HF1ItVquPytc1b77juSXzR2lYCNw/edit?usp=sharing'
df <- gsheet2tbl(url_responses)

# # Get info on videos
# httr::set_config( config( ssl_verifypeer = 0L ) )
# app_details <- yaml::yaml.load_file('credentials/credentials.yaml')
# yt_oauth(app_details$google_client_id,
#          app_details$google_client_secret,
#          scope = own_account_readonly)#,
#          # token = '')
# httr::set_config( config( ssl_verifypeer = 0L ) )
# 
# # The below is not currently working
# videos <- tuber::list_channel_videos('UC5s88i82hVTF-AvtlTvExPA')
# 
# # For each video, get info
# for(i in 1:nrow(videos)){
#   this_video_id <- as.character(videos$id[i])
#   this_info <- tuber::get_video_details(video_id = this_video_id)
# }

# Get video info
url_videos <- 'https://docs.google.com/spreadsheets/d/1tNz4tOnG8_BmpZYJtOtVb9yoiZRJb8gyD5PF3ciZ2Sk/edit?usp=sharing'
youtube_list <- gsheet2tbl(url_videos)

urls <- youtube_list$collaboration_url

#Setting up empty df to store data
temp.df = data.frame(id="", date="", title="", duration="", mins="", secs="",
                     description="", views= "", pos="", neg="", fullurl="")
youtube_df = data.frame() #Will include the final outcome

#Loop through the list of links and extract some general metadata
library(xml2)
library(rvest)
for(i in 1:length(youtube_list$url)){
  if(!is.na(youtube_list$url[i])){
    message(i)
    youtube_url = read_html(as.character(youtube_list$url[[i]]))
    id = as.character(html_nodes(youtube_url, 'meta[itemprop="videoId"]') %>% 
                        html_attr("content"))
    date = as.character(html_nodes(youtube_url, 'meta[itemprop="datePublished"]') %>% 
                          html_attr("content"))
    title = as.character(html_nodes(youtube_url, 'meta[itemprop="name"]') %>% 
                           html_attr("content"))
    mins = as.numeric(gsub("M","",str_extract(as.character(html_nodes(youtube_url, 'meta[itemprop="duration"]') %>% 
                                                             html_attr("content")), "\\d*M")))
    secs = as.numeric(gsub("S","",str_extract(as.character(html_nodes(youtube_url, 'meta[itemprop="duration"]') %>% 
                                                             html_attr("content")), "\\d*S")))
    duration = (mins*60) + secs
    description = as.character(html_node(youtube_url, '#eow-description') %>% 
                                 html_text())
    views = as.numeric(html_nodes(youtube_url, 'meta[itemprop="interactionCount"]') %>% 
                         html_attr("content"))  
    try({
      pos = html_nodes(youtube_url, 'span.yt-uix-button-content') %>% 
        html_text()
      pos = as.numeric(gsub(",", "", pos[15]))}, silent = TRUE)
    if(length(pos)==0){
      pos=NA
    }
    try({
      neg = html_nodes(youtube_url, 'span.yt-uix-button-content') %>% 
        html_text()
      neg = as.numeric(gsub(",", "", neg[18]))}, silent = TRUE)
    if(length(neg)==0){
      neg=NA
    }
    fullurl = paste("https://www.youtube.com/watch?v=",id, sep="")
    #Saves output into a df and appends the data to the final df
    temp.df = data.frame(id, date, title, duration, description, mins, secs, views, pos, neg, fullurl)
    youtube_df = rbind(youtube_df, temp.df)
    
    #Empties all the fields before creating a new entry 
    temp.df = data.frame(id="", date="", title="", duration="", mins="", secs="", description="",  
                         views= "", pos="", neg="", fullurl="")
    #Clear all temp variables    
    remove(id, date, title, duration, description,views, pos, neg, fullurl, mins, secs)
  }

}
#Delete temporary df
remove(temp.df, youtube_url, i, youtube_list)

# Now there is an object called youtube_df, with all the videos




connection <- file('../credentials/password.txt')
password <- readLines(connection)
close(connection)
# Define function for sending emails
sendify <- function(df){
  
  # Define body
  body <- paste0(
    'Dear Dr. ', 
    df$last_name,
    '\n\n',
    'I found your name and email through your article "',
    df$title,
    '", published in ',
    df$year,
    '.\n\n',
    'For my PhD research on the economics of malaria, I am conducting a ',
    'survey of researchers. The aim is to use a "wisdom of crowds" ',
    'approach to gauge the likelihood and timeframe of eradication. Given your research/publication history, ',
    'I was hoping you would have two minutes or so ',
    'to answer a few questions. The survey is at ',
    'https://goo.gl/forms/IroAEooDuJ6KM5Ho2 .\n\n',
    'Thank you very much for your time. If you have any questions, please do not ',
    'hesitate to contact me.\n\n',
    'Best,\n\n',
    'Joe Brew\n',
    'Barcelona Institute for Global Health (www.isglobal.org)\n\n',
    '(P.S. If you want more details on the study I am doing, visit ',
    'https://github.com/joebrew/malaria_survey#can-we-do-it-a-survey-of-research-professionals-on-the-timeline-and-obstacles-to-eliminating-malaria .)'
    
    
  )
  
  # Define subject
  the_subject <- paste0(
    'Hi Dr. ',
    df$last_name,
    ' - questions about malaria eradication'
  )
  
  # Define sender
  if(i %% 2 == 1){
    sender <- 'joebrew@gmail.com'
  } else {
    sender <- 'joe.brew@isglobal.org'
  }
  
  # Define passowrd
  if(sender == 'joebrew@gmail.com'){
    password <- password
  } else {
    password <- password2
  }
  send.mail(from = sender,
            to = as.character(df$email),
            subject = the_subject,
            body = body,
            smtp = list(host.name = "smtp.gmail.com", 
                        port = 465, 
                        # user.name="joebrew@gmail.com", 
                        user.name = sender,
                        passwd=password, 
                        ssl=TRUE),
            authenticate = TRUE,
            # attach.files = '../in_kind_proposal.pdf',
            send = TRUE)
}