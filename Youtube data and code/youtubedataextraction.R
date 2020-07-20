############################ VIDEO ID EXTRACION ########################################
rm(list= ls())
library(rvest)
library(purrr)
Moviedataset<- read.csv("movies2006.csv", sep = ",")
Moviedataset <- Moviedataset[(Moviedataset$year <=2006),]
Moviedataset_Name <- gsub(" ","+",Moviedataset$name)
Linkdataset <- NULL

for(i in Moviedataset_Name){
  url_base <- "https://www.youtube.com/results?search_query=%s"
  url_base<- sprintf(url_base,i)
  print(url_base)
  pg <- read_html(url_base, handle = curl::new_handle("useragent" = "Mozilla/5.0"))
  links <- pg %>% html_nodes("a") %>% html_attr("href")
  print(links[42])
  Linkdataset <- rbind(Linkdataset,data.frame(Youtube_Name = i,YoutubeLink = links[42]))
  Sys.sleep(sample(10, 1) * 0.1)
}
Linkdataset_backup <- Linkdataset
write.csv(Linkdataset,file = "LinkDataset.csv")



############################ LIKES/DISLIKES/VIEWCOUNT ###################################
rm(list= ls())
install.packages("tuber")
install.packages("httpuv")
library (tuber)
library (httpuv)
app_id <- "663405963637-cd1q2b2b7td4qe1905acrtoeesg7gkr9.apps.googleusercontent.com"
app_secret <- "v49cK3A-HivRSsYObJ5ShOv2"
yt_oauth(app_id, app_secret,token = '')
Linksdataset <- read.csv("LinkDataset.csv",sep= ",")
video_id <- as.character(Linksdataset$Youtube_link)
datasetofYoutube <- NULL
for(i in video_id)
{
  print(i)
  Statsvideo <- get_stats(video_id = i)
  if(is.null(Statsvideo$likeCount))
    likecount<- 0
  else
    likecount<- Statsvideo$likeCount
  
  if(is.null(Statsvideo$dislikeCount))
    dislikecount<- 0
  else
    dislikecount<- Statsvideo$dislikeCount
  
  if(is.null(Statsvideo$viewCount))
    viewcount <- 0
  else
    viewcount <- Statsvideo$viewCount
  
  datasetofYoutube <- rbind(datasetofYoutube, data.frame(LinkID = i,
                                    Likes = likecount,
                                    Dislike = dislikecount,
									Viewcount <- viewcount))
  Sys.sleep(sample(20, 1) * 0.1)
}
write.csv(datasetofYoutube,file = "Youtbedataset.csv")