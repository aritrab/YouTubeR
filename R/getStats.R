#' getStats
#' 
#' @param videoid
#' @return dataset.ls
#' @export


getStats <- function(videoid){
  url=paste0("https://gdata.youtube.com/feeds/api/videos/",videoid,"?v=2&alt=json")
  raw.data <- retry(httr::GET(paste0(url)),url)
  raw.data<-retry_raw_data(httr::content(raw.data, "text"),raw.data)
  rd<-retryfromJSON(rjson::fromJSON(raw.data),raw.data,httr::content(raw.data, "text"),url,httr::GET(paste0(url)))
  userId<-rd$entry$author[[1]]$`yt$userId`$`$t`
  dop<-as.character(rd$entry$published)
  term<-rd$entry$category[[2]]$term
  label<-rd$entry$category[[2]]$label
  title<-rd$entry$`media$group`$`media$title`$`$t`
  description<-rd$entry$`media$group`$`media$description`$`$t`
  comment.countHint<-rd$entry$`gd$comments`$`gd$feedLink`$countHint
  link<-rd$entry$link[[3]]$href
  author<-rd$entry$author[[1]]$name$`$t`
  duration<-as.numeric(rd$entry$`media$group`$`media$content`[[3]]$duration)
  favs <- rd$entry$`yt$statistics`["favoriteCount"]
  views <- rd$entry$`yt$statistics`["viewCount"]
  dislikes <- rd$entry$`yt$rating`["numDislikes"]
  likes <- rd$entry$`yt$rating`["numLikes"]
  uploaderID<-rd$entry$`media$group`$`yt$uploaderId`$`$t`
  vidID<-rd$entry$`media$group`$`yt$videoid`$`$t`
  avgrt<-rd$entry$`gd$rating`$average
  noofrater<-rd$entry$`gd$rating`$numRaters
  dataset.df<-fbshare(vidID)
  shared.count<-twittershare(vidID)
  dataset.ls<-list(userId,dop,term,label,title,description,comment.countHint,link,author,duration,favs,views,dislikes,likes,uploaderID,vidID,avgrt,noofrater,dataset.df[[1]][[1]][1],dataset.df[[1]][[1]][2],dataset.df[[1]][[1]][3],shared.count)
  dataset.ls<-as.data.frame(dataset.ls)
  names(dataset.ls)<-c("UserID","DOP","Category","Subcatagory","Title","Description","NoofComments","URL","Author","Duration","FavCount","ViewConunt","DislikeCount","LikesCount","UploaderID","VideoID","AvgRt","NoodRaters","fb_share_count","fb_like_count","fb_comment_count","twitter_share_count")
  return (dataset.ls)}
