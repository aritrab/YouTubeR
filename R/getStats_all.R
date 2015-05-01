#' description
#' 
#' @param vidID
#' @return dataset.df
#' @export
#' 
#' 
#' 


getStats_all <- function(name){i<-1;id<-1;j<-1;is<-1;
                               youtubeid<-list();dop<-list();term<-list();label<-list();title<-list();
                               content<-list();link<-list();author<-list();duration<-list();
                               url=paste0("https://gdata.youtube.com/feeds/api/users/",sprintf("%s",name),"/uploads?v=2&alt=jsonc&max-results=0")
                               raw.data <- retry(httr::GET(paste0(url)),url)
                               raw.data<-retry_raw_data(httr::content(raw.data, "text"),raw.data)
                               data<-retryfromJSON(rjson::fromJSON(raw.data),raw.data,httr::content(raw.data, "text"),url,httr::GET(paste0(url)))
                               n<-as.numeric(data$data$totalItems)
                               rm(url,raw.data,data)
                               
                               while(i<n+1){
                                 url=paste0("http://gdata.youtube.com/feeds/api/users/",sprintf("%s",name),"/uploads?max-results=50&start-index=",i,"&alt=json")
                                 raw.data <- retry(httr::GET(paste0(url)),url)
                                 raw.data<-retry_raw_data(httr::content(raw.data, "text"),raw.data)
                                 data<-retryfromJSON(rjson::fromJSON(raw.data),raw.data,httr::content(raw.data, "text"),url,httr::GET(paste0(url)))
                                 while(j<51){
                                   
                                   youtubeid[[id]]<-data$feed$entry[[j]]$id$`$t`
                                   dop[[id]]<-as.character(data$feed$entry[[j]]$published$`$t`)
                                   term[[id]]<-data$feed$entry[[j]]$category[[2]]$term
                                   label[[id]]<-data$feed$entry[[j]]$category[[2]]$label
                                   title[[id]]<-data$feed$entry[[j]]$title$`$t`
                                   content[[id]]<-data$feed$entry[[j]]$content$`$t`
                                   link[[id]]<-data$feed$entry[[j]]$link[[3]]$href
                                   author[[id]]<-data$feed$entry[[j]]$author[[1]]$name$`$t`
                                   duration[[id]]<-as.numeric(data$feed$entry[[j]]$`media$group`$`media$content`[[1]]$duration)
                                   j=j+1
                                   id=id+1
                                   if(id==n)
                                     break;
                                   
                                 }
                                 if(id==n){
                                   rm(data,raw.data,url);
                                   break;
                                 }
                                 rm(data,raw.data,url)
                                 j=1;
                                 i=i+50;
                                 if(i>n)
                                   break;
                                 
                                 print(sprintf("%s video related data scrapped successfully",i))
                               }
                               dataset.ls<-list(youtubeid,dop,term,label,title,content,link,author,duration)
                               dataset.df<-dplyr::tbl_df(as.data.frame(Reduce(cbind,dataset.ls)))
                               dataset.df<-dplyr::mutate(dataset.df,vidID=stringr::str_sub(as.character(dataset.df$V7), start=32))
                               names(dataset.df)<-c("APIURL","DOP","Catagory","Subcategory","Title","Description","YouTubeURL","Author","Duration","VideoID")
                               VideoID<-list();fb_share_count<-list();twitter_share_count<-list();
                               VideoID<-as.list(dataset.df$VideoID)
                               i<-1;userId<-list();dop<-list();term<-list();label<-list();title<-list();description<-list();comment.countHint<-list();link<-list();
                               author<-list();duration<-list();favs <-list();views<-list();dislikes<-list();likes<-list();uploaderID<-list();vidID<-list();avgrt<-list();
                               noofrater<-list();fb_like_count<-list();fb_comment_count<-list();
                              while(i<n+1){
                              url=paste0("https://gdata.youtube.com/feeds/api/videos/",VideoID[[i]],"?v=2&alt=json")
                              raw.data <- retry(httr::GET(paste0(url)),url)
                              raw.data<-retry_raw_data(httr::content(raw.data, "text"),raw.data)
                              rd<-retryfromJSON(rjson::fromJSON(raw.data),raw.data,httr::content(raw.data, "text"),url,httr::GET(paste0(url)))
                              userId[[i]]<-rd$entry$author[[1]]$`yt$userId`$`$t`
                              dop[[i]]<-as.character(rd$entry$published)
                              term[[i]]<-rd$entry$category[[2]]$term
                              label[[i]]<-rd$entry$category[[2]]$label
                              title[[i]]<-rd$entry$`media$group`$`media$title`$`$t`
                              description[[i]]<-rd$entry$`media$group`$`media$description`$`$t`
                              comment.countHint[[i]]<-rd$entry$`gd$comments`$`gd$feedLink`$countHint
                              link[[i]]<-rd$entry$link[[3]]$href
                              author[[i]]<-rd$entry$author[[1]]$name$`$t`
                              duration[[i]]<-as.numeric(rd$entry$`media$group`$`media$content`[[3]]$duration)
                              favs[[i]] <- as.numeric(rd$entry$`yt$statistics`["favoriteCount"])
                              views[[i]] <- as.numeric(rd$entry$`yt$statistics`["viewCount"])
                              dislikes[[i]] <- as.numeric(rd$entry$`yt$rating`["numDislikes"])
                              likes[[i]] <- as.numeric(rd$entry$`yt$rating`["numLikes"])
                              uploaderID[[i]]<-rd$entry$`media$group`$`yt$uploaderId`$`$t`
                              vidID[[i]]<-rd$entry$`media$group`$`yt$videoid`$`$t`
                              avgrt[[i]]<-as.numeric(rd$entry$`gd$rating`$average)
                              noofrater[[i]]<-as.numeric(rd$entry$`gd$rating`$numRaters)
                              dataset.df<-fbshare(VideoID[[i]])
                              fb_share_count[[i]]<-as.numeric(dataset.df[[1]][[1]][1])
                              fb_like_count[[i]]<-as.numeric(dataset.df[[1]][[1]][2])
                              fb_comment_count[[i]]<-as.numeric(dataset.df[[1]][[1]][3])
                              shared.count<-twittershare(VideoID[[i]])
                              twitter_share_count[[i]]<-as.numeric(shared.count)
                              i=i+1
                              if(i==n)
                                break;
                              }
                              dataset.ls<-list(userId,dop,term,label,title,description,comment.countHint,link,author,duration,favs,views,dislikes,likes,uploaderID,vidID,avgrt,noofrater,fb_share_count,fb_like_count,fb_comment_count,twitter_share_count)
                              dataset.df<-dplyr::tbl_df(as.data.frame(Reduce(cbind,dataset.ls)))
                              names(dataset.df)<-c("UserID","DOP","Category","Subcatagory","Title","Description","NoofComments","URL","Author","Duration","FavCount","ViewConunt","DislikeCount","LikesCount","UploaderID","VideoID","AvgRt","NoodRaters","fb_share_count","fb_like_count","fb_comment_count","twitter_share_count")
                              return (dataset.df)}
