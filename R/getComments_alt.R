#' To get all comments 
#' @param x
#' @return dataset.tdf
#' @export 
getComments_alt<-function(x){
                         i<-1;id<-1;j<-1;is<-1;
                         url=paste0("https://gdata.youtube.com/feeds/api/videos/",x,"?v=2&alt=json")
                         raw.data <- retry(httr::GET(paste0(url)),url)
                         raw.data<-retry_raw_data(httr::content(raw.data, "text"),raw.data)
                         data<-retryfromJSON(rjson::fromJSON(raw.data),raw.data,httr::content(raw.data, "text"),url,httr::GET(paste0(url)))
                         n<-as.numeric(data$entry$`gd$comments`$`gd$feedLink`$countHint)
                         rm(url,raw.data,data)  
                         userid<-list();content<-list();youtubeuid<-list();dislike<-list();name<-list();
                         like<-list();author<-list();
                         if(n<50)
                           n=n+50
                         while(i<n){
                             url=paste0("http://www.sandracires.com/en/client/youtube/comments.php?v=",x,"&page=",is,"")
                             raw.data <- retry(httr::GET(paste0(url)),url)
                             raw.data<-retry_raw_data(httr::content(raw.data, "text"),raw.data)
                             data<-retryfromJSON(rjson::fromJSON(raw.data),raw.data,httr::content(raw.data, "text"),url,httr::GET(paste0(url)))
                             while(j<51){
                                 userid[[id]] <- data$list[[j]]$userid
                                 content[[id]]<-data$list[[j]]$text
                                 author[[id]]<-data$list[[j]]$username
                                 like[[id]]<-data$list[[j]]$likes
                                 dislike[[id]]<-data$list[[j]]$dislikes
                               j=j+1
                               id=id+1
                               if(id==n)
                                 break;
                             }
                             is=is+1
                             j=1;
                             i=i+50;
                             print(sprintf("%s no of comments scrapped sucessfully",i))
                             rm(data,raw.data,url)
                             }
                         dataset.ls<-list(userid,content,author,like,dislike)
                         dataset.df<-data.frame(Reduce(cbind,dataset.ls))
                         dataset.tdf<-dplyr::tbl_df(dataset.df)
                         names(dataset.tdf)<-c("UserID","Comments","UserName","Like","Dislike")
                         rm(dataset.ls,dataset.df)
                         return (dataset.tdf)} 