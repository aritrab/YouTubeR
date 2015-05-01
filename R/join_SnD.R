#' description
#' @param df1 is a static dataframe obtained from YouTube
#' @param df2 is a dataframe of dynamic data obtained from YouTube
#' @return df combined dataframe about a channel 
#' @export 
#' 

join_SnD<-function(df1,df2){
  df<-cbind.data.frame(df1,df2)
  names(df)<-c(1:32)
  df<-df%>%
    select(-9,-10,-12:-16,-18:-20)
  names(df)<-c("APIURL","DOP","Catagory","Subcatagory","Title","Details","videoURL","ChannelName","UserID","CommentsCount","FavCount","ViewConunt","DislikeCount","LikesCount","UploaderID","VideoID","AvgRt","NoodRaters","FBShareCount","FBLikeCount","FBCommentsCount","TwitterShareCount")
  return(df)
}