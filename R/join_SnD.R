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
  df<-as.data.frame(df);df<-dplyr::tbl_df(df);df[is.na(df)]<-0;
  df$CommentsCount<-as.numeric(as.character(df$CommentsCount)); df$FavCount<-as.numeric(as.character(df$FavCount)); df$ViewConunt<-as.numeric(as.character(df$ViewConunt)); df$DislikeCount<-as.numeric(as.character(df$DislikeCount)); df$LikesCount<-as.numeric(as.character(df$LikesCount));df$AvgRt<-as.numeric(as.character(df$AvgRt));
  df$NoodRaters<-as.numeric(as.character(df$NoodRaters));df$FBShareCount<-as.numeric(as.character(df$FBShareCount));df$FBLikeCount<-as.numeric(as.character(df$FBLikeCount));df$FBCommentsCount<-as.numeric(as.character(df$FBCommentsCount));df$TwitterShareCount<-as.numeric(as.character(df$TwitterShareCount))
  return(df)
}