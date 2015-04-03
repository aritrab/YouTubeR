#'This a general retry function with a wrapper with try() and trycatch(). 
#'In general cases this functions is to be used with readLines or 
#'rjson::fromJSON. 
#'
#'@param .FUN is a function which is going to be executed. 
#'
#'@return it returns API content 
#'@export

retry <- function(.FUN,max.attempts=5,sleep.seconds=60) 
{
  x <- NULL
  for (i in 1:max.attempts)
  {
    f <- substitute(.FUN)
    x <- try(eval(f))
    if (class(x) == "try-error")
    {
      #print("Readline Error")
      #Sys.sleep(sleep.seconds)
      x <- try(eval(f))
    }
    else
    {
      return (x)
    }
  }
  x
}