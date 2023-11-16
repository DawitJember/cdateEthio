#' Convert date from the Ethiopian calendar to the Gregorian
#'
#' @param a a numeric value of day DD
#' @param b a numeric value of month MM
#' @param c a numeric value of year YYYY
#' @param id a numeric value of individual
#'
#' @return a date value
#' @export
#'
#' @examples
#' #To convert date, month, year from a data set
#'
#' davedate(a=data$day, b=data$month, c=data$year, id=data$id)
davedate <- function(a, b, c, id) {

  date <- numeric(length(a))
  month <- numeric(length(a))
  year <- numeric(length(a))

  a_na <- is.na(a)
  b_na <- is.na(b)
  c_na <- is.na(c)

  date[!b_na & b %in% 1:2] <- a[!b_na & b %in% 1:2] + 10
  date[!b_na & b %in% c(3,4,7)] <- a[!b_na & b %in% c(3,4,7)] + 9
  date[!b_na & b %in% c(5,8,9)] <- a[!b_na & b %in% c(5,8,9)] + 8
  date[!b_na & b %in% c(6,10,11)] <- a[!b_na & b %in% c(6,10,11)] + 7
  date[!b_na & b == 12] <- a[!b_na & b == 12] + 6
  date[!b_na & b == 13] <- a[!b_na & b == 13] + 5

  month[!b_na & b %in% 1:3] <- b[!b_na & b %in% 1:3] + 8
  month[!b_na & !a_na & b == 4 & a <23]<-b[!b_na & !a_na & b == 4 & a <23] + 8
  month[!b_na & b %in% 5:13] <- b[!b_na & b %in% 5:13] - 4
  month[!b_na & !a_na & b == 4 & a >=23] <- 1

  year[!b_na & b %in% 1:3] <- c[!b_na & b %in% 1:3] + 7
  year[!b_na & !a_na & b == 4 & a <23] <- c[!b_na & !a_na & b == 4 & a <23] + 7
  year[!b_na & !a_na & b == 4 & a >= 23] <- c[!b_na & !a_na & b == 4 & a >= 23] + 8
  year[!b_na & b %in% 5:13] <- c[!b_na & b %in% 5:13] + 8

  date[!b_na & !a_na & date>30 & month==9] <- date[!b_na & !a_na & date>30 & month==9]-30
  date[!b_na & !a_na & date>31 & month==10] <- date[!b_na & !a_na & date>31 & month==10]-31

  date[!b_na & !a_na & date>30 & month==11] <- date[!b_na & !a_na & date>30 & month==11]-30
  date[!b_na & !a_na & date>31 & month==12] <- date[!b_na & !a_na & date>31 & month==12]-31
  date[!b_na & !a_na & date>31 & month==1] <- date[!b_na & !a_na & date>31 & month==1]-31
  date[!b_na & !a_na & date>29 & month==2] <- date[!b_na & !a_na & date>29 & month==2]-29
  date[!b_na & !a_na & date>31 & month==3] <- date[!b_na & !a_na & date>31 & month==3]-31
  date[!b_na & !a_na & date>30 & month==4] <- date[!b_na & !a_na & date>30 & month==4]-30
  date[!b_na & !a_na & date>31 & month==5] <- date[!b_na & !a_na & date>31 & month==5]-31
  date[!b_na & !a_na & date>30 & month==6] <- date[!b_na & !a_na & date>30 & month==6]-30
  date[!b_na & !a_na & date>31 & month==7] <- date[!b_na & !a_na & date>31 & month==7]-31
  date[!b_na & !a_na & date>30 & month==8] <- date[!b_na & !a_na & date>30 & month==8]-30


  month[!b_na & !a_na & date>30 & month==9] <- month[!b_na & !a_na & date>30 & month==9]<-10
  month[!b_na & !a_na & date>31 & month==10] <- month[!b_na & !a_na & date>31 & month==10]<-11
  month[!b_na & !a_na & date>30 & month==11] <- month[!b_na & !a_na & date>30 & month==11]<-12
  month[!b_na & !a_na & date>31 & month==1] <- month[!b_na & !a_na & date>31 & month==1]<-2
  month[!b_na & !a_na & date>29 & month==2] <- month[!b_na & !a_na & date>29 & month==2]<-3
  month[!b_na & !a_na & date>31 & month==3] <- month[!b_na & !a_na & date>31 & month==3]<-4
  month[!b_na & !a_na & date>30 & month==4] <- month[!b_na & !a_na & date>30 & month==4]<-5
  month[!b_na & !a_na & date>31 & month==5] <- month[!b_na & !a_na & date>31 & month==5]<-6
  month[!b_na & !a_na & date>30 & month==6] <- month[!b_na & !a_na & date>30 & month==6]<-7
  month[!b_na & !a_na & date>31 & month==7] <- month[!b_na & !a_na & date>31 & month==7]<-8
  month[!b_na & !a_na & date>30 & month==8] <- month[!b_na & !a_na & date>30 & month==8]<-9

  date_str <- paste(year, month, date, sep = "-")
  date <- as.Date(date_str, format = "%Y-%m-%d")

  return(data.frame(date = date, id))

}
