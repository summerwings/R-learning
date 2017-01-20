packagelist <- function(package_list_order = "date",keyword = ""){
  if (package_list_order == "date") 
  {      
    url <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"
    webpage <- read_html(url,encoding="ISO-8859-1")
    the_data <- webpage %>% html_nodes("td") %>% html_text()
    n <- length(the_data)
    i <- 1
    num_of_package <- NULL
    num_of_package[1] <- 2
    repeat{
      num_of_package[i+1] <- num_of_package[i]+3
      i <- i+1
      if (num_of_package[i] >= n-2) break
    }
    names <- the_data[c(num_of_package)]
    dis <- the_data[c(num_of_package+1)]
    dates <- the_data[c(num_of_package+2)]
    package_frame <- data.frame(date = dates, package = names,title = dis)
    frame_result1 = package_frame[grep(keyword,package_frame[,1]),] 
    frame_result2 = package_frame[grep(keyword,package_frame[,2]),]
    frame_result3 = package_frame[grep(keyword,package_frame[,3]),]
    frame_result <- rbind(frame_result1,frame_result2,frame_result3)
    frame_final <- unique(frame_result)
    frame_final <- frame_final[order( frame_final[,1]),]
    return(frame_final)
  }    
  
  if (package_list_order == "name") 
  {
    url <- "https://cran.r-project.org/web/packages/available_packages_by_name.html"
    webpage <- read_html(url,encoding="ISO-8859-1")
    the_data <- webpage %>% html_nodes("td") %>% html_text()
    n <- length(the_data)
    i <- 1
    num_of_package <- NULL
    num_of_package[1] <- 2
    repeat{
      num_of_package[i+1] <- num_of_package[i]+2
      i <- i+1
      if (num_of_package[i] >= n-1) break
    }
    names <- the_data[c(num_of_package)]
    dis <- the_data[c(num_of_package+1)]
    package_frame <- data.frame(package = names,title = dis)
    frame_result1 = package_frame[grep(keyword,package_frame[,1]),] 
    frame_result2 = package_frame[grep(keyword,package_frame[,2]),]
    frame_result <- rbind(frame_result1,frame_result2)
    frame_final <- unique(frame_result)
    frame_final <- frame_final[-order( frame_final[,1]),]
    return(frame_final)
  }
  
  else {
    "YOU HAVE INPUT AN INVALID VALUE"
  }  
}
