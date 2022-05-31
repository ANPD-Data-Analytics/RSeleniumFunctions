# Amazon Vendor Central Date Pick Function ####
# Variables Needed
# #### Define Dates #### 
# startdate <- format(prevweekday(prevweekday(Sys.Date(), 1) - weeks(1), 1), "%Y-%m-%d")
# enddate <- format(prevweekday(Sys.Date(), 7), "%Y-%m-%d")
# 
# #### Input Dates ####   
# # Open up Calendar Date Picker
# webElement.date.picker <- remDr$findElements(using = 'class', "date-picker-input")
# webElement.date.picker[[1]]$clickElement()
# 
# # Find Month Currently in Display
# webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
# current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
# current.month.check <- lubridate::my(current.month.name)
# 
# # Define Dates --- For weekly, can click same start day 2x or Start/End too
# report.date.start <- format(as.Date(startdate))
# report.date.end <- format(as.Date(enddate))
# 
# report.month <- format(as.Date(startdate), "%B %Y")
# report.month.end <- format(as.Date(enddate), "%B %Y")
# report.month.check <- lubridate::my(report.month)
# report.month.end.check <- lubridate::my(report.month.end)
# 
# report.date.end.find <- paste0("//*[@aria-label='day-", as.character(mday(report.date.end)), "'and not(contains(@class, 'react-datepicker__day--outside-month')) and not(contains(@class, 'react-datepicker__day--disabled'))]")
# report.date.start.find<-paste0("//*[@aria-label='day-", as.character(mday(report.date.start)), "'and not(contains(@class, 'react-datepicker__day--outside-month')) and not(contains(@class, 'react-datepicker__day--disabled'))]")

# Inventory Walk
AMZ.VC.Walk2Dates.Inv <- function() {
  # Walk to Start Month, Start Date ####
  Message2Log(paste0("Setting start day to - ", as.character(report.date.start)))
  if (current.month.check > format(report.month.check)) {
    while (current.month.check != format(report.month.check)) {
      webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[contains(@class, 'react-datepicker__navigation--previous')]")
      webElement.date.picker[[1]]$clickElement()
      Sys.sleep(2)
      
      webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
      current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
      current.month.check <- lubridate::my(current.month.name)
    }
  } else if (current.month.check < format(report.month.check)) {
    while (current.month.check != format(report.month.check)) {
      webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[contains(@class, 'react-datepicker__navigation--next')]")
      webElement.date.picker[[1]]$clickElement()
      Sys.sleep(2)
      
      webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
      current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
      current.month.check <- lubridate::my(current.month.name)
    }
  } else  {
    if(current.month.check == format(report.month.end.check)) {
        # Set Date
        webElement.days <- remDr$findElements(using = 'xpath', report.date.start.find)
        webElement.days[[1]]$clickElement()
      }
  }
  
  # Walk to End Month, End Date ####
Message2Log(paste0("Setting end day to - ", as.character(report.date.end)))
  
  # Open up Calendar Date Picker - End Date ####
  webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[@id='dashboard-filter-periodPicker']/div/div/div[3]/input")
  webElement.date.picker[[1]]$clickElement()
  Sys.sleep(2)
  
  if (current.month.check > format(report.month.end.check)) {
    while (current.month.check != format(report.month.end.check)) {
      webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[contains(@class, 'react-datepicker__navigation--previous')]")
      webElement.date.picker[[1]]$clickElement()
      Sys.sleep(2)
      
      webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
      current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
      current.month.check <- lubridate::my(current.month.name)
    }
    
  } else if (current.month.check < format(report.month.end.check)) {
    while (current.month.check != format(report.month.end.check)) {
      webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[contains(@class, 'react-datepicker__navigation--next')]")
      webElement.date.picker[[1]]$clickElement()
      Sys.sleep(2)
      
      webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
      current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
      current.month.check <- lubridate::my(current.month.name)
    }
    
  } else  {
    if (current.month.check == format(report.month.end.check)) {
      # Set Date
      webElement.days <- remDr$findElements(using = 'xpath', report.date.end.find)
      webElement.days[[1]]$clickElement()
      
    }
  } 
}

# Sales Daily
AMZ.VC.Walk2Dates.SD <- function() {
  # Walk to Start Month, Start Date ####
  Message2Log(paste0("Setting start day to - ", as.character(report.date.start)))
  if (current.month.check > format(report.month.check)) {
    while (current.month.check != format(report.month.check)) {
      webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[contains(@class, 'react-datepicker__navigation--previous')]")
      webElement.date.picker[[1]]$clickElement()
      Sys.sleep(2)
      
      webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
      current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
      current.month.check <- lubridate::my(current.month.name)
    }
    
  } else if (current.month.check < format(report.month.check)) {
    while (current.month.check != format(report.month.check)) {
      webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[contains(@class, 'react-datepicker__navigation--next')]")
      webElement.date.picker[[1]]$clickElement()
      Sys.sleep(2)
      
      webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
      current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
      current.month.check <- lubridate::my(current.month.name)
    }
  }
    
   if (current.month.check == format(report.month.check)) {
      webElement.days <- remDr$findElements(using = 'xpath', report.date.start.find)
      webElement.days[[1]]$clickElement()
      Sys.sleep(2)
      
      webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
      current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
      current.month.check <- lubridate::my(current.month.name)
   }
  
  # Walk to End Month, End Date ####
  Message2Log(paste0("Setting end day to - ", as.character(report.date.end)))
  if (current.month.check > format(report.month.end.check)) {
    while (current.month.check != format(report.month.end.check)) {
      webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[contains(@class, 'react-datepicker__navigation--previous')]")
      webElement.date.picker[[1]]$clickElement()
      Sys.sleep(2)
      
      webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
      current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
      current.month.check <- lubridate::my(current.month.name)
    }
  } else if (current.month.check < format(report.month.end.check)) {
    while (current.month.check != format(report.month.end.check)) {
      webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[contains(@class, 'react-datepicker__navigation--next')]")
      webElement.date.picker[[1]]$clickElement()
      Sys.sleep(2)
      
      webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
      current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
      current.month.check <- lubridate::my(current.month.name)
    }
  }
  if (current.month.check == format(report.month.end.check)) {
      webElement.days <- remDr$findElements(using = 'xpath', report.date.end.find)
      webElement.days[[1]]$clickElement()
    }
  }

# Sales Weekly
AMZ.VC.Walk2Dates.Weekly <- function() {
  # Walk to Start Month, Start Date ####
Message2Log(paste0("Setting start day to - ", as.character(report.date.start)))
  if (current.month.check > format(report.month.check)) {
    while (current.month.check != format(report.month.check)) {
      webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[contains(@class, 'react-datepicker__navigation--previous')]")
      webElement.date.picker[[1]]$clickElement()
      Sys.sleep(2)
      
      webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
      current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
      current.month.check <- lubridate::my(current.month.name)
    }
    
  } else if (current.month.check < format(report.month.check)) {
    while (current.month.check != format(report.month.check)) {
      webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[contains(@class, 'react-datepicker__navigation--next')]")
      webElement.date.picker[[1]]$clickElement()
      Sys.sleep(2)
      
      webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
      current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
      current.month.check <- lubridate::my(current.month.name)
    }
    
  } else {
    if (current.month.check == format(report.month.check)) {
      webElement.days <- remDr$findElements(using = 'xpath', report.date.start.find)
      try(
      webElement.days[[1]]$clickElement()
      ,silent = TRUE)
    }
  }

  # Walk to End Month, End Date ####
Message2Log(paste0("Setting end day to - ", as.character(report.date.end)))
  
  # Open up Calendar Date Picker - End Date ####
  webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[@id='dashboard-filter-periodPicker']/div/div/div[3]/input")
  webElement.date.picker[[1]]$clickElement()
  
  if (current.month.check > format(report.month.end.check) & (mday(report.date.end)<="7")) {
    while (current.month.check != format(report.month.end.check)) {
      webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[contains(@class, 'react-datepicker__navigation--previous')]")
      webElement.date.picker[[1]]$clickElement()
      Sys.sleep(2)
      
      webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
      current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
      current.month.check <- lubridate::my(current.month.name)
    }
  } else if (current.month.check < format(report.month.end.check) & (mday(report.date.end)>="7")) {
    while (current.month.check != format(report.month.end.check)) {
      webElement.date.picker <- remDr$findElements(using = 'xpath', "//*[contains(@class, 'react-datepicker__navigation--next')]")
      webElement.date.picker[[1]]$clickElement()
      Sys.sleep(2)
      
      webElement.current.month <- remDr$findElements(using = 'class', 'react-datepicker__current-month')
      current.month.name <- as.character(webElement.current.month[[1]]$getElementText())
      current.month.check <- lubridate::my(current.month.name)
    }
  } else {
    if (current.month.check <= format(report.month.end.check)) {
      webElement.days <- remDr$findElements(using = 'xpath', report.date.end.find)
      try(
      webElement.days[[1]]$clickElement()
      ,silent = TRUE)
    }
  }
}

# Message to Log function #### 
Message2Log <- function(M) { 
  # Log
  log_string <- paste(now(), (M))
  cat(log_string, file = log_file, sep="\n") 
  # Message in Console
  message(M)
}
#Ex.
#log_file <- file("X:\\Data\\D107641_LA\\CDA_Shr_fldr\\Retail Analytics\\Automation Script Logs\\Vendor Central Log.txt", open="a")

# Function receives Chrome Driver Information ####
getChromeVersion <- function() {
  chrome_version <-
    system2(command = "wmic",
            args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
            stdout = TRUE,
            stderr = TRUE) %>%
    stringr::str_extract(pattern = "(?<=Version=)(\\d+\\.){3}") %>%
    magrittr::extract(!is.na(.))
  
  versions = binman::list_versions("chromedriver")
  
  chrome_version %>%
    magrittr::extract(!is.na(.)) %>%
    stringr::str_replace_all(pattern = "\\.",
                             replacement = "\\\\.") %>%
    paste0("^",  .) %>%
    stringr::str_subset(string = dplyr::last(versions)) %>%
    as.numeric_version() %>%
    max() %>%
    as.character()
}

# Set start and end date function ####
prevweekday <- function(date, wday) { 
  date <- as.Date(date)
  diff <- wday - wday(date)
    if (diff > 0)
    diff <- diff - 7
return(date + diff)  }

# Start Selenium Function ####
start_selenium <- function(attempted = 0, condition = "Success starting Selenium web driver!", browserpreference = "chrome"){
  if(!require(netstat)){
    install.packages("netstat")
    library(netstat)
  }
  
  if(!require(RSelenium)){
    install.packages("RSelenium")
    library(RSelenium)
  }
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  
  if(!require(stringr)){
    install.packages("stringr")
    library(stringr)
  }
  
  if(!require(binman)){
    install.packages("binman")
    library(binman)
  }
  
  if(!require(wdman)){
    install.packages("wdman")
    library(wdman)
  }
  
  if(attempted >= 2){
    return("Failure starting Selenium!")
  }
  
  tryCatch({
    # Create Java Task List Before Starting Selenium
    .GlobalEnv$before.tasklist <-  system2("tasklist", stdout = TRUE )
    .GlobalEnv$before.tasklist <- before.tasklist[-(1:3)]
    .GlobalEnv$df <- as.data.frame(before.tasklist)
    .GlobalEnv$df$taskname <- trimws(substr(before.tasklist, 1, 29))
    .GlobalEnv$df$pid <- as.integer(substr(before.tasklist, 30, 34))
    .GlobalEnv$df$before.tasklist <- NULL
    .GlobalEnv$df.java.before <- df[df$taskname == 'java.exe', ]
    
    # Define args and Connect to Chrome
    .GlobalEnv$chrome.version<-getChromeVersion()
    .GlobalEnv$eCaps <- list(chromeOptions = list(args = c('--disable-gpu'
                                                           , '--no-sandbox'
                                                           , '--start-maximized'
                                                           ,'--disable-blink-features'
                                                           , '--disable-blink-features=AutomationControlled'
                                                           ,'enable-automation'
                                                           #,'--headless'                                     # hides browser
                                                           #,'--disable-dev-shm-usage'
                                                           ,'--disable-browser-side-navigation'
                                                           ,'--dns-prefetch-disable')))
    .GlobalEnv$driver <- rsDriver(browser=c("chrome"), chromever= getChromeVersion(), port= free_port(), extraCapabilities=eCaps)
    Sys.sleep(2)
    .GlobalEnv$remDr <- driver[['client']]
    
    # Message Port
    message(c("Using Port : ",driver$client$port))
    
    # Create Java Task List After Starting Selenium
    .GlobalEnv$after.tasklist <-  system2("tasklist", stdout = TRUE )
    .GlobalEnv$after.tasklist <- after.tasklist[-(1:3)]
    .GlobalEnv$df <- as.data.frame(after.tasklist)
    .GlobalEnv$df$taskname <- trimws(substr(after.tasklist, 1, 29))
    .GlobalEnv$df$pid <- as.integer(substr(after.tasklist, 30, 34))
    .GlobalEnv$df$after.tasklist <- NULL
    .GlobalEnv$df.java.after <- df[df$taskname == 'java.exe', ]
    
    # Find the new Java Tast
    .GlobalEnv$new.java <- subset(df.java.after, !(df.java.after$pid %in% df.java.before$pid))
    
    # Remove
    .GlobalEnv$df <- NULL
    .GlobalEnv$after.tasklist <- NULL
    .GlobalEnv$df.java.before <- NULL
    
    # Close Startup Window
    remDr$closeWindow()
    
  }
  , error = function(error_condition) {
    if(grepl("already in use",error_condition, fixed = TRUE)){
      tryCatch(driver$close(),error = function(error_condition){message(error_condition)})
      rD[["server"]]$stop()
      attempted <- attempted + 1
      condition <<- start_selenium(attempted)
    }
  })
  return(condition)
}

# Kill New Java Task Function - Determined in start_selenium()
KillCurrentJava <- function(x) {
  for (j in 1:nrow(new.java)){
    pid.to.kill <- as.integer(new.java$pid[j])  
    message(c("Killing Pid - ", pid.to.kill))
    taskkill.cmd <- paste( "taskkill" , "/F /PID" , pid.to.kill)
    system( taskkill.cmd )
  }
}

# Retry Element until No Error, Then Click ####
RetryElement <- function(using,value){
  if(!require(retry)){
    install.packages("retry")
    library(retry)
  }
  .GlobalEnv$click <- retry(driver$client$findElement(using = using , value = value), 
                            until = ~remDr$status[1]=='0',
                            timeout = 6000, 
                            silent = TRUE)
  .GlobalEnv$click$clickElement()
}

# Retry Element until No Error ####
RetryElementxClick <- function(using,value){
  if(!require(retry)){
    install.packages("retry")
    library(retry)
  }
  .GlobalEnv$click <- retry(driver$client$findElement(using = using , value = value), 
                            until = ~remDr$status[1]=='0',
                            timeout = 6000, 
                            silent = TRUE)
}
