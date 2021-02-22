suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))

# get command line argument "monday" as "yyyy-mm-dd"
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    stop("You need to specify the Monday of the week you're interested in as yyyy-mm-dd, for example:\n\nRscript AutomatedClockify.R 2019-12-25\n\n", call. = FALSE)
  } 
  # only getting first argument here
  return(args[1])
}

monday <- main()

# max number of results to return
maxresults <- 5000



# percentages from timesheet
me <- c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 128, 104, 114, 204, 128, 100, 97, 97, 86, 40, 20, 100, 100, 88, 110, 115, 89, 113.39, 127.74, 153.33, 112.56, 115.52, 117.29, 86.48, 99.70, 78.40, 55.05, 95.95, 101.54, 113.82, 132.88, 114.72,104.33,112.36, 104.76, 92.06, 95.95)


#First, let's get to the json data frame of the timesheet.
sunday <-  ymd(as_datetime(monday) + days(6))
startdate <- paste0(monday, "T00:00:00.998Z")
enddate <- paste0(sunday, "T23:59:02Z")
# from account page
clockifyAPIkey <- Sys.getenv("clockifyAPIkey")
# from clockify/workspaces
myworkspaceid <- Sys.getenv("clockifyworkspace")
# my user id
# from curl -H "content-type: application/json" -H "X-Api-Key: XYILtXquvAHjPFA6" -X GET https://api.clockify.me/api/v1/user
myuserid <- Sys.getenv("clockifyuserid")
# need headers for the API
#
#
#
## define function for projects/tags
get_projTags <- function(string = 'tags'){
  # this works for tags AND projects
  whatIwanturl <- stringr::str_c(
    'https://api.clockify.me/api/v1/workspaces/',
    myworkspaceid,
    '/',
    # this can be tags or projects
    string
  )
  whatIwant_json <-
    GET(whatIwanturl,
      add_headers("content-type" = "application/json",
      "X-Api-Key" = clockifyAPIkey),
      query = list("page-size" = maxresults),
      accept_json())
  whatIwant_df <- jsonlite::fromJSON(content(whatIwant_json, "text"))
  return(whatIwant_df)
}

## define function for tasks from projects
get_tasks <- function(projectid){
  # this works for tags AND projects
  whatIwanturl <- stringr::str_c(
    'https://api.clockify.me/api/v1/workspaces/',
    myworkspaceid,
    '/',
    # this can be tags or projects
    'projects/',
    projectid,
    '/tasks'
  )
  whatIwant_json <-
    GET(whatIwanturl,
      add_headers("content-type" = "application/json",
      "X-Api-Key" = clockifyAPIkey),
      query = list("page-size" = maxresults),
      accept_json())
  whatIwant_df <- jsonlite::fromJSON(content(whatIwant_json, "text"))
  return(whatIwant_df)
}


## get tags and projects ------------------------
tags_df <- get_projTags(string = 'tags')
projects_df <- get_projTags(string = 'projects')


# get timesheet --------------
timesheeturl <- stringr::str_c(
  'https://api.clockify.me/api/v1/workspaces/',
  myworkspaceid,
  '/user/',
  myuserid,
  '/time-entries'
)
timesheet_json <-
  GET(timesheeturl,
    add_headers("content-type" = "application/json",
    "X-Api-Key" = clockifyAPIkey),
    query = list("start" = startdate,
               "end" = enddate,
               "page-size" = maxresults,
               "in-progress" = "false"),
    accept_json())


timesheet_df <- jsonlite::fromJSON(content(timesheet_json, "text"))
timesheet_timings <- timesheet_df$timeInterval
timesheet_df$timeInterval <- NULL
timesheet_df <- cbind(timesheet_df, timesheet_timings) %>%  tidyr::unnest_longer(tagIds) %>% rename(autoduration = duration)





# filter for essentials
tags_df_clean <- tags_df %>% dplyr::select(id, name) %>% dplyr::rename(tag_id = id, tag_name = name)
projects_df_clean <- projects_df %>% dplyr::select(id, name, color, archived, duration) %>%
  dplyr::rename(project_id = id, project_name = name, project_archived = archived, project_duration = duration)
timesheet_df_clean <- timesheet_df %>% dplyr::select(id, description, tagIds, taskId, projectId, start,end, autoduration ) %>%
  rename(timesheet_id = id, timesheet_description  = description )

# get tasks
tasks <- data.frame(matrix(ncol = 6, nrow = 0))
names(tasks) <- c("id", "name", "projectId", "assigneeId", "estimate", "status")
for (i in 1:nrow(projects_df)) {
  if (class(get_tasks(projects_df$id[i])) == "data.frame") {
      tmp2 <- get_tasks(projectid = projects_df$id[i])
      tasks <- rbind(tasks, tmp2)
      rm(tmp2)
  }
}
tasks_df_clean <- tasks %>% dplyr::select(id, name) %>% dplyr::rename(task_id = id, task_name = name)




timesheet_tags <- dplyr::left_join(timesheet_df_clean, tags_df_clean, by = c("tagIds" = "tag_id"))
  




timesheet_tags_tasks <- merge.data.frame(timesheet_tags,
                                         tasks_df_clean,
                                         by.x = "taskId",
                                         by.y = "task_id",
                                         all.x = TRUE)

all_data_together <- merge.data.frame(timesheet_tags_tasks,
                                      projects_df_clean,
                                      by.x = "projectId",
                                      by.y = "project_id",
                                      all.x = TRUE) %>%
  mutate(start = as_datetime(start, tz = "Australia/Sydney"),
         end = as_datetime(end, tz = "Australia/Sydney"),
         myduration = (start %--% end)/dseconds(x = 1))

rm(timesheet_tags, timesheet_tags_tasks)

# How much did I work (excluding projects)?

print("How much did I work (exluding projects)?")
all_data_together %>%
  dplyr::select(tag_name, myduration) %>%
  group_by(tag_name) %>%
  summarise(totalSec = sum(myduration)) %>%
  mutate(hours = totalSec %/% 3600, minutes = (totalSec %% 3600)/60 ) %>%
  mutate(propWeek = totalSec/(35*60*60))


# How much did I work on each project?

print("How much did I work on each project?")
all_data_together %>%
  dplyr::filter(tag_name == "Projects") %>%
  dplyr::select(project_name, myduration) %>%
  group_by(project_name) %>%
  summarise(totalSec = sum(myduration)) %>%
  mutate(hours = totalSec %/% 3600, minutes = (totalSec %% 3600)/60 ) %>%
  mutate(propWeek = totalSec/(35*60*60))

print("################################################")
print("################################################")
print("################################################")
print("How much did I work on each training subproject?")
all_data_together %>%
  dplyr::filter(substr(tag_name,1,8) == "Teaching") %>%
  dplyr::select(project_name, myduration) %>%
  group_by(project_name) %>%
  summarise(totalSec = sum(myduration)) %>%
  mutate(hours = totalSec %/% 3600, minutes = (totalSec %% 3600)/60 ) %>%
  mutate(propWeek = totalSec/(35*60*60))

print("How did I go with Overhead?")
all_data_together %>% filter(tag_name == "Overhead") %>%
  select(project_name, timesheet_description, start, myduration) %>%
  mutate(myduration = myduration/3600)

#print("Sanity check of all data")
#all_data_together
print("How much did I work out of 35 hours?")
all_data_together %>%
  summarise(totalSec = sum(myduration)) %>%
  mutate(propWeek = totalSec/(35*60*60)) %>%
  dplyr::pull(propWeek)

ggplot2::theme_set(ggplot2::theme_minimal())
all_data_together %>%
  # week starts Monday
  mutate(day = wday(start, week_start = 1),
         start_time = hour(start) + minute(start)/60 ,
         end_time = hour(end) + minute(end)/60) %>%
  ggplot2::ggplot() + ggplot2::geom_rect(ggplot2::aes(
    xmin = day - 0.5, 
    xmax = day + 0.5,
    ymin = hour(start) + minute(start)/60,
    ymax = hour(end) + minute(end)/60,
    fill = project_name)) + 
  ggplot2::scale_y_reverse(breaks = c(8,9,10,11,12,13,14,15,16,17,18)) + 
  ggplot2::scale_x_continuous(
    breaks = c(1,2,3,4,5,6,7),
    labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  ggplot2::labs(title = stringr::str_c("Work diary (by project) for ", format(ymd(monday), format = "%a %b %d, %Y"), " - ", format(ymd(monday) + 6, format = "%a %b %d, %Y") ) , fill = "") + 
  ggplot2::geom_hline(yintercept = 9, lty = 2) + ggplot2::geom_hline(yintercept = 17, lty = 2) + ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(paste0(monday, "_proj.pdf"), height = 4)


ggplot2::theme_set(ggplot2::theme_minimal())
all_data_together %>%
  # week starts Monday
  mutate(day = wday(start, week_start = 1),
         start_time = hour(start) + minute(start)/60 ,
         end_time = hour(end) + minute(end)/60) %>%
  ggplot2::ggplot() + ggplot2::geom_rect(ggplot2::aes(
    xmin = day - 0.5, 
    xmax = day + 0.5,
    ymin = hour(start) + minute(start)/60,
    ymax = hour(end) + minute(end)/60,
    fill = tag_name)) + 
  ggplot2::scale_y_reverse(breaks = c(8,9,10,11,12,13,14,15,16,17,18)) + 
  ggplot2::scale_x_continuous(
    breaks = c(1,2,3,4,5,6,7),
    labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  ggplot2::labs(title = stringr::str_c("Work diary (by tag) for ", format(ymd(monday), format = "%a %b %d, %Y"), " - ", format(ymd(monday) + 6, format = "%a %b %d, %Y") ) , fill = "") + 
  ggplot2::geom_hline(yintercept = 9, lty = 2) + ggplot2::geom_hline(yintercept = 17, lty = 2)

ggplot2::ggsave(paste0(monday, "_tag.pdf"), height = 4)


