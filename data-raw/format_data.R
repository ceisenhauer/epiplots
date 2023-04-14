#' -------------------------------------------------------------------------------------------------
#' @title : Format and Integrate Internal Data
#
#' @author : Catherine Eisenhauer
#' @date : 2021-02
#'
#' @suggests : dplyr, geojsonio, usethis
#' -------------------------------------------------------------------------------------------------

library(dplyr)

# NATIONAL INCIDENCE BY AGE ------------------------------------------------------------------------
df_age <- rio::import('data-raw/incidence_france.csv',
                     sep = ';',
                     header = TRUE) %>%
          select(date, age, pop, p) %>%
          filter(age > 0) %>%
          mutate(age = recode(age,
                              `9` = '0 - 9',
                              `19` = '10 - 19',
                              `29` = '20 - 29',
                              `39` = '30 - 39',
                              `49` = '40 - 49',
                              `59` = '50 - 59',
                              `69` = '60 - 69',
                              `79` = '70 - 79',
                              `89` = '80 - 89',
                              `90` = '90+')) %>%
          mutate(date = as.Date(date)) %>%
          as_tibble()

usethis::use_data(df_age)


# INCIDENCE BY DEPARTMENT --------------------------------------------------------------------------
df_dep <- read.table('data-raw/incidence_department.csv',
                     sep = ';',
                     header = TRUE) %>%
          filter(cl_age90 == 0) %>%
          rename(date = jour,
                 p = P) %>%
          mutate(date = as.Date(date)) %>%
          select(date, dep, pop, p) %>%
          as_tibble()

usethis::use_data(df_dep)

