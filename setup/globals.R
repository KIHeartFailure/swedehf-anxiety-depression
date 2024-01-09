# default is to use tidyverse functions
select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete

# used for calculation of ci
global_z05 <- qnorm(1 - 0.025)

shfdbpath <- "D:/STATISTIK/Projects/20210525_shfdb4/dm/"
datadate <- "20220908"

global_cols <- RColorBrewer::brewer.pal(7, "Dark2")

global_followup_years <- 5
global_followup <- 5 * 365.25
