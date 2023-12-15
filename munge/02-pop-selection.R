# Inclusion/exclusion criteria --------------------------------------------------------

flow <- flow[1:10, 1:2]

flow <- rbind(c("General inclusion/exclusion criteria", ""), flow)

flow <- rbind(flow, c("Project specific inclusion/exclusion criteria", ""))

rsdata <- rsdata411 %>%
  filter(shf_indexdtm >= ymd("2008-02-01"))
flow <- rbind(flow, c("Exclude posts < 2008-02-01 (Anxiety/depression included in the CRF)", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_anxiety))
flow <- rbind(flow, c("Exclude posts with missing Anxiety/depression", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_ef_cat))
flow <- rbind(flow, c("Exclude posts with missing EF", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post / patient", nrow(rsdata)))

rm(rsdata411)
gc()