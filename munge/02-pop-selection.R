# Inclusion/exclusion criteria --------------------------------------------------------

flow <- flow[c(1:8, 10), 1:2]

names(flow) <- c("Criteria", "N")

flow <- flow %>%
  mutate(Criteria = case_when(
    Criteria == "Exclude posts with with index date > 2023-12-31 (SwedeHF)/2021-12-31 (NPR HF, Controls)" ~ "Exclude posts with index date > 2023-12-31",
    Criteria == "Exclude posts censored end fu < index" ~ "Exclude posts with end of follow-up < index",
    TRUE ~ Criteria
  ))

flow <- rbind(c("General inclusion/exclusion criteria", ""), flow)

flow <- rbind(flow, c("Project specific inclusion/exclusion criteria", ""))

rsdata <- rsdata421 %>%
  filter(shf_indexdtm >= ymd("2008-02-01"))
flow <- rbind(flow, c("Include posts >= 2008-02-01 (Anxiety/depression included in the CRF)", nrow(rsdata)))

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

rsdataexclude <- anti_join(rsdata421, rsdata, by = "lopnr")
rsdataexclude <- rsdataexclude %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

rm(rsdata421)
gc()
