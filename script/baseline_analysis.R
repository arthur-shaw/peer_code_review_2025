##############
#Author: Xiaoming Zhang
#Date: 3.12.2025
#Purpose: Baseline Analysis code for reproducibility 
#############


pacman::p_load(knitr, flm, stargazer, tidyverse, dplyr, here, sf, ggplot2, readxl, writexl, janitor, randomizr, RCT, purrr, lfe)
library(googlesheets4)
getwd()


# Import Data ----
dropbox <- 'C:/Users/~/Dropbox' #Replace ~ with your own path to Dropbox

hfc_data_path <- file.path(
  dropbox,
  "peer_code_review_2025/data"
)

output_path <- file.path(
  dropbox,
  "peer_code_review_2025/output"
)

data_path <- file.path(
  dropbox,
  "peer_code_review_2025/data"
)

hfc_constr <- read_xlsx(file.path(data_path, "hfc_constr.xlsx"))

#Regression analysis

hfc_regress <- hfc_constr %>% 
  mutate(
    asset_index = C3_1 + C3_2 + C3_3
    + C3_4 + C3_5 + C3_6 + C3_7 + C3_8
    + C3_9 + C3_10 +C3_11 + C3_12 + C3_13
  ) %>% 
  mutate(
    wtp_12 = J4_2*12,
    wtp_24 = J5_2*24
  ) %>% 
  mutate(across(c("J1_final", # wtp_fixed
                  "J2_1",     # wtp_fixed_appliance
                  "J3_1"     # wtp_fixed_low_reliability
  ), ~ pmax(.x, 1000)))  %>% 
  mutate(across(c("J1_final", # wtp_fixed
                  "J2_1",     # wtp_fixed_appliance
                  "J3_1"     # wtp_fixed_low_reliability
  ), ~pmin(.x, 100000))) %>% 
  
  rename(
    fixed_system = J1_final,
    appliance = J2_1,
    low_reliability = J3_1,
    lightbulb = J6_1
  ) %>%  
  mutate(
    lightbulb = pmax(lightbulb, 100)
  ) %>% 
  mutate(
    `log(appliance) - log(fixed_system)` = log(appliance) - log(fixed_system),
    `log(fixed_system) - log(low_reliability)` = log(fixed_system) - log(low_reliability)
  )


#Adding other controls----

hfc_regress <- hfc_regress %>% 
  rename(household_size = A1_1) %>% 
  rename(
    primary_week = A2_7,
    primary_day = A2_8,
    primary_hour = A2_9,
    secondary_week = A3_8,
    secondary_day = A3_9,
    secondary_hour = A3_10
  )

hfc_regress <- hfc_regress %>%
  mutate(
    A2_4_week = case_when(
      A2_5_label == "Hour" ~ A2_4 * primary_hour * primary_day,
      A2_5_label == "Day"  ~ A2_4 * primary_week,
      A2_5_label == "Week" ~ A2_4 ,
      A2_5_label == "2 Weeks" ~ round(A2_4/2,2),
      A2_5_label == "Month" ~ round(A2_4/4,2),
      A2_5_label == "Quarter" ~ round(A2_4/(3*4),2),
      A2_5_label == "Agricultural season" ~ round(A2_4/(4*4),2),
      A2_5_label == "Half Year" ~ round(A2_4/(6*4),2),
      A2_5_label == "Year" ~ round(A2_4/(12*4),2),
      TRUE ~ A2_4
    )
  ) %>% 
  mutate(
    A2_4_week = ifelse(
      A2_4_week > quantile(A2_4_week, 0.99, na.rm = TRUE),
      quantile(A2_4_week, 0.99, na.rm = TRUE), A2_4_week
    ) 
  ) %>% 
  rename(
    weekly_income = A2_4_week
  ) 


hfc_regress<- hfc_regress%>% 
  mutate(
    weekly_income = ifelse(is.na(weekly_income), 0, weekly_income)
  )

hfc_regress <- hfc_regress %>%
  mutate(
    log_head_weekly_income = log(weekly_income / 1000 + 1),  
    `log(wtp_12)-log(fixed_system)` = log(wtp_12) - log(fixed_system) ,
  )


#Regression outputs----


reg_low_reliability <- felm(log(low_reliability) ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_low_reliability)

reg_fixed <- felm(log(fixed_system) ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_fixed)

reg_appliance_fix <- felm(`log(appliance) - log(fixed_system)`~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_appliance_fix)

reg_fix_low_reliability <- felm(`log(fixed_system) - log(low_reliability)` ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_fix_low_reliability)

reg_12 <- felm(`log(wtp_12)-log(fixed_system)` ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_12)

reg_lightbulb <- felm(log(lightbulb) ~ log_head_weekly_income + asset_index + household_size|village, data = hfc_regress)
summary(reg_lightbulb)

regs <- list(
  "low_reliability" = reg_low_reliability,
  "fixed_system" = reg_fixed,
  "appliance_fix" = reg_appliance_fix,
  "fix_reliability" = reg_fix_low_reliability,
  "reg_12" = reg_12,
  "lightbulb" = reg_lightbulb
)

stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = file.path(output_path, "regression_output1.latex")
)

stargazer(
  regs,
  type = "html",
  title = "Regression Results",
  out = file.path(output_path, "regression_output1.html")
)

#Distance to LV----


#Distance to the household and the surveyed LV lines
rwa_villages <- st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))

rwa_villages <- st_make_valid(rwa_villages)

hfc_sf <- st_as_sf(hfc_regress, coords = c("coordinate.Longitude", "coordinate.Latitude"), crs = 4326)

##Karongi----
karongi_lv <- st_read(dsn = file.path(data_path, "Karongi Surveyed 0116", "Surveyed_LV_Lines.shp"))

hfc_karongi <- hfc_sf %>% 
  filter(district_key == "Karongi")

karongi_villages <- rwa_villages %>% 
  filter(District == "Karongi")

karongi_lv <- st_transform(karongi_lv, crs = 4326)
karongi_villages <- st_transform(karongi_villages, crs = 4326)

karongi_plot <- ggplot(data = karongi_villages) +
  geom_sf(fill = NA, color = "lightgrey") +  
  geom_sf(data = karongi_lv, color = "blue", size = 0.5) + 
  geom_sf(data = hfc_karongi, color = "red", size = 0.1) +
  labs(title = "Electrification Network in Karongi") +
  theme_minimal()

ggsave(
  filename = file.path(output_path, "karongi.jpeg"),
  plot = karongi_plot,
  width = 12,      
  height = 9,      
  units = "in",
  scale = 0.5
)

##Rulindo----

rulindo_lv <- st_read(dsn = file.path(data_path, "Rulindo Surveyed 0116", "Surveyed_LV_Lines.shp"))

hfc_rulindo <- hfc_sf %>% 
  filter(district_key == "Rulindo")

rulindo_villages <- rwa_villages %>% 
  filter(District == "Rulindo")

rulindo_lv <- st_transform(rulindo_lv, crs = 4326)

rulindo_villages <- st_transform(rulindo_villages, crs = 4326)

hfc_rulindo <- st_transform(hfc_rulindo, crs = 4326)

hfc_rulindo <- st_intersection(rulindo_villages, hfc_rulindo)

rulindo_plot <- ggplot(data = rulindo_villages) +
  geom_sf(fill = NA, color = "lightgrey") +  
  geom_sf(data = rulindo_lv, color = "blue", size = 0.5) + 
  geom_sf(data = hfc_rulindo, color = "red", size = 0.1) +
  labs(title = "Electrification Network in Rulindo") +
  theme_minimal()

ggsave(
  filename = file.path(output_path, "rulindo.jpeg"),
  plot = rulindo_plot,
  width = 12,      
  height = 9,      
  units = "in",
  scale = 0.5
)


##Rutsiro----
rutsiro_lv <- st_read(dsn = file.path(data_path, "Rutsiro Surveyed 0116", "Surveyed_LV_Lines.shp"))

hfc_rutsiro <- hfc_sf %>% 
  filter(district_key == "Rutsiro")

rutsiro_villages <- rwa_villages %>% 
  filter(District == "Rutsiro")

rutsiro_lv <- st_transform(rutsiro_lv, crs = 4326)
rutsiro_villages <- st_transform(rutsiro_villages, crs = 4326)

rutsiro_villages <- st_zm(rutsiro_villages)

rutsiro_plot <- ggplot() +
  geom_sf(data = rutsiro_villages, fill = NA, color = "lightgrey", size = 0.3) +  
  geom_sf(data = rutsiro_lv, color = "blue", size = 0.5) +  
  geom_sf(data = hfc_rutsiro, color = "red", size = 0.1) +  
  labs(title = "Electrification Network in Rutsiro") +
  theme_minimal()

ggsave(
  filename = file.path(output_path, "rutsiro.jpeg"),
  plot = rutsiro_plot,
  width = 12,      
  height = 9,      
  units = "in",
  scale = 0.5
)

#Rusizi----


hfc_rusizi <- hfc_sf %>% 
  filter(district_key == "Rusizi")


rusizi_villages <- rwa_villages %>% 
  filter(District == "Rusizi")


rusizi_villages <- st_transform(rusizi_villages, crs = 4326)

rusizi_villages <- st_zm(rusizi_villages)

hfc_rusizi <- st_intersection(hfc_rusizi, rusizi_villages)

rusizi_plot <- ggplot() +
  geom_sf(data = rusizi_villages, fill = NA, color = "black", size = 0.3) +  
  # geom_sf(data = rusizi_lv, color = "blue", size = 0.5) +  
  geom_sf(data = hfc_rusizi, color = "red", size = 0.5) +  
  labs(title = "Surveyed Households in Rusizi") +
  theme_void()


ggsave(
  filename = file.path(output_path, "rusizi.jpeg"),
  plot = rusizi_plot,
  width = 12,      
  height = 9,      
  units = "in",
  scale = 0.5
)




#LV line----
karongi_lv <- karongi_lv %>% rename(length = SHAPE_Leng)
rutsiro_lv <- rutsiro_lv %>% select(length, geometry)

lv_line <- rbind(karongi_lv, rulindo_lv, rutsiro_lv)

lv_line <-st_zm(lv_line)

dist_matrix <- st_distance(hfc_sf, lv_line)

dist_matrix <- as.data.frame(dist_matrix)


dist_matrix$min_meter <- apply(dist_matrix, 1, min, na.rm = TRUE)

dist <- dist_matrix %>% 
  select(min_meter) %>% 
  mutate(min_meter = round(min_meter,2))


hfc_sf_regress <- hfc_sf %>%
  mutate(distance_to_lv = dist) %>% 
  st_drop_geometry() %>% 
  mutate(
    distance_to_lv = as.numeric(unlist(distance_to_lv)),
    distance_to_lv = distance_to_lv / 1000  # Convert meters to kilometers
  )



#felm with distance_lv-----


# Regression 1: Low Reliability
reg_low_reliability <- felm(
  log(low_reliability) ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_low_reliability)

# Regression 1: Low Reliability
reg_fixed_system <- felm(
  log(fixed_system) ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_fixed_system)

# Regression 2: Appliance Fix (including village FE)
reg_appliance_fix <- felm(
  `log(appliance) - log(fixed_system)` ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_appliance_fix)

# Regression 3: Fixed System vs Low Reliability (including village FE)
reg_fix_low_reliability <- felm(
  `log(fixed_system) - log(low_reliability)` ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_fix_low_reliability)

# Regression 4: WTP 12 vs Fixed System (including village FE)
reg_12 <- felm(
  `log(wtp_12)-log(fixed_system)` ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_12)

# Regression 5: Lightbulb (including village FE)
reg_lightbulb <- felm(
  log(lightbulb) ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_lightbulb)

regs <- list(
  "low_reliability" = reg_low_reliability,
  "fixed_system" = reg_fixed_system,
  "appliance_fix" = reg_appliance_fix,
  "fix_reliability" = reg_fix_low_reliability,
  "reg_12" = reg_12,
  "lightbulb" = reg_lightbulb
)

stargazer(
  regs,
  type = "latex",
  title = "Regression Results",
  out = file.path(output_path, "regression_output2.latex")
)

stargazer(
  regs,
  type = "html",
  title = "Regression Results",
  out = file.path(output_path, "regression_output2.html")
)


#LV with other control----

distance_hhsize <- felm(
  distance_to_lv ~  household_size  | village,
  data = hfc_sf_regress
)

summary(distance_hhsize)

distance_asset <- felm(
  distance_to_lv ~ asset_index  | village,
  data = hfc_sf_regress
)

summary(distance_asset)

distance_income <- felm(
  distance_to_lv ~ log_head_weekly_income  | village,
  data = hfc_sf_regress
)

summary(distance_income)



distance_all <- felm(
  distance_to_lv ~ log_head_weekly_income + asset_index + household_size  | village,
  data = hfc_sf_regress
)

summary(distance_all)

regs <- list(
  "household size" = distance_hhsize,
  "household asset" = distance_asset,
  "household income" = distance_income,
  "combined" = distance_all
)


stargazer(
  regs,
  type = "latex",
  title = "Regression Results",
  out = file.path(output_path, "regression_output3.latex")
)


stargazer(
  regs,
  type = "html",
  title = "Regression Results",
  out = file.path(output_path, "regression_output3.html")
)

