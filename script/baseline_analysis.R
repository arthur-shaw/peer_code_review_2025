##############
#Author: Xiaoming Zhang
#Date: 3.12.2025
#Purpose: Baseline Analysis code for reproducibility 
#############


library(googlesheets4)
getwd()

# ==============================================================================
# Set project paths
# ==============================================================================

# Import Data ----
# rather than have users point to their own local folder and change the path
# consider using `{pins}`
# while this is something I've wanted to do at WBG for a LONG time, I've not
# tested it yet, among other reasons because of our restrictive IT environment
# nevertheless, it looks like Dropbox might be supported somehow,
# unless this is just a pin that exists in a shared folder that has a different
# path on each user's device
# see more here: https://pins.rstudio.com/index.html

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

# ==============================================================================
# Data preparation
# ==============================================================================

hfc_constr <- readxl::read_xlsx(file.path(data_path, "hfc_constr.xlsx"))

#Regression analysis

hfc_regress <- hfc_constr |> 
  dplyr::mutate(
    asset_index = C3_1 + C3_2 + C3_3
    + C3_4 + C3_5 + C3_6 + C3_7 + C3_8
      + C3_9 + C3_10 +C3_11 + C3_12 + C3_13,
    wtp_12 = J4_2*12,
    wtp_24 = J5_2*24,
    dplyr::across(
      .cols = c(
        "J1_final", # wtp_fixed
                  "J2_1",     # wtp_fixed_appliance
                  "J3_1"     # wtp_fixed_low_reliability
      ),
      .fns = ~ pmax(.x, 1000)
    ),
    dplyr::across(
      .cols = c(
        "J1_final", # wtp_fixed
                  "J2_1",     # wtp_fixed_appliance
                  "J3_1"     # wtp_fixed_low_reliability
      ),
      .fns = ~pmin(.x, 100000)
    )
  ) |> 
  dplyr::rename(
    fixed_system = J1_final,
    appliance = J2_1,
    low_reliability = J3_1,
    lightbulb = J6_1
  ) |>  
  dplyr::mutate(
    lightbulb = pmax(lightbulb, 100),
    `log(appliance) - log(fixed_system)` = log(appliance) - log(fixed_system),
    `log(fixed_system) - log(low_reliability)` = log(fixed_system) - log(low_reliability)
  ) |> 
  dplyr::rename(
    household_size = A1_1
    primary_week = A2_7,
    primary_day = A2_8,
    primary_hour = A2_9,
    secondary_week = A3_8,
    secondary_day = A3_9,
    secondary_hour = A3_10
  ) |>
  dplyr::mutate(
    A2_4_week = dplyr::case_when(
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
  ) |> 
  dplyr::mutate(
    A2_4_week = dplyr::if_else(
      condition = A2_4_week > quantile(A2_4_week, 0.99, na.rm = TRUE),
      true = quantile(A2_4_week, 0.99, na.rm = TRUE),
      false = A2_4_week
    ) 
  ) |> 
  dplyr::rename(weekly_income = A2_4_week) |> 
  dplyr::mutate(
    weekly_income = dplyr::if_else(
      condition = is.na(weekly_income), 
      true = 0,
      false = weekly_income
    ),
    log_head_weekly_income = log(weekly_income / 1000 + 1),  
    `log(wtp_12)-log(fixed_system)` = log(wtp_12) - log(fixed_system)
)

# ==============================================================================
# Create map plots
# ==============================================================================

#Distance to LV----


#Distance to the household and the surveyed LV lines
rwa_villages <- sf::st_read(dsn = file.path(data_path, "rwa_villages", "Village.shp"))

rwa_villages <- sf::st_make_valid(rwa_villages)

hfc_sf <- sf::st_as_sf(
  x = hfc_regress,
  coords = c("coordinate.Longitude", "coordinate.Latitude"),
  crs = 4326
)

# consider making a function
# you seem to be doing the same thing for Karongi, Rulindo, and Rutsiro
# this could also help if, say, you want the change the plot theme in
# one place rather than several
# this is clearly refactoring that might not have been desirable early on
# in the project

#' Create the area plot for a given area
#'
#' @param hfc_sf
#' @param villages_df
#' @param data_dir Character. Path to the Dropbox data directory.
#' @param sub_dir Character. Name of the area directory where shapefiles are
#' located.
#' @param shp_name Character. Name of the shapefile for the area.
#' @param district_key Character. Name of the district.
#' @param output_path Character. Where plot to be saved.
#'
#' @return lv
#' Also, side-effect of creating area plot.
#'
#' @importFrom sf st_read st_transform
#' @importFrom dplyr filer
#' @importFrom ggplot2 ggplot geom_sf labs theme_minimal ggsave
#' @importFrom glue glue
create_area_plot <- function(
  hfc_sf,
  villages_df,
  data_dir,
  sub_dir,
  shp_name,
  district_key,
  output_path
) {

  lv <- sf::st_read(dsn = file.path(data_path, sub_dir, shp_name))

  hfc <- hfc_sf |> 
    dplyr::filter(district_key == district_key)

  villages <- villages_df |> 
    dplyr::filter(District == district_key)

  lv <- sf::st_transform(lv, crs = 4326)
  villages <- sf::st_transform(villages, crs = 4326)

  plot <- ggplot2::ggplot(data = villages) +
    ggplot2::geom_sf(fill = NA, color = "lightgrey") +  
    ggplot2::geom_sf(data = lv, color = "blue", size = 0.5) + 
    ggplot2::geom_sf(data = hfc, color = "red", size = 0.1) +
    ggplot2::labs(title = glue::glue("Electrification Network in {district_key}")) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(
    filename = file.path(output_path, glue::glue("{district_key}")),
    plot = plot,
    width = 12,      
    height = 9,      
    units = "in",
    scale = 0.5
  )

  return(lv)

}

# its use might look like this

# Karongi
karongi_lv <- create_area_plot(
  hfc_sf = hfc_sf,
  villages_df = rwa_villages,
  data_dir = data_path,
  sub_dir = "Karongi Surveyed 0116",
  shp_name = "Surveyed_LV_Lines.shp"
  district_key = "Karongi",
  output_path = output_path
)

# Rulido
rulindo_lv <- create_area_plot(
  hfc_sf = hfc_sf,
  villages_df = rwa_villages,
  data_dir = data_path,
  sub_dir = "Rulindo Surveyed 0116",
  shp_name = "Surveyed_LV_Lines.shp"
  district_key = "Rulindo",
  output_path = output_path
)

# Rutsiro
rutsiro_lv <- create_area_plot(
  hfc_sf = hfc_sf,
  villages_df = rwa_villages,
  data_dir = data_path,
  sub_dir = "Rutsiro Surveyed 0116",
  shp_name = "Surveyed_LV_Lines.shp"
  district_key = "Rutsiro",
  output_path = output_path
)

#Rusizi----

# this one is different for some reason--why?

hfc_rusizi <- hfc_sf |> 
  dplyr::filter(district_key == "Rusizi")


rusizi_villages <- rwa_villages |> 
  dplyr::filter(District == "Rusizi")


rusizi_villages <- sf::st_transform(rusizi_villages, crs = 4326)

rusizi_villages <- sf::st_zm(rusizi_villages)

hfc_rusizi <- sf::st_intersection(hfc_rusizi, rusizi_villages)

rusizi_plot <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = rusizi_villages, fill = NA, color = "black", size = 0.3) +  
  # geom_sf(data = rusizi_lv, color = "blue", size = 0.5) +  
  ggplot2::geom_sf(data = hfc_rusizi, color = "red", size = 0.5) +  
  ggplot2::labs(title = "Surveyed Households in Rusizi") +
  ggplot2::theme_void()


ggplot2::ggsave(
  filename = file.path(output_path, "rusizi.jpeg"),
  plot = rusizi_plot,
  width = 12,      
  height = 9,      
  units = "in",
  scale = 0.5
)




#LV line----
karongi_lv <- dplyr::rename(.data = karongi_lv, length = SHAPE_Leng)
rutsiro_lv <- dplyr::select(.data = rutsiro_lv, length, geometry)

lv_line <- rbind(karongi_lv, rulindo_lv, rutsiro_lv)

lv_line <- sf::st_zm(lv_line)

dist_matrix <- sf::st_distance(hfc_sf, lv_line)

dist_matrix <- as.data.frame(dist_matrix)


dist_matrix$min_meter <- apply(dist_matrix, 1, min, na.rm = TRUE)

dist <- dist_matrix |> 
  dplyr::select(min_meter) |> 
  dplyr::mutate(min_meter = round(min_meter,2))


hfc_sf_regress <- hfc_sf |>
  dplyr::mutate(distance_to_lv = dist) |> 
  sf::st_drop_geometry() |> 
  dplyr::mutate(
    distance_to_lv = as.numeric(unlist(distance_to_lv)),
    distance_to_lv = distance_to_lv / 1000  # Convert meters to kilometers
  )

# ==============================================================================
# Regression analysis
# ==============================================================================

#felm with distance_lv-----

# consider creating a function here
# the only tricky part is that you'll need "quote" the formula and then evluate it
# see `20.6.2.1 Wrapping modelling functions` here: https://adv-r.hadley.nz/evaluation.html

# might look something like this

run_felm_reg <- function(
  data,
  formula,
  env = rlang::caller_env()
) {

  # quote inputs
  formula <- rlang::enexpr(formula)
  data <- enexpr(data)

  # run regression
  reg <- lfe::felm(
    formula = !!formula,
    data = !!data
  )

  # print regression output
  summary(reg)

  return(reg)

}

# at worst, this would make your repetitive work more compact
# at best, you could use `purrr` to apply to apply the function to a list of
# forumulas and get a list of results

#Regression outputs----


reg_low_reliability <- lfe::felm(
  formula = log(low_reliability) ~ log_head_weekly_income + asset_index + household_size|village,
  data = hfc_regress
)
summary(reg_low_reliability)

reg_fixed <- lfe::felm(
  formula = log(fixed_system) ~ log_head_weekly_income + asset_index + household_size|village,
  data = hfc_regress
)
summary(reg_fixed)

reg_appliance_fix <- lfe::felm(
  formula = `log(appliance) - log(fixed_system)`~ log_head_weekly_income + asset_index + household_size|village,
  data = hfc_regress
)
summary(reg_appliance_fix)

reg_fix_low_reliability <- lfe::felm(
  formula = `log(fixed_system) - log(low_reliability)` ~ log_head_weekly_income + asset_index + household_size|village,
  data = hfc_regress
)
summary(reg_fix_low_reliability)

reg_12 <- lfe::felm(
  formula = `log(wtp_12)-log(fixed_system)` ~ log_head_weekly_income + asset_index + household_size|village,
  data = hfc_regress
)
summary(reg_12)

reg_lightbulb <- lfe::felm(
  formula = log(lightbulb) ~ log_head_weekly_income + asset_index + household_size|village,
  data = hfc_regress
)
summary(reg_lightbulb)

regs <- list(
  "low_reliability" = reg_low_reliability,
  "fixed_system" = reg_fixed,
  "appliance_fix" = reg_appliance_fix,
  "fix_reliability" = reg_fix_low_reliability,
  "reg_12" = reg_12,
  "lightbulb" = reg_lightbulb
)

stargazer::stargazer( 
  regs,
  type = "latex", 
  title = "Regression Results", 
  out = file.path(output_path, "regression_output1.latex")
)

stargazer::stargazer(
  regs,
  type = "html",
  title = "Regression Results",
  out = file.path(output_path, "regression_output1.html")
)

#felm with distance_lv-----


# Regression 1: Low Reliability
reg_low_reliability <- lfe::felm(
  log(low_reliability) ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_low_reliability)

# Regression 1: Low Reliability
reg_fixed_system <- lfe::felm(
  log(fixed_system) ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_fixed_system)

# Regression 2: Appliance Fix (including village FE)
reg_appliance_fix <- lfe::felm(
  `log(appliance) - log(fixed_system)` ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_appliance_fix)

# Regression 3: Fixed System vs Low Reliability (including village FE)
reg_fix_low_reliability <- lfe::felm(
  `log(fixed_system) - log(low_reliability)` ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_fix_low_reliability)

# Regression 4: WTP 12 vs Fixed System (including village FE)
reg_12 <- lfe::felm(
  `log(wtp_12)-log(fixed_system)` ~ log_head_weekly_income + asset_index + household_size + distance_to_lv | village,
  data = hfc_sf_regress
)
summary(reg_12)

# Regression 5: Lightbulb (including village FE)
reg_lightbulb <- lfe::felm(
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

stargazer::stargazer(
  regs,
  type = "latex",
  title = "Regression Results",
  out = file.path(output_path, "regression_output2.latex")
)

stargazer::stargazer(
  regs,
  type = "html",
  title = "Regression Results",
  out = file.path(output_path, "regression_output2.html")
)


#LV with other control----

distance_hhsize <- lfe::felm(
  distance_to_lv ~  household_size  | village,
  data = hfc_sf_regress
)

summary(distance_hhsize)

distance_asset <- lfe::felm(
  distance_to_lv ~ asset_index  | village,
  data = hfc_sf_regress
)

summary(distance_asset)

distance_income <- lfe::felm(
  distance_to_lv ~ log_head_weekly_income  | village,
  data = hfc_sf_regress
)

summary(distance_income)



distance_all <- lfe::felm(
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


stargazer::stargazer(
  regs,
  type = "latex",
  title = "Regression Results",
  out = file.path(output_path, "regression_output3.latex")
)


stargazer::stargazer(
  regs,
  type = "html",
  title = "Regression Results",
  out = file.path(output_path, "regression_output3.html")
)

