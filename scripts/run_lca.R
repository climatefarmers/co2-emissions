#main in function form

#### TO DO: fix n2o_n_fixing & leakage functions so that it is calc and displayed
#### TO DO: include pastures and compst ?? to n2o_n_fixing
#### TO DO LATER: include n2o_n_fixing & compost import to leakage

run_lca <- function(inputs_loc,
                    co2_emissions_loc,
                    results_loc, 
                    data_loc,
                    scenario_selected
){
  
  source(file.path(co2_emissions_loc, "scripts", "calc_functions.R"))
  source(file.path(co2_emissions_loc, "scripts", "results_functions.R"))
  source(file.path(co2_emissions_loc, "scripts", "agroforestry_functions.R"))
  source(file.path(co2_emissions_loc, "scripts", "leakage_functions.R"))
  source(file.path(co2_emissions_loc, "scripts", "test_functions.R"))
  
  input_files <- list.files(inputs_loc)
  required_inputs <- c("additional_manure_inputs.csv","agroforestry_inputs.csv","animal_inputs.csv","animal_summary.csv","bare_field_inputs.csv","barrier_analysis.txt","crop_inputs.csv","farm_details.json","fertilizer_inputs.csv","field_inputs.csv","fuel_inputs.csv","pasture_inputs.csv", "parcel_inputs.csv")
  if( length(setdiff(input_files, required_inputs)) > 0){stop(paste0("Missing inputs files. The missing files include: ", setdiff(input_files, required_inputs)))}
  
  animal_data <- read_csv(file.path(inputs_loc, "animal_summary.csv")) %>% filter(!is.na(species))
  crop_data <- read_csv(file.path(inputs_loc, "crop_inputs.csv"))  
  tree_data <- read_csv(file.path(inputs_loc, "agroforestry_inputs.csv"))  
  field_data <- read_csv(file.path(inputs_loc, "field_inputs.csv"))
  fertilizer_data <- read_csv(file.path(inputs_loc, "fertilizer_inputs.csv"))
  fuel_data <- read_csv(file.path(inputs_loc, "fuel_inputs.csv"))
  add_manure_data <- read_csv(file.path(inputs_loc, "additional_manure_inputs.csv"))
  parcel_data <- read_csv(file.path(inputs_loc, "parcel_inputs.csv"))
  pasture_data <- read_csv(file.path(inputs_loc, "pasture_inputs.csv"))
  
  climate_zone <- unique(field_data$climate_zone)
  if(length(climate_zone) > 1){stop("Climate Zones not unique")}
  
  climate_wet_or_dry <- unique(field_data$climate_wet_or_dry)
  if(length(climate_zone) > 1){stop("Climate Zones not unique")}
  
  field_area <- unique(field_data$hectares)
  if(length(field_area) == 0){warning("No field area data available")}
  # TODO: Set this up to run for more than one field if necessary. Include Field IDs
  
  # Read in lca data
  animal_factors <- read_csv(file.path(data_loc,"data", "animal_emission_factors.csv"))
  crop_factors <- read_csv(file.path(data_loc,"data", "crop_factors.csv"))
  methane_factors <- read_csv(file.path(data_loc,"data", "methane_emission_factors.csv")) %>% filter(climate == climate_zone) %>% select(-climate)
  fertilizer_factors <- read_csv(file.path(data_loc,"data", "fertilizer_factors.csv"))
  fuel_factors <- read_csv(file.path(data_loc, "data", "fuel_factors.csv"))
  co2eq_factors <- read_csv(file.path(data_loc, "data", "co2eq_factors.csv"))
  tree_factors <- read_csv(file.path(data_loc, "data", "agroforestry_factors.csv"))
  manure_factors <- read_csv(file.path(data_loc, "data", "carbon_share_manure.csv"))
  pasture_factors <- read_csv(file.path(data_loc, "data", "pasture_factors.csv"))
  
  # Check input data for validity
  check_animal_data(animal_data, animal_factors)
  check_crop_data(crop_data, crop_factors)
  check_fertilizer_data(fertilizer_data, fertilizer_factors)
  check_fuel_data(fuel_data, fuel_factors)
  check_manure_data(add_manure_data, manure_factors)  
  
  # merge in factors into lca data
  animal_data <- left_join(filter(animal_data, scenario==scenario_selected), animal_factors, by = "species")
  animal_data <- left_join(animal_data, methane_factors, by = c("species" = "species", "grazing_management" = "grazing_management", "productivity" = "productivity"))
  n_fixing_species_crop <- left_join(filter(crop_data, scenario==scenario_selected), crop_factors, by = "crop")
  n_fixing_species_crop <- left_join(n_fixing_species_crop, parcel_data, by = "parcel_ID")
  n_fixing_species_pasture <- left_join(filter(pasture_data, scenario==scenario_selected), pasture_factors, by = "grass")
  n_fixing_species_pasture <- left_join(n_fixing_species_pasture, parcel_data, by = "parcel_ID")
  fertilizer_data <- left_join(filter(fertilizer_data, scenario==scenario_selected), fertilizer_factors, by = "fertilizer_type")
  fuel_data <- left_join(filter(fuel_data, scenario==scenario_selected), fuel_factors, by = "fuel_type")
  add_manure_data <- left_join(filter(add_manure_data, scenario==scenario_selected), manure_factors, by = "manure_source")
  add_manure_data <- left_join(filter(add_manure_data, scenario==scenario_selected), parcel_data, by = "parcel_ID")
  # Run through calculations
  
  fertilizer_data <- n2o_fertilizer(fertilizer_data,
                                    ef_fertilizer = 0.01) 
  
  animal_data <- ch4_enteric_fermentation(animal_data)
  
  animal_data <- n2o_manure_deposition_direct(animal_data, climate_wet_or_dry=climate_wet_or_dry)
  animal_data <- n2o_manure_deposition_indirect(animal_data, climate_wet_or_dry=climate_wet_or_dry)
  
  animal_data <- ch4_manure_deposition(animal_data)
  
  n_fixing_species_crop <- n2o_n_fixing_species_crop(n_fixing_species_crop, 
                                                field_area = field_area)
  
  n_fixing_species_pasture <- n2o_n_fixing_species_pasture(n_fixing_species_pasture, 
                                                field_area = field_area)
  
  fuel_data <- co2_fuel_consumption(fuel_data)
  
  
  # add leakage calculations
  
  leakage <- manure_leakage(add_manure_data)
  
  # Clean Results 
  
  if (nrow(fertilizer_data) > 0){fertilizer_results <- fertilizer_data %>% select(fertilizer_type, n2o_fertilizer)}else{
    fertilizer_results <- create_empty_dataframe(c("fertilizer_type", "n2o_fertilizer"))
  }
  if (nrow(animal_data) > 0){  animal_results <- animal_data %>% select(species, ch4_manure_dep, ch4_ent_ferm, n2o_urine_dung_indirect, n2o_urine_dung_direct)}else{
    animal_results <- create_empty_dataframe(c("species", "ch4_manure_dep", "ch4_ent_ferm", "n2o_urine_dung_indirect", "n2o_urine_dung_direct"))
  }
  if (nrow(fuel_data) > 0){  fuel_results <- fuel_data %>% select(fuel_type, co2_fuel)}else{
    fuel_results <- create_empty_dataframe(c("fuel_type", "co2_fuel"))
  }
  if (nrow(n_fixing_species_crop) > 0){crop_results <- n_fixing_species_crop %>% select(crop, n2o_n_fixing)}else{
    crop_results <- create_empty_dataframe(c("crop", "n2o_n_fixing"))
  }
  if (nrow(n_fixing_species_pasture) > 0){pasture_results <- n_fixing_species_pasture %>% select(grass, n2o_n_fixing)}else{
    pasture_results <- create_empty_dataframe(c("grass", "n2o_n_fixing"))
  }
  
  
  
  fertilizer_results_sum <- fertilizer_results %>% summarise(n2o_fertilizer = sum(n2o_fertilizer))
  animal_results_sum <- animal_results %>% summarise(ch4_manure_dep = sum(ch4_manure_dep),
                                                     ch4_ent_ferm = sum(ch4_ent_ferm),
                                                     n2o_manure_deposition = sum(n2o_urine_dung_indirect)+sum(n2o_urine_dung_direct))
  fuel_results_sum <- fuel_results %>% summarise(co2_fuel = sum(co2_fuel))
  
  crop_results_sum <- crop_results %>% summarise(n2o_n_fixing = sum(n2o_n_fixing))
  pasture_results_sum <- pasture_results %>% summarise(n2o_n_fixing = sum(n2o_n_fixing))
  n_fixing_sum <- crop_results_sum + pasture_results_sum
  
  leakage_sum <- leakage %>% summarise(co2_leakage = sum(co2_leakage))
  
  # Summarise Results
  
  all_results <- bind_rows(fertilizer_results_sum, animal_results_sum, fuel_results_sum, n_fixing_sum, leakage_sum) %>% 
    pivot_longer(cols = everything(), names_to = "source") %>% 
    filter(!is.na(value)) %>% 
    mutate(gas = substr(source,1,3),
           source = substr(source, 5, nchar(source))) %>% 
    left_join(co2eq_factors, by = "gas") %>% 
    mutate(co2eq = co2eq_factor * value)
  
  return(all_results)
  
}


create_empty_dataframe <- function(column_names = c()){
  
  df = matrix(ncol = length(column_names), nrow = 0)
  colnames(df)  <- column_names
  df <- as.data.frame(df)
  
  return(df)
  
}

