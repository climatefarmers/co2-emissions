#main in function form


run_lca <- function(inputs_loc,
                    lca_loc,
                    results_loc, 
                    data_loc
){
  
  source(file.path(lca_loc, "scripts", "calc_functions.R"))
  source(file.path(lca_loc, "scripts", "results_functions.R"))
  source(file.path(lca_loc, "scripts", "agroforestry_functions.R"))
  source(file.path(lca_loc, "scripts", "leakage_functions.R"))
  source(file.path(lca_loc, "scripts", "test_functions.R"))
  
  input_files <- list.files(inputs_loc)
  required_inputs <- c("additional_manure_inputs.csv","agroforestry_inputs.csv","animal_inputs.csv","bare_field_inputs.csv","barrier_analysis.txt","crop_inputs.csv","farm_details.json","fertilizer_inputs.csv","field_inputs.csv","fuel_inputs.csv")
  if( length(setdiff(input_files, required_inputs)) > 0){stop(paste0("Missing inputs files. The missing files include: ", setdiff(input_files, required_inputs)))}
  
  animal_data <- read_csv(file.path(inputs_loc, "animal_inputs.csv")) %>% filter(!is.na(species))
  crop_data <- read_csv(file.path(inputs_loc, "crop_inputs.csv"))  
  tree_data <- read_csv(file.path(inputs_loc, "agroforestry_inputs.csv"))  
  field_data <- read_csv(file.path(inputs_loc, "field_inputs.csv"))
  fertilizer_data <- read_csv(file.path(inputs_loc, "fertilizer_inputs.csv"))
  fuel_data <- read_csv(file.path(inputs_loc, "fuel_inputs.csv"))
  add_manure_data <- read_csv(file.path(inputs_loc, "additional_manure_inputs.csv"))
  
  
  climate_zone <- unique(field_data$climate_zone)
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
  
  # Check input data for validity
  check_animal_data(animal_data, animal_factors)
  check_crop_data(crop_data, crop_factors)
  check_fertilizer_data(fertilizer_data, fertilizer_factors)
  check_fuel_data(fuel_data, fuel_factors)
  check_manure_data(add_manure_data, manure_factors)  
  
  # merge in factors into lca data
  animal_data <- left_join(animal_data, animal_factors, by = "species")
  animal_data <- left_join(animal_data, methane_factors, by = c("species" = "species", "grazing_management" = "grazing_management", "productivity" = "productivity"))
  n_fixing_species <- left_join(crop_data, crop_factors, by = "crop")
  fertilizer_data <- left_join(fertilizer_data, fertilizer_factors, by = "fertilizer_type")
  fuel_data <- left_join(fuel_data, fuel_factors, by = "fuel_type")
  add_manure_data <- left_join(add_manure_data, manure_factors, by = "manure_source")
  
  # Run through calculations
  
  fertilizer_data <- n2o_fertilizer(fertilizer_data,
                                    ef_fertilizer = 0.01, 
                                    field_area = field_area) 
  
  animal_data <- ch4_enteric_fermentation(animal_data)
  
  animal_data <- n2o_urine_dung(animal_data,
                                frac_gasm = 0.21,
                                ef_4 = 0.01)
  
  animal_data <- ch4_manure_deposition(animal_data)
  
  n_fixing_species <- n2o_n_fixing_species(n_fixing_species, 
                                           field_area = field_area)
  
  fuel_data <- co2_fuel_consumption(fuel_data)
  
  
  # add leakage calculations
  
  leakage <- manure_leakage(add_manure_data, field_area = field_area)
  
  # Clean Results 
  
  if (nrow(fertilizer_data) > 0){fertilizer_results <- fertilizer_data %>% select(fertilizer_type, n2o_fertilizer)}else{
    fertilizer_results <- create_empty_dataframe(c("fertilizer_type", "n2o_fertilizer"))
  }
  if (nrow(animal_data) > 0){  animal_results <- animal_data %>% select(species, ch4_manure_dep, ch4_ent_ferm, n2o_urine_dung)}else{
    animal_results <- create_empty_dataframe(c("species", "ch4_manure_dep", "ch4_ent_ferm", "n2o_urine_dung"))
  }
  if (nrow(fuel_data) > 0){  fuel_results <- fuel_data %>% select(fuel_type, co2_fuel)}else{
    fuel_results <- create_empty_dataframe(c("fuel_type", "co2_fuel"))
  }
  if (nrow(n_fixing_species) > 0){crop_results <- n_fixing_species %>% select(crop, n2o_n_fixing)}else{
    crop_results <- create_empty_dataframe(c("crop", "n2o_n_fixing"))
  }
  
  
  
  fertilizer_results_sum <- fertilizer_results %>% summarise(n2o_fertilizer = sum(n2o_fertilizer))
  animal_results_sum <- animal_results %>% summarise(ch4_manure_dep = sum(ch4_manure_dep),
                                                     ch4_ent_ferm = sum(ch4_ent_ferm),
                                                     n2o_urine_dung = sum(n2o_urine_dung))
  fuel_results_sum <- fuel_results %>% summarise(co2_fuel = sum(co2_fuel))
  crop_results_sum <- crop_results %>% summarise(n2o_n_fixing = sum(n2o_n_fixing))
  
  leakage_sum <- leakage %>% summarise(co2_leakage = sum(co2_leakage))
  
  # Summarise Results
  
  all_results <- bind_rows(fertilizer_results_sum, animal_results_sum, fuel_results_sum, crop_results_sum, leakage_sum) %>% 
    pivot_longer(cols = everything(), names_to = "source") %>% 
    filter(!is.na(value)) %>% 
    mutate(gas = substr(source,1,3),
           source = substr(source, 5, nchar(source))) %>% 
    left_join(co2eq_factors, by = "gas") %>% 
    mutate(co2eq = co2eq_factor * value)
  
  gas_summary <- all_results %>% 
    group_by(gas) %>% 
    summarise(co2eq = sum(co2eq), .groups = "drop")
  
  
  plot_source_breakdown(all_results %>%  filter(source != "leakage"), results_loc)
  write_csv(gas_summary, file.path(results_loc, "lca_results.csv"))
  write_csv(all_results, file.path(results_loc, "lca_results_source.csv"))
  
}


create_empty_dataframe <- function(column_names = c()){
  
  df = matrix(ncol = length(column_names), nrow = 0)
  colnames(df)  <- column_names
  df <- as.data.frame(df)
  
  return(df)
  
}

