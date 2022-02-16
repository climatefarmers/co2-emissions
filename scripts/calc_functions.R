# Initial calculations all in kg -> convert to tonnes later

co2_fuel_consumption <- function(
  fuel_data
){
  if(nrow(fuel_data) > 0){
    fuel_data <- fuel_data %>% 
      mutate(co2_fuel = value_l * ef_fuel_kg_l)
  }else{
    warning("No fuel data provided - or included in project")
  }
  return(fuel_data)
  
}

ch4_enteric_fermentation <- function(
  animal_data
){
  if(nrow(animal_data) > 0){
    animal_data <- animal_data %>% 
      mutate(ch4_ent_ferm = n_animals * (grazing_days / 365) * ef_enteric_fermentation_kg_head)
  }else{
    warning("No animal data provided - or included in project")
  }
  return(animal_data)
  
}

ch4_manure_deposition <- function(
  animal_data
){
  if(nrow(animal_data) > 0){
    animal_data <- animal_data %>% 
      mutate(ch4_manure_dep = n_animals * grazing_days/365 * ef_methane_kg_head)
  }else{
    warning("No animal data provided - or included in project")
  }
  return(animal_data)
  
}

n2o_n_fixing_species <- function(
  n_fixing_species,
  ef_n = 0.01,
  field_area = 50
){
  
  if(nrow(n_fixing_species) > 0){
    n_fixing_species <- n_fixing_species %>% 
      mutate(frac_removed = 1-residue,
             agr = yield_t_ha * dry * r_ag_t,
             bgr = (yield_t_ha * dry + agr) * rs_t *field_area * (1 / replanting_freq),
             n2o_n_fixing = ((agr*n_ag*(1-frac_removed - (frac_burnt * cf))) + (bgr * n_bg))* ef_n * (44/28) * 1000  # 1000 converts t to kg
      )  %>% select(-frac_removed, -agr, -bgr) 
  }else{
    warning("No n-fixing species data provided - or included in project")
  }
  
  return(n_fixing_species)
}

n2o_fertilizer <- function(
  fertilizer_data,
  ef_fertilizer = 0.01,
  field_area = 50
){
  
  if(nrow(fertilizer_data) > 0){
    fertilizer_data <- fertilizer_data %>% 
      mutate(n2o_fertilizer = quantity_kg_ha * field_area * n_content * (1 - volatile_fraction) * ef_fertilizer * (44/28))
  }else{
    warning("No Fertilizer data provided - or included in project")
  }
  return(fertilizer_data)
  
}

n2o_urine_dung <- function(
  animal_data,
  frac_gasm = 0.21,
  ef_4 = 0.01
){
  
  if(nrow(animal_data) > 0){
    animal_data <- animal_data %>% 
      mutate(n2o_urine_dung = n_animals * grazing_days/365 * n_excretion_rate_kg_1000am * 365 * mass/1000 * frac_gasm * ef_4)
  }else{
    warning("No Animal data provided - or included in project")
  }
  return(animal_data)
}

