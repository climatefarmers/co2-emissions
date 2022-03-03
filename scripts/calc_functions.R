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
      mutate(agr= ifelse(is.na(dry_residue)==FALSE, dry_residue*dry_c,
                                   ifelse(is.na(fresh_residue)==FALSE, fresh_residue*dry*dry_c,
                                          ifelse(is.na(dry_residue)==TRUE & is.na(fresh_residue)==TRUE & is.na(residue_frac)==TRUE,NA,
                                                 ifelse(is.na(dry_yield)==FALSE, dry_yield*dry_c*residue_frac,
                                                        ifelse(is.na(fresh_yield)==FALSE, fresh_yield*dry*dry_c*residue_frac,
                                                               ifelse(is.na(ag_dm_peak)==FALSE, ag_dm_peak*dry_c*residue_frac,
                                                                      NA)))))),
             bgr= ifelse(is.na(dry_residue)==FALSE & is.na(dry_yield)==FALSE, (dry_yield+dry_residue)*dry_c*r_s_ratio,
                                  ifelse(is.na(fresh_residue)==FALSE & is.na(fresh_yield)==FALSE, (fresh_yield+fresh_residue)*dry*dry_c*r_s_ratio,
                                         ifelse(is.na(residue_frac)==TRUE,NA,
                                                ifelse(is.na(dry_yield)==FALSE, dry_yield/(1-residue_frac)*dry_c*r_s_ratio,
                                                       ifelse(is.na(fresh_yield)==FALSE, fresh_yield/(1-residue_frac)*dry*dry_c*residue_frac*r_s_ratio,
                                                              ifelse(is.na(ag_dm_peak)==FALSE, ag_dm_peak*dry_c*r_s_ratio,
                                                                     NA)))))),
             n2o_n_fixing = (area *(agr*n_ag*(1-residue_frac - (frac_burnt * cf))) + (bgr * n_bg))* ef_n * (44/28) * 1000)  # 1000 converts t to kg)
  }else{
    warning("No n-fixing species data provided - or included in project")
  }
  
  return(n_fixing_species)
}

n2o_fertilizer <- function(
  fertilizer_data,
  ef_fertilizer = 0.011
){
  
  if(nrow(fertilizer_data) > 0){
    fertilizer_data <- fertilizer_data %>% 
      mutate(n2o_fertilizer = quantity_kg_ha * field_area * n_content * (1 - volatile_fraction) * ef_fertilizer * (44/28))
  }else{
    warning("No Fertilizer data provided - or included in project")
  }
  return(fertilizer_data)
  
}

n2o_manure_deposition_indirect <- function(
  animal_data,
  frac_gasm = 0.21,
  frac_leach = 0.24,
  ef_4 = 0.01,
  ef_5 = 0.011,
  climate_wet_or_dry
){
  if (climate_wet_or_dry == "wet"){ef_4=0.014}
  if (climate_wet_or_dry == "dry"){ef_4=0.005}
  
  if(nrow(animal_data) > 0){
    animal_data <- animal_data %>% 
      mutate(n2o_urine_dung_indirect = n_animals * grazing_days/365 * n_excretion_rate_kg_1000am * 365 * mass/1000 * 
               (frac_gasm * ef_4 + frac_leach * ef_5))
  }else{
    warning("No Animal data provided - or included in project")
  }
  return(animal_data)
}

n2o_manure_deposition_direct <- function(
  animal_data,
  ef_3_pasture = 0.004,
  ef_3_deep_bedding=0.01,
  climate_wet_or_dry
){
  if (climate_wet_or_dry == "wet"){ef_3_pasture=0.006}
  if (climate_wet_or_dry == "dry"){ef_3_pasture=0.002}
  if(nrow(animal_data) > 0){
    animal_data <- animal_data %>% 
      mutate(n2o_urine_dung_direct = n_animals * n_excretion_rate_kg_1000am * mass/1000 * 
               (grazing_days * ef_3_pasture + (365 - grazing_days)*ef_3_deep_bedding))
  }else{
    warning("No Animal data provided - or included in project")
  }
  return(animal_data)
}

