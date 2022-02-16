# leakage functions


# Carbon share of manure come from: https://www.researchgate.net/publication/225385126_Carbon_resources_of_residue_and_manure_in_Japanese_farmland_soils/figures?lo=1

manure_leakage <- function(add_manure_data, field_area = 50){
  
  if(nrow(add_manure_data) > 0){
  add_manure_data <- add_manure_data %>% 
    mutate(co2_leakage = quantity_kg_ha * field_area * carbon_content * .12 * (44/12)) %>% 
    select(manure_source, co2_leakage)}else{
      add_manure_data <- create_empty_dataframe(c("manure_source", "co2_leakage"))
      warning("No additional manure data provided")
    }
  
  
  return(add_manure_data)
  
}

productivity_leakage <- function(){
  
  # TODO: Incorporate this as part of the monitoring report. 
  # This should check the changes in production and check whether there has been a reduction in productivity. 
  # This can only happen when there are results to compare to. 
  
}