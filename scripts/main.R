# This script calls in the different functions that make up the LCA

library(ggplot2);
library(dplyr);
library(tidyr);
library(deSolve);
library(readr);
library(rjson);
library(ggrepel)

working_dir <- getwd()

source(file.path(working_dir, "scripts", "run_lca.R"))

parameters <- fromJSON(file = file.path(working_dir, "parameter_files", "init.json"))

project_name = parameters$project_name

project_loc = file.path(parameters$project_loc, project_name)
results_loc <- file.path(project_loc, "results")
if(!dir.exists(results_loc)){dir.create(results_loc)}
inputs_loc <- file.path(project_loc, "inputs")
if(!dir.exists(inputs_loc)){dir.create(inputs_loc)}

data_loc <- file.path(parameters$data_loc)
co2_emissions_loc <- working_dir

scenario_selected = "previous_year"
all_results_previous_year <- run_lca(inputs_loc,
        co2_emissions_loc,
        results_loc,
        data_loc = data_loc,
        scenario_selected
)

gas_summary_previous_year <- all_results_previous_year %>% 
  group_by(gas) %>% 
  summarise(co2eq = sum(co2eq), .groups = "drop")

scenario_selected = "current"
all_results_current <- run_lca(inputs_loc,
                       co2_emissions_loc = working_dir,
                       results_loc,
                       data_loc = data_loc,
                       scenario_selected
                       )
gas_summary_current <- all_results_current %>% 
  group_by(gas) %>% 
  summarise(co2eq = sum(co2eq), .groups = "drop")

plot_source_breakdown(all_results , results_loc)#%>%  filter(source != "leakage")

write_csv(gas_summary, file.path(results_loc, "lca_results.csv"))
write_csv(all_results, file.path(results_loc, "lca_results_source.csv"))






