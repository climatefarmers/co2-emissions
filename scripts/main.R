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

scenario_selected = "previous_years"
all_results_previous_years <- run_lca(inputs_loc,
        co2_emissions_loc,
        results_loc,
        data_loc = data_loc,
        scenario_selected
)
all_results_previous_years <- all_results_previous_years %>% rename(co2eq_previous_years=co2eq) %>%
  select(source, gas, co2eq_previous_years)

gas_summary_previous_years <- all_results_previous_years %>% 
  group_by(gas) %>% 
  summarise(co2eq = sum(co2eq_previous_years), .groups = "drop")

scenario_selected = "current"
all_results_current <- run_lca(inputs_loc,
                       co2_emissions_loc = working_dir,
                       results_loc,
                       data_loc = data_loc,
                       scenario_selected
                       )
all_results_current <- all_results_current %>% rename(co2eq_current=co2eq) %>%
  select(source, gas, co2eq_current)

gas_summary_current <- all_results_current %>%  group_by(gas) %>% 
  summarise(co2eq = sum(co2eq_current), .groups = "drop")

plot_source_breakdown(all_results_current  %>% rename(co2eq=co2eq_current) %>% filter (source!="leakage"), results_loc)#%>%  filter(source != "leakage")

write_csv(gas_summary_current, file.path(results_loc, "CO2eq_emissions_2021.csv"))
write_csv(all_results_current, file.path(results_loc, "CO2eq_emissions_source_2021.csv"))

write_csv(gas_summary_previous_years, file.path(results_loc, "CO2eq_emissions_2020.csv"))
write_csv(all_results_previous_years, file.path(results_loc, "CO2eq_emissions_source_2020.csv"))

all_results <- inner_join(all_results_previous_years, all_results_current, by="source") %>%
  mutate(co2eq_reductions=co2eq_previous_years-co2eq_current)%>%
  mutate(reduction_or_increase=ifelse(co2eq_previous_years-co2eq_current>0,"reduction","increase"))

write_csv(all_results, file.path(results_loc, "diff_btw_2021_2020.csv"))

name <-"Reductions_bar_chart"
graph <- ggplot(data = all_results %>% filter (source!="leakage"), aes(x=source, fill = reduction_or_increase))+
  geom_bar(aes(y = co2eq_reductions*1e-3), stat="identity",  alpha=0.7)+   
  labs(title = name)+
  xlab("Time")+
  ylab("tCO2 avoided")
print(graph)
png(file.path(results_loc,paste(name,".png",sep="")))
print(graph)
dev.off()



