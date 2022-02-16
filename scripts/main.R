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


run_lca(inputs_loc,
        lca_loc = working_dir,
        results_loc,
        data_loc = data_loc
)






