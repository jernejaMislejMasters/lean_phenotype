#!/usr/bin/python3

with open('list_of_described_variables') as described_variables_f:
    described_variables = described_variables_f.read().splitlines()
    
with open('list_of_dataset_variables') as dataset_variables_f:
    dataset_variables = dataset_variables_f.read().splitlines()    



for ds_var in range(0,len(dataset_variables)):
    if dataset_variables[ds_var] not in described_variables:
        print(dataset_variables[ds_var])