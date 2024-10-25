This model was developed for an Early Value Assessment (EVA) of point-of-care tests for urinary tract infections to improve antimicrobial prescribing. Further details about this EVA can be found on the NICE website at https://www.nice.org.uk/guidance/hte7

The External Assessment Report produced by the Bristol Technology Assessment Group can be found at https://www.nice.org.uk/guidance/hte7/documents/committee-papers-2

Section 6 of the External Assessment Report describes the methods used to develop this model. The model code is hopefully fairly self-explanatory when read in conjunction with Section 6 of the report.

The model currently contains several NA values where we are lacking data, and these will need to be populated in order to obtain meaningful results.

Changes to which tests, patient subgroups and antibiotics are included can be made by editing the test_names, patient_subgroup_names and treatment_costs variables respectively. If these are changed then the mapping variables (mapped_tests and mapped_tests_treatment_costs) will also need to be updated manually with the relevant information.

The number of PSA samples used, n_samples, will need to be changed to an appropriate number for the analysis.
