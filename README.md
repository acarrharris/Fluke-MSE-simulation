# Fluke-MSE-simulation
Management model for the fluke MSE

To start, gather the .xlsx and .R files in a directory, which you will enter in "modelling wrapper 2.R" 

"Modelling wrapper 2.R" first calibrates the model to 2019 fishing conditions for all states, using each states' "calibration2 ST.R" script. Then, the wrapper runs the model for the  prediciton year ("prediction2 ST.R"). For these years, catch-at-length for summer flounder is an output from "catch at length given stock structure - prediction.R". I have turned this script off for now, and am using the same catch-at-length distributions for both calibration and prediction. The "prediction2 ST.R"'s will use a dataset containing alternative regualtions ("directed_trips_region - alternative regs test.xlsx"), which you can be alter for any combination of bag limit, minimum size limit, and maximum size limit. Additional code will be needed for other types of regulations. 
