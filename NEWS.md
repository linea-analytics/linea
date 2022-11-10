# linea 0.1.3

In version 0.1.3 new functions, functions' arguments, and other adjustments have been implemented.

* New functions:
  * The function `combo_number()` returns the number of combinations contained in `what_combo()`'s input. 
  * The function `total_decomp_chart()` generates a decomposition like `decomp_chart()` but grouped and summed by variable or category.
  * The `gt_f()` function cites Google Trends as the data source.

* Function amends:
  * The `run_combo_model()` function no longer requires a model object to be provided, as it is returned by `what_combo()`'s output directly.
  * The `run_model()` function now produces also a vif column in the `model_output_table`.
  * `charting.R` functions allow for controlling chart and font colors. 
  
* Other amends:
  * Error and warnings messages have been amended to be printed appropriately.
  * Example data has been added to the package in the `/data` folder: `sales_ts.rda`,`pooled_gt_data.rda`.
