# linea 0.1.1

In version 0.1.1 major adjustments have been implemented. Core functions have been simplified so that inputs do not need to be passed to functions repeatedly. Instead, these are contained in the output object (i.e. the model object). Further more, functions have been added to cover supporting features linked to pooled regression and `lm()`'s offset.

* Amends:
  * The meta_data input has been dropped as redundant. The functions now only rely on `pool_var` for pooled regressions.
  * The data object passed to `run_model()` is stored in the output object.
  * `run_model()`'s `output_model_table` now includes the regression's VIF.[in-progress]

* Functions:
  * `filter_decomp_pool()`
  * `add_total_pool()`
  * `add_total_pool_to_data()`
  * `get_offset()`

* Features:
  * `decomping()` allows to measure tails of decomposition over time.
