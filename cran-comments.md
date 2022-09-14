## Re-Submission

This is a re-submission. In version 0.0.3 the function `gt_f` wasn't failing gracefully. Version 0.1.1 addresses this by handling the error and returning an informative message.

* Amended:
  * `gt_f()` now fails gracefully with an informative message

Beyond this, major adjustments have been implemented in version 0.1.1. Core functions have been simplified so that inputs do not need to be passed to functions repeatedly. Instead, these are contained in the output object (i.e. the model object). Further more, functions have been added to cover supporting features linked to pooled regression and `lm()`'s offset.

* Amends:
  * The meta_data input has been dropped as redundant. The functions now only rely on `pool_var` for pooled regressions.
  * The data object passed to `run_model()` is stored in the output object.
  * `run_model()`'s `output_model_table` now includes the regression's VIF.
  * `build_formula()` now allows for fixed coefficients.
  * The google trends wrapper `gt_f()` fails gracefully.

* Functions:
  * `filter_decomp_pool()`
  * `add_total_pool()`
  * `add_total_pool_to_data()`

* Features:
  * `decomping()` allows to measure tails of decomposition over time.
