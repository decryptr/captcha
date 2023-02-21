# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# I changed it only to install torch when the user does not have it.

library(testthat)
library(captcha)

if (!torch::torch_is_installed()) {
  torch::install_torch()
}

test_check("captcha")
