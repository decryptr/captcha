f_captcha <- fs::dir_ls("img")

# Option 1: classify manually
captcha::classify(f_captcha, "img_train")

# Option 2: shiny app
captcha::classify_app(f_captcha, "img_train")
