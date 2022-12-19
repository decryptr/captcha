# Classify captchas -------------------------------------------------------

# load captchas
img_path <- "img"
f_captcha <- fs::dir_ls(img_path)

captcha::classify(f_captcha, img_path)

