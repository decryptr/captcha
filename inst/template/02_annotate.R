# Annotate captchas -------------------------------------------------------

# load captchas
img_path <- "img"
f_captcha <- fs::dir_ls(img_path)

captcha::captcha_annotate(f_captcha, img_path)

