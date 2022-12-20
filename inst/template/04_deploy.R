# edit parameters here
parm <- list(
  fitted_model_path = "my_captcha.pt",
  img_path = "img",
  release_tag = "captcha_model",
  path_training = "f_train.txt"
)

# Configure Git
## ignore files and model
usethis::use_git_ignore(parm$img_path)
usethis::use_git_ignore(parm$fitted_model_path)
usethis::use_git()

# Configure GitHub
usethis::use_github()

# Setup release
piggyback::pb_new_release(tag = parm$release_tag)
piggyback::pb_upload(parm$fitted_model_path, tag = parm$release_tag)
piggyback::pb_upload(parm$path_training, tag = parm$release_tag)

# (optional) upload image files
f_captcha <- fs::dir_ls(parm$img_path)
f_zip <- paste0(parm$img_path, ".zip")
utils::zip(f_zip, files = f_captcha, flags = "-r0Dq")
piggyback::pb_upload(f_zip, tag = parm$release_tag)
