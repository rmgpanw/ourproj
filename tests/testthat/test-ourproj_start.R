test_that("`ourproj_start` runs without error", {
  ourproj_gl <- "ourproj_gl"
  ourproj_gh <- "ourproj_gh"

  # create projects in tempdir (GiHub/GitLab, full/minimal)
  ourproj_start(
    directory = file.path(tempdir(), ourproj_gl),
    project_title = "Gitlab project",
    git_username = "rmgpanw",
    github_gitlab = "gitlab",
    template = 'workflowr_targets'
  )

  ourproj_start(
    directory = file.path(tempdir(), ourproj_gh),
    project_title = "GitHub project",
    git_username = "rmgpanw",
    github_gitlab = "github",
    template = 'workflowr_targets'
  )

  # check for appropriately named .Rproj file in each newly created directory
  expect_true(file.exists(file.path(
    tempdir(), ourproj_gl, paste0(ourproj_gl, ".Rproj")
  )))

  expect_true(file.exists(file.path(
    tempdir(), ourproj_gh, paste0(ourproj_gh, ".Rproj")
  )))

})
