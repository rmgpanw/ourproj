test_that("`ourproj_start` runs without error", {
  ourproj_gl <- "ourproj_gl"
  ourproj_gh <- "ourproj_gh"
  ourproj_gl_minimal <- "ourproj_gl_minimal"
  ourproj_gh_minimal <- "ourproj_gh_minimal"

  # create projects in tempdir (GiHub/GitLab, full/minimal)
  ourproj_start(
    directory = file.path(tempdir(), ourproj_gl),
    project_title = "Gitlab project",
    git_username = "rmgpanw",
    github_gitlab = "gitlab",
    minimal = FALSE
  )

  ourproj_start(
    directory = file.path(tempdir(), ourproj_gh),
    project_title = "GitHub project",
    git_username = "rmgpanw",
    github_gitlab = "github",
    minimal = FALSE
  )

  ourproj_start(
    directory = file.path(tempdir(), ourproj_gl_minimal),
    project_title = "Gitlab project",
    git_username = "rmgpanw",
    github_gitlab = "gitlab",
    minimal = TRUE
  )

  ourproj_start(
    directory = file.path(tempdir(), ourproj_gh_minimal),
    project_title = "Gitlab GitHub",
    git_username = "rmgpanw",
    github_gitlab = "github",
    minimal = TRUE
  )

  # check for appropriately named .Rproj file in each newly created directory
  expect_true(file.exists(file.path(
    tempdir(), ourproj_gl, paste0(ourproj_gl, ".Rproj")
  )))

  expect_true(file.exists(file.path(
    tempdir(), ourproj_gh, paste0(ourproj_gh, ".Rproj")
  )))

  expect_true(file.exists(file.path(
    tempdir(), ourproj_gl_minimal, paste0(ourproj_gl_minimal, ".Rproj")
  )))

  expect_true(file.exists(file.path(
    tempdir(), ourproj_gh_minimal, paste0(ourproj_gh_minimal, ".Rproj")
  )))
})
