test_that("Anonymous keys", {
   id <- getScimapId()
   expect_that(id, matches("[0-9][0-9][0-9]+"));
   expect_that(id, equals(getScimapId()));
   jobid <- scimapClient:::getJobId()
   expect_that(jobid, matches("[0-9][0-9][0-9]+"));
   expect_that(jobid, equals(getJobId()));
   expect_that(jobid, matches(paste(id, ".*", sep="")));
   instid <- scimapClient:::getInstallId()
   expect_that(instid, matches("[0-9][0-9][0-9]+"));
   expect_that(instid, equals(getInstallId()));
   expect_that(jobid, matches(paste(id, ".*", sep="")));
});

test_that("packet content", {
   expect_that(previewPacket(), matches(paste("\\{.*", getScimapId(), ".*\\}", sep="")));
   deanonymize("testuser","","");
   deps <- scimapClient:::justDependencies();
   expect_that(appendVersion("testthat") %in% names(deps$pkgT), is_true());
   expect_that(deps[["userMetadata"]][["ssnm_name"]], equals("testuser"));
});

test_that("Helper functions", {
   expect_that(scimapClient:::appendVersion("testthat"), matches("testthat/[0-9]+.[0-9]+.[0-9]+"));
});
