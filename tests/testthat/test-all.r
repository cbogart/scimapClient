test_that("Disabled By Default", {
   expect_that(isEnabledScimap(), is_false());
});
test_that("Anonymous keys", {
   id <- generateScimapId()
   expect_that(id, matches("[0-9][0-9][0-9]+"));
});

test_that("packet content", {
   expect_that(previewPacket(), matches(paste("\\{.*", getScimapId(), ".*\\}", sep="")));
   deanonymize("testuser","","");
   deps <- scimapClient:::justDependencies();
   expect_that(appendVersion("testthat") %in% names(deps$pkgT), is_true());
   expect_that(deps[["userMetadata"]][["ssnm_name"]], equals("testuser"));
   addUserMetadata(list("asdf"="1234", "ssnm_name"="othertestuser"));
   deps <- scimapClient:::justDependencies();
   expect_that(deps[["userMetadata"]][["ssnm_name"]], equals(c("testuser", "othertestuser")));
   expect_that(deps[["userMetadata"]][["asdf"]], equals("1234"));
});

test_that("Helper functions", {
   expect_that(scimapClient:::appendVersion("testthat"), matches("testthat/[0-9]+.[0-9]+.[0-9]+"));
});
