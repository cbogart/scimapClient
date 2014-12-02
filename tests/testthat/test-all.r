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
   dict1 <- addMultiDict(list(a=list(1,2), b=list(3,4)), "a", 7)
   expect_that(dict1, equals(list(a=list(1,2,7),b=list(3,4))))
   dict2 <- addMultiDict(list(a=list(1,2), b=list(3,4)), "d", 7)
   expect_that(dict2, equals(list(a=list(1,2),b=list(3,4), d=list(7))))
   dict3 <- addMultiDict(dict2, "d", 8)
   expect_that(dict3, equals(list(a=list(1,2),b=list(3,4), d=list(7,8))))

   # Doesn't work: hard to test this one:
   #expect_that(scimapClient:::guessPackage(toString(sys.call(which=-1))), equals("testthat"));
});
