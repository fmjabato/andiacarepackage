dummy_andiacare <- data.frame(TIR = c(100,50,26,10), 
                              TBR = c(0,0,0,0))

test_that("Classify using andiacare", {
  expect_equal("Green",andiacare(dummy_andiacare[1,]))
  expect_equal("Yellow",andiacare(dummy_andiacare[2,]))
  expect_equal("Orange",andiacare(dummy_andiacare[3,]))
  expect_equal("Red",andiacare(dummy_andiacare[4,]))
})

colnames(dummy_andiacare) <- c("A","B")
cnames <- list(TIR = "A", TBR = "B")

test_that("Classify using andiacare wrappers",{
  expect_equal("Green",andiacareWrapper(dummy_andiacare[1,], cnames))
  expect_equal("Yellow",andiacareWrapper(dummy_andiacare[2,], cnames))
  expect_equal("Orange",andiacareWrapper(dummy_andiacare[3,], cnames))
  expect_equal("Red",andiacareWrapper(dummy_andiacare[4,], cnames))  
  expect_null(andiacareWrapper(dummy_andiacare[4,], cnames, "extra"))
})


dummy_gri <- data.frame(TIR = c(91,84,76,52,45),
                        TBR = c(3,7,9,1,5),
                        TBRS = c(3,7,9,1,5),
                        TAR = c(6,9,15,47,50),
                        TARS = c(0,9,15,47,50),
                        Class = c("Green","Yellow","Orange",
                                  "Red","Brown"))

test_that("Classify using gri classifier",{
  expect_equal(dummy_gri$Class[1], gri(dummy_gri[1,]))
  expect_equal(dummy_gri$Class[2], gri(dummy_gri[2,]))
  expect_equal(dummy_gri$Class[3], gri(dummy_gri[3,]))
  expect_equal(dummy_gri$Class[4], gri(dummy_gri[4,]))
  expect_equal(dummy_gri$Class[5], gri(dummy_gri[5,]))
})

colnames(dummy_gri) <- c("A","B","C","D","E","Class")
cnames <- list(TIR="A",TBR="B",TBRS="C",TAR="D",TARS="E")
dummy_gri_extra <- data.frame(GRIVal = 95,
                              HypoglycemiaC = 5,
                              HyperglycemiaC = 50)

test_that("Classify using gri wrapper",{
  expect_equal(dummy_gri$Class[1], griWrapper(dummy_gri[1,],cnames))
  expect_equal(dummy_gri$Class[2], griWrapper(dummy_gri[2,],cnames))
  expect_equal(dummy_gri$Class[3], griWrapper(dummy_gri[3,],cnames))
  expect_equal(dummy_gri$Class[4], griWrapper(dummy_gri[4,],cnames))
  expect_equal(dummy_gri$Class[5], griWrapper(dummy_gri[5,],cnames))

  expect_equal(dummy_gri_extra$GRIVal[1], 
               griWrapper(dummy_gri[5,],cnames,"extra")$GRIVal)
  expect_equal(dummy_gri_extra$HypoglycemiaC[1], 
               griWrapper(dummy_gri[5,],cnames,"extra")$HypoglycemiaC)
  expect_equal(dummy_gri_extra$HyperglycemiaC[1], 
               griWrapper(dummy_gri[5,],cnames,"extra")$HyperglycemiaC)
})


test_that("Classify using general classifier API", {
  expect_equal("Green", classifyPatient(dummy_andiacare[1,], 
                                        "Andiacare",cnames))
  expect_null(classifyPatient(dummy_andiacare[1,],"Andiacare",cnames,"extra"))

  expect_equal("Green", classifyPatient(dummy_gri[1,],"GRI",cnames))
  expect_equal(dummy_gri_extra$GRIVal[1], 
               classifyPatient(dummy_gri[5,],"GRI", cnames,"extra")$GRIVal)
})


test_that("Blue flag classify",{
  dummy_blue <- data.frame(MeanReads = 4)
  expect_false(isBlueAndiacare(dummy_blue))
  expect_true(isBlueAndiacare(dummy_blue,thr = 5))
  expect_equal(NA, isBlueAndiacare(dummy_blue, readsCol = "fakeCol"))

})