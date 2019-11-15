context("Parent child monitors")
data(asia)

asia.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") #this is the candidate model from pg 240


test_that("valid parent nodes", {
  expect_equal(seq.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="T", pa.names = "A", pa.val = 'yes',alpha=2)[1], 1.88e-08-0.7071068)
  expect_error(seq.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="T", pa.names = 1, pa.val = 'yes',alpha=2), "Only strings can be converted to symbols")

})

test_that("valid node name",{
  expect_error(seq.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name=7, pa.names = "A", pa.val = 'yes',alpha=2),"invalid 'times' argument")
})

test_that("valid pa.vals", {
  expect_error(seq.pa.ch.monitor(dframe=asia, dag=asia.dag, node.name="T", pa.names = "A", pa.val = 2,alpha=2),"missing value where TRUE/FALSE needed")
})

