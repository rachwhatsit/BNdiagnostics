context("Global monitors")
data(asia)

asia.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") #this is the candidate model from pg 240

test_that("testing a valid node ", {
  expect_equal(global.monitor.bn.node(node.idx = 4,dag = asia.dag,alpha = 2,df = asia), 1.74e-06-19.5535)
  expect_error(global.monitor.bn.node(node.idx = 9,dag = asia.dag,alpha = 2,df = asia), "invalid 'times' argument" )#bigger node
  expect_error(global.monitor.bn.node(node.idx = "T",dag = asia.dag,alpha = 2,df = asia), "invalid 'times' argument")
})

test_that("valid output", {
  expect_true(is_tibble(global.monitor.tbl(asia.dag, alpha = 2, df=asia)))
  expect_error(global.monitor.bn.node(node.idx = 4,dag = as.grain(asia.dag),alpha = 2,df = asia), "no applicable method for 'as.grain' applied to an object of class \"bn\"" )
  expect_error(global.monitor.bn.node(node.idx = 4,dag = asia.dag,alpha = "2",df = asia),"non-numeric argument to binary operator")
  expect_equal(global.monitor.bn.node(node.idx = 4,dag = asia.dag,alpha = 2,df = as_tibble(asia)),0)
})


