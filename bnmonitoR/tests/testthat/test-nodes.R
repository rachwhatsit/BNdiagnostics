context("Node monitors")
data(asia)
asia.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]") #this is the candidate model from pg 240

test_that("correct output for node.tbl", {
   expect_true(is_tibble(node.monitor.tbl(dag = asia.dag,df = asia)))
    expect_equal(as.numeric(node.monitor.tbl(dag = asia.dag,df = asia)[2,2]),-5.81e-05-114.835)
  }
)
