test_that("tensor product", {
  along_idx <- list(
    1,2,3,4,c(1,2), c(2,3), c(3,4), c(1,3), c(2,4), c(1,4), c(1,2,3,4),
    c(1,2,3), c(2,3,4), c(1,3,4), c(1,2,4)
  )

  A <- rnorm(120); dim(A) <- 2:5; dimA <- dim(A)
  B <- rnorm(120); dim(B) <- c(1,3,5,2,4); dimB <- dim(B)
  B <- as.farray(B)

  lapply(along_idx, function(alongA){
    alongB <- c(3,1,4,2)[alongA] + 1
    dest <- tensorprod(A,B,alongA=alongA, alongB=alongB)
    comp <- tensor::tensor(A[drop=FALSE], B[drop=FALSE], alongA, alongB)
    rg <- range(as.vector(dest[]) - as.vector(comp))
    expect_lt(max(abs(rg)), 1e-10)
    dest$remove_data(force = TRUE, warn = FALSE)
  })


})

# a <- tensor::tensor(A[], B[])
# apply(a, c(1,2,4:7), sum) - comp
