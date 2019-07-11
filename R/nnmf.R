
# clusta_nnmf = function(m, k = 10, sd.cutoff = 0.8) {
#     if (any(m < 0)) {
#         print('Setting negative values to 0')
#         m[m < 0] <- 0
#     }
#     obj = NMFN::nnmf(m, k = k)
#     i = which(apply(obj$W, 2, sd) >= sd.cutoff)
#     programs = obj$W[,i]
#     programs = as.list(as.data.frame(programs))
#     sapply(programs, stats::setNames, rownames(obj$W), simplify = F)
# }


clusta_nnmf = function(m, k = 10, sd.cutoff = 0.8) {
    if (any(m < 0)) {
        print('Setting negative values to 0')
        m[m < 0] <- 0
    }
    obj = NMFN::nnmf(m, k = k)
    i = which(apply(obj$H, 1, sd) >= sd.cutoff)
    programs = obj$W[,i]
    programs = as.list(as.data.frame(programs))
    sapply(programs, stats::setNames, rownames(obj$W), simplify = F)
}
