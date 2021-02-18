load('visualization/shiny/saved.simset.Rdata')
int = attr(simset, 'intervention')

comparator = intervention.from.code('ybhm.6m.50.90.3y')

interventions.equal(int, comparator)

target.populations.equal(int$raw$testing$target.populations[[1]],
                         comparator$raw$testing$target.populations[[1]])


tpop.1 = int$raw$testing$target.populations[[1]]
tpop.0 = comparator$raw$testing$target.populations[[1]]

sum(tpop.1 != tpop.0)

tpop.1[1,1,,]
tpop.0[1,1,,]

tpop.1[,1,1,]
tpop.0[,1,2,]

tpop.1[1,,1,]
tpop.0[1,,2,]
