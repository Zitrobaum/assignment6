# get biocmanager
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install()
# install package
#BiocManager::install("HMM")
require(HMM)
# number of throws
nSim = 2000
# possible states of dice
States = c("encoding", "non encoding")
# possible result of throws
Symbols = c("A","C","G","T") # didnt work
Symbols = 1:4
# probability of changing dice
transProbs = matrix(c(0.99, 0.01, 0.02, 0.98), c(length(States), length(States)), byrow = TRUE)
# probability of each dice roll
emissionProbs = matrix(c(1/6,1/3,1/3,1/6, c(rep(0.25, 4))), c(length(States), length(Symbols)), byrow = TRUE)

# create hidden markov model out of previous data
hmm = initHMM(States, Symbols, transProbs = transProbs, emissionProbs =
                emissionProbs)
# start a simulation
sim = simHMM(hmm, nSim)

# determine when dice switches states
# most probably path of hidden markov model
vit = viterbi(hmm, sim$observation)

# forward probablities: probablity of sequence of observations
f = forward(hmm, sim$observation)

# define parameters out of forward probabilites
i <- f[1, nSim]
j <- f[2, nSim]

# calculate probablity of obversation
probObservations = (i + log(1 + exp(j - i)))

#########################################
## NO MORE DOCUMENTATION BELOW THIS LINE
#########################################
x = list(hmm = hmm, sim = sim, vit = vit)
# PLOT simulated throws at top #####################
mn = "CG and non CG sequences"
xlb = "Throw nr."
ylb = ""
plot(x$sim$observation, ylim = c(-7.5, 6), pch = 3, main = mn,
     xlab = xlb, ylab = ylb, bty = "n", yaxt = "n")
axis(2, at = 1:4)
# PLOT Simulated, which die was used (ground truth) ###########
text(0, -1.2, adj = 0, cex = 0.8, col = "black", "True: green = encoding")
for (i in 1:nSim) {
  if (x$sim$states[i] == "encoding")
    rect(i, -1, i + 1, 0, col = "green", border = NA)
  else rect(i, -1, i + 1, 0, col = "red", border = NA)
}
# PLOT Most probable path (viterbi) #######################
text(0, -3.2, adj = 0, cex = 0.8, col = "black", "Most probable path")
for (i in 1:nSim) {
  if (x$vit[i] == "encoding")
    rect(i, -3, i + 1, -2, col = "green", border = NA)
  else rect(i, -3, i + 1, -2, col = "red", border = NA)
}
# PLOT Differences ####################
text(0, -5.2, adj = 0, cex = 0.8, col = "black", "Difference")
differing = !(x$sim$states == x$vit)
for (i in 1:nSim) {
  if (differing[i])
    rect(i, -5, i + 1, -4, col = rgb(0.3, 0.3, 0.3),
         border = NA)
  else rect(i, -5, i + 1, -4, col = rgb(0.9, 0.9, 0.9),
            border = NA)
}


