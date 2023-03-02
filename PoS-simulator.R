#==============================================================================
#= PoS Simulator - Version 1.0 - (2023) by Alberto Leporati
#==============================================================================

library(DescTools)
#library(parallel)
#library(compiler)

# Definition of parameters
numberOfPeers = 1000
numberOfCorruptedPeers = 100
numberOfValidators = 20
minNumberOfTokensPerPeer = 1
maxNumberOfTokensPerPeer = 1000
stakeablePercentage = 50
numberOfRewardTokens = 10
percentageOfPenalty = 50
numberOfIterations = 2000

printInfoAtEachIteration = FALSE


# Print info about the token distribution: sum, mean, standard deviation,
# Gini coefficient
PrintInfoTokenDistribution = function(showDistribution = FALSE) {
  if (showDistribution) {
    cat("Token distribution:\n")
    cat(tokenDistribution)
    cat("\n\n")
  }
  cat(paste("Total number of tokens:", sum(tokenDistribution), "\n"))
  cat(paste("Mean number of tokens per peer:", mean(tokenDistribution), "\n"))
  cat(paste("Standard deviation of token distribution:", sd(tokenDistribution), "\n"))
  cat(paste("Gini coefficient of token distribution:",
            Gini(tokenDistribution, unbiased=FALSE), "\n\n"))
}


# Plot the distribution of tokens. By providing a filename, the plot is saved
# in a PDF file
PlotTokenDistribution = function(fileName = NULL) {
  if (!is.null(fileName)) pdf(fileName)
  plot(tokenDistribution, type = "h", col = "steelblue",
       xlab = "Peers", ylab = "Number of tokens")
  if (!is.null(fileName)) dev.off()
}

# Print the list of validators
PrintValidators = function() {
  cat("Validators: ")
  validators = sort(validators)
  cat(validators)
  cat("\n")
}


# Print the numner of staked tokens
PrintNumberOfStakedTokens = function() {
  numberOfStakedTokens = sum(stake)
  cat(paste("Number of staked tokens:", numberOfStakedTokens,
            "(", round(numberOfStakedTokens/stakeableTotal*100,2),
            "% of the stakeable tokens,",
            round(numberOfStakedTokens/sum(tokenDistribution)*100,2), 
            "% of the total tokens supply)\n"))
}


# Print the number and the list of corrupted validators
PrintCorruptedValidators = function() {
  cat(paste("Number of corrupted validators:", length(corruptedValidators), "\n"))
  cat("Corrupted validators: ")
  cat(corruptedValidators)
  cat("\n")
}


cat("============================================================\n")
cat("= PoS Simulator - Version 1.0 - (2023) by Alberto Leporati =\n")
cat("============================================================\n\n")
  
# Randomly choose the corrupted peers
corruptedPeers = sample(1:numberOfPeers, numberOfCorruptedPeers)
corruptedPeers = sort(corruptedPeers)

# Print the parameters
cat("Parameters:\n")
cat(paste("- Number of peers:", numberOfPeers, "\n"))
cat(paste("- Number of corrupted peers:", numberOfCorruptedPeers,
          "(", numberOfCorruptedPeers/numberOfPeers*100,
          "% of the total number of peers)\n"))
# Print the list of corrupted peers
cat("- Corrupted peers: ")
cat(corruptedPeers)
cat("\n")
cat(paste("- Number of validators:", numberOfValidators,
          "(", numberOfValidators/numberOfPeers*100,
          "% of the total number of peers)\n"))
cat(paste("- Minimum number of tokens per peer:", minNumberOfTokensPerPeer, "\n"))
cat(paste("- Maximum number of tokens per peer:", maxNumberOfTokensPerPeer, "\n"))
cat(paste("- Percentage of tokens that can be staked:", stakeablePercentage, "%\n"))
cat(paste("- Number of reward tokens for validators:", numberOfRewardTokens, "\n"))
cat(paste("- Percentage of tokens removed to corrupted validators:", percentageOfPenalty, "%\n\n"))
  
# Set the initial distribution of tokens
tokenDistribution = sample(x = c(minNumberOfTokensPerPeer:maxNumberOfTokensPerPeer),
                           size = numberOfPeers, replace = TRUE)
tokenDistribution = sort(tokenDistribution)  
  
# Print info about the initial token distribution
cat("--------------------------\n")
cat("Initial token distribution\n")
cat("--------------------------\n")
PrintInfoTokenDistribution()

# Plot the token distribution
PlotTokenDistribution()

# Iterations
for (iteration in 1:numberOfIterations) {
  if (printInfoAtEachIteration) {
    cat("--------------------\n")
    cat(paste("Iteration number", iteration, "\n"))
    cat("--------------------\n")
  }
  
  # Choose the validators, proportionally with respect to the maximum number of
  # tokens that each peer is willing to put in the stake, and computes the
  # actual stake
  stakeableTokens = floor((stakeablePercentage/100)*tokenDistribution)
  stakeableTotal = sum(stakeableTokens)
  stake = replicate(numberOfPeers, 0)
  validators = replicate(numberOfValidators, 0)
  i = 1
  # At each iteration, select the i-th validator
  while (i <= numberOfValidators) {
    r = sample(1:stakeableTotal, 1)
    # Given r, determine which peer becomes a validator
    s = 0
    j = 1
    while (s <= r) {
      s = s + stakeableTokens[j]
      j = j + 1
    }
    j = j - 1
    # If the validator has not already been chosen previously...
    if (stake[j] == 0) {
      #... add it to the list of validators,
      validators[i] = j
      # ... and put his stakeable tokens in the stake
      stake[j] = stakeableTokens[j]
      # Proceed with the choice of the next validator
      i = i + 1
    }
  }

  if (printInfoAtEachIteration) {
    PrintInfoTokenDistribution()
    # Print the list of validators chosen
    PrintValidators()
    # Print the number of staked tokens
    PrintNumberOfStakedTokens()
  }
  
  # Compute the list of corrupted validators
  corruptedValidators = intersect(validators, corruptedPeers)
  corruptedValidators = sort(corruptedValidators)
  if (printInfoAtEachIteration) {
    # Print the number and the list of corrupted validators
    PrintCorruptedValidators()  
  }
  
  # Tokens update
  # Remove staked tokens from the token distribution
  tokenDistribution = tokenDistribution - stake
  # Add rewards to all validators
  stake = stake + numberOfRewardTokens
  # Remove rewards and apply penalty to corrupted validators
  for (v in corruptedValidators) {
    stake[v] = stake[v] - numberOfRewardTokens
    stake[v] = floor(stake[v]*percentageOfPenalty/100)
  }
  # Update token distribution
  tokenDistribution = tokenDistribution + stake
}

# Print info about the final token distribution
cat("------------------------\n")
cat("Final token distribution\n")
cat("------------------------\n")
PrintInfoTokenDistribution()

# Plot the final token distribution
PlotTokenDistribution()
