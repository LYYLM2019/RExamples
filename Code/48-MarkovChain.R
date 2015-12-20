library(markovchain)
p01.gaze = factor(x = c("a", "b", "b", "a", "a", "a", 
                        "a", "a", "a", "a", "a", "a", 
                        "a", "b", "b", "a", "d", "d", 
                        "d", "a", "a", "a", "e", "e", 
                        "d", "e", "e", "a","a", "e", 
                        "a", "a", "a", "e", "e", "e", 
                        "e", "e", "e", "e", "e", "e", 
                        "e", "d", "b", "b", "b", "d", 
                        "d", "d", "d", "d", "d", "d", 
                        "b", "d", "d", "d", "d", "d", 
                        "d", "d", "d", "d", "d", "d", 
                        "d", "d", "d", "d", "d", "d", 
                        "d", "d", "d", "d", "d", "d", 
                        "d", "d", "d", "d", "d", "a", 
                        "b", "a", "d", "d", "a", "c", 
                        "e", "e", "e", "c", "c", "a", 
                        "e", "e", "a", "a", "a"))
p01_gaze_tpm = createSequenceMatrix(p01.gaze, toRowProbs = TRUE)
p01_gaze_mc = as(p01_gaze_tpm, "markovchain")
plot(p01_gaze_mc, edge.arrow.size = 0.2)
