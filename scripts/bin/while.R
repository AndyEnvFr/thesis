death = 0
landscape = 2500
event = 0
mortality_strength = 10
base_fitness = 1

print("calculating death loop")

# ndd scenario
while (death < landscape) {
  event = event + 1
  
  if(event %% mortality_strength != 0){
    
    survival = base_fitness - rbeta(n = 1, 0.5, 20)
    death = runif(n = 1, min = 0, max = 3)
    if(survival > death){
      next
    }
  }
  death = death + 1
}

cat("nDD event: ", event)

# pdd scenario
while (death < landscape) {
  event = event + 1
  
  if(event %% mortality_strength != 0){
    
    survival = base_fitness + rbeta(n = 1, 0.5, 20)
    death = runif(n = 1, min = 0, max = 3)
    if(survival > death){
      next
    }
  }
  death = death + 1
}
cat("pDD event: ", event)