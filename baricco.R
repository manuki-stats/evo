game = function(n, r=9600){  #n = size of the four groups, r = a priori max number of units

res = rep(0, r-4*n)
ind1 = c(rep(1, 4*n), res)
ind0 = c(rep(0, 4*n), res)
inds = c(rep(1:4, n), res)

pop = data.frame( fertility = ind1, strength = ind1, mutability = ind1, 
                  teamwork = ind1, species = inds, food = ind1, 
                  hungry = ind0, offspring = ind0, alive = ind1, age = ind0 )

#bonus points per species

for(i in 1:4)
  pop[pop$species==i, i] = 1.5

key = 1

while(key == 1){

  pop = sorting(pop)  #put them at the top, reshuffled (a way to simulate random encounters between the living units)

  pop[pop$alive==1,] = day(pop[pop$alive==1,])

  pop$alive[pop$hunger==2] = 0  #kill the hungry

  pop = sorting(pop)

  pop[pop$alive==1,] = sort_species(pop[pop$alive==1,])  #regroup by species (for reproduction)
  
  pop = night(pop)

  pop$age[pop$alive==1] = pop$age[pop$alive==1] + 1  #update age

  pops = split(pop[pop$alive==1, -c(5, 9:10)], pop$species)
  print( lapply( pops, summary )[-1] )

  print( paste( "Living units of species 1:", nrow( pop[pop$alive==1 & pop$species==1,] ) ) )
  print( paste( "Living units of species 2:", nrow( pop[pop$alive==1 & pop$species==2,] ) ) )
  print( paste( "Living units of species 3:", nrow( pop[pop$alive==1 & pop$species==3,] ) ) )
  print( paste( "Living units of species 4:", nrow( pop[pop$alive==1 & pop$species==4,] ) ) )	

  pop$food[pop$alive==1] = 0  #clear food

  key = as.integer( readline("Press 1 to repeat") )
  
  }

}  #end of the game


day = function(pop_alive){

num_alive = nrow(pop_alive)

index = seq(1, num_alive-1, by=2)  #the last one doesn't "play" if num_alive is odd

for(i in index){

  #if the two units belong to different species they fight

  if( ! pop_alive$species[i] == pop_alive$species[i+1] ){
  
    ber = sample(0:1, 1)

    #same strength: growth by pressure

    pop_alive$strength[i+ber] = pop_alive$strength[i+ber] + .1*( pop_alive$strength[i] == pop_alive$strength[i+1] )
	
    #winner takes one piece of food, loser gets a little hungry

    stronger = which.max(pop_alive$strength[i:(i+1)])
    weaker = which.min(pop_alive$strength[i:(i+1)])

    stronger_wins = rbinom( 1, 1, max(pop_alive$strength[i:(i+1)])/sum(pop_alive$strength[i:(i+1)]) )

    pop_alive$food[i:(i+1)][stronger] = 1*stronger_wins
    pop_alive$food[i:(i+1)][weaker] = 1 - 1*stronger_wins
  
    pop_alive$hungry[i:(i+1)][stronger] = 1 - 1*stronger_wins
    pop_alive$hungry[i:(i+1)][weaker] = 1*stronger_wins

    }

  #same species: cooperation

  else{

    pop_alive$food[i:(i+1)] = as.integer( ( 1 + min( pop_alive$teamwork[i], pop_alive$teamwork[i+1] ) )/2 )

    #giving a certain amount of food to both, clearing their hunger

    pop_alive$hungry[i:(i+1)] = 0

    }

  }  #end of for cycle

return(pop_alive)

}  #end of the day


night = function(pop){

pop_alive = pop[pop$alive==1,]
nums = table(pop_alive$species)
num_alive = sum(nums)

#counting the living population for each group

for(i in 1:4){

  index = seq(1, nums[i]-1, by=2)

  for(j in index){

    coeff1 = pop_alive$fertility[pop_alive$species==i][j]*pop_alive$food[pop_alive$species==i][j]
    coeff2 = pop_alive$fertility[pop_alive$species==i][j+1]*pop_alive$food[pop_alive$species==i][j+1]

    nkids = as.integer( 1 + sqrt(min(coeff1, coeff2)) )

    pop_alive$offspring[pop_alive$species==i][j:(j+1)] = pop_alive$offspring[pop_alive$species==i][j:(j+1)] + nkids

    # ^ just an arbitrary formula to determine the number of kids each couple gets, depending on food and fertility

    for(x in 1:nkids)
      pop[rowSums(pop)==0,][1,] = birth( pop_alive[pop_alive$species==i,][j,], pop_alive[pop_alive$species==i,][j+1,] )

    p = sample(1:4, 1)  #random mutations

    pop_alive[pop_alive$species==i,][j, p] = pop_alive[pop_alive$species==i,][j, p] + as.integer( rbinom(1, 1, .25)*pop_alive$mutability[pop_alive$species==i][j] )/10

    p = sample(1:4, 1)

    pop_alive[pop_alive$species==i,][j, p] = pop_alive[pop_alive$species==i,][j+1, p] + as.integer( rbinom(1, 1, .25)*pop_alive$mutability[pop_alive$species==i][j+1] )/10

    }

  }

pop[1:num_alive,] = pop_alive

return(pop)

}  #end of the night


sorting = function(pop){
  num_alive = nrow(pop[pop$alive==1,])
  return( rbind( pop[pop$alive==1,][sample(1:num_alive),], pop[pop$alive==0,] ) )
  }

sort_species = function(pop_alive)
  return( pop_alive[order(pop_alive$species),] )


birth = function(p1, p2){
	
  kid = rep(1, 10)

  for(k in 1:4)
  	kid[k] = (p1[k]+p2[k])/2

  kid[5] = p1[5]  #hopefully it's the same as p2[5]
  kid[c(7:8, 10)] = 0

  return( as.vector( as.numeric(kid) ) )
	
  }  #kids hereditate their parents' genes

