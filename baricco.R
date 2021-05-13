game = function(n, r=9600){  #n = size of the four groups, r = a priori max number of units

res = rep(0, r-4*n)
ind1 = c(rep(1, 4*n), res)
ind0 = c(rep(0, 4*n), res)
inds = c(rep(1:4, n), res)

pop = data.frame( fertility = ind1, strength = ind1, mutability = ind1, 
                  teamwork = ind1, species = inds, food = ind1, 
                  hungry = ind0, offspring = ind0, alive = ind1, age = ind0 )

#bonus points per species

pop[pop$species==1, 1] = 1.5  #fertility
pop[pop$species==2, 2] = 1.5  #strength
pop[pop$species==3, 3] = 1.5  #mutability
pop[pop$species==4, 4] = 1.5  #teamwork

key = 1

while(key == 1){

  num_alive = nrow(pop[pop$alive==1,])  #count the living

  pop = sorting(pop, num_alive)  #put them at the top, reshuffled (a way to simulate random encounters between the living units)

  pop = day(pop, num_alive)

  pop[pop$hunger==2, 9] = 0  #kill the hungry

  num_alive = nrow(pop[pop$alive==1,])

  pop = sorting(pop, num_alive)

  pop = night_sort(pop)  #regroup by species (for reproduction)
  
  pop = night(pop)

  pop[pop$alive==1, 10] = pop[pop$alive==1, 10] + 1  #update age

  pops = split(pop[pop$alive==1, -c(5, 9:10)], pop$species)
  print( lapply( pops, summary )[-1] )

  pop[pop$alive==1, 6] = 0  #clear food

  key = as.integer( readline("Press 1 to repeat") )
  
  }

}  #end of the game


day = function(pop, num_alive){

index = seq(1, num_alive-1, by=2)  #the last one doesn't "play" if num_alive is odd

for(i in index){

#if the two units belong to different species they fight

if( ! pop[pop$alive==1,][i, 5] == pop[pop$alive==1,][i+1, 5] ){
  
  ber = sample(0:1, 1)

  #same strength: growth by pressure

  if( pop[pop$alive==1,][i, 2] == pop[pop$alive==1,][i+1, 2] )
    pop[pop$alive==1,][i+ber, 2] = pop[pop$alive==1,][i+ber, 2] + .1
	
  #winner takes one piece of food, loser gets a little hungry (tried to avoid some ifs here, but it's probably worse this way)

  pop[pop$alive==1,][i:(i+1),][ pop[pop$alive==1,][i:(i+1),][,2] == max(pop[pop$alive==1,][i:(i+1),][,2]), 6 ] = 1
  pop[pop$alive==1,][i:(i+1),][ pop[pop$alive==1,][i:(i+1),][,2] == min(pop[pop$alive==1,][i:(i+1),][,2]), 6 ] = 0

  pop[pop$alive==1,][i:(i+1),][ pop[pop$alive==1,][i:(i+1),][,2] == max(pop[pop$alive==1,][i:(i+1),][,2]), 7 ] = 0
  pop[pop$alive==1,][i:(i+1),][ pop[pop$alive==1,][i:(i+1),][,2] == min(pop[pop$alive==1,][i:(i+1),][,2]), 7 ] = pop[pop$alive==1,][i:(i+1),][pop[pop$alive==1,][i:(i+1),][,2] == min(pop[pop$alive==1,][i:(i+1),][,2]), 7] + 1

  }

#same species: cooperation

if( pop[pop$alive==1,][i, 5] == pop[pop$alive==1,][i+1, 5] ){

  pop[pop$alive==1,][c(i, i+1), 6] = as.integer( ( 1 + min( pop[pop$alive==1,][i, 4], pop[pop$alive==1,][i, 4] ) )/2 )

  #giving a certain amount of food to both, clearing their hunger

  pop[pop$alive==1,][c(i, i+1), 7] = 0

  }

}  #end of for cycle

return(pop)

}  #end of the day


night = function(pop){

nums = c( nrow(pop[pop$alive==1 & pop$species==1,]), nrow(pop[pop$alive==1 & pop$species==2,]), nrow(pop[pop$alive==1 & pop$species==3,]), nrow(pop[pop$alive==1 & pop$species==4,]) )

#counting the living population for each group

for(i in 1:4){

  index = seq(1, nums[i]-1, by=2)

  for(j in index){

    coeff1 = pop[pop$alive==1 & pop$species==i,][j, 1]*pop[pop$alive==1 & pop$species==i,][j, 6]
    coeff2 = pop[pop$alive==1 & pop$species==i,][j+1, 1]*pop[pop$alive==1 & pop$species==i,][j+1, 6]

    nkids = 1 + sqrt(min(coeff1, coeff2))

    pop[pop$alive==1 & pop$species==i,][j, 8] = pop[pop$alive==1 & pop$species==i,][j, 8] + nkids
    pop[pop$alive==1 & pop$species==i,][j+1, 8] = pop[pop$alive==1 & pop$species==i,][j+1, 8] + nkids

    # ^ just an arbitrary formula to determine the number of kids each couple gets, depending on food and fertility

    for(x in 1:nkids)
      pop[rowSums(pop)==0,][1,] = birth( pop[pop$alive==1 & pop$species==i,][j,], pop[pop$alive==1 & pop$species==i,][j+1,] )

    p = sample(1:4, 1)  #random mutations

    pop[pop$alive==1 & pop$species==i,][j, p] = pop[pop$alive==1 & pop$species==i,][j, p] + as.integer( rbinom(1, 1, .25)*pop[pop$alive==1 & pop$species==i,][j, 3] )/10

    p = sample(1:4, 1)

    pop[pop$alive==1 & pop$species==i,][j+1, p] = pop[pop$alive==1 & pop$species==i,][j+1, p] + as.integer( rbinom(1, 1, .25)*pop[pop$alive==1 & pop$species==i,][j+1, 3] )/10

    }

  }

return(pop)

}  #end of the night


sorting = function(pop, num_alive)
  return( rbind( pop[pop$alive==1,][sample(1:num_alive),], pop[pop$alive==0,] ) )


night_sort = function(pop)
  return( rbind( pop[pop$alive==1,][order(pop[pop$alive==1,]$species),], pop[pop$alive==0,] ) )


birth = function(p1, p2){
	
  kid = rep(1, 10)

  for(k in 1:4)
  	kid[k] = (p1[k]+p2[k])/2

  kid[5] = p1[5]  #hopefully it's the same as p2[5]
  kid[c(7:8, 10)] = 0

  return(kid)
	
  }  #kids hereditate their parents' genes


