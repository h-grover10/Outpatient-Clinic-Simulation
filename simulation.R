library(simmer)
library(simmer.plot)

# create environment

env <- simmer("outpatient clinic")

env

## add resources

env %>%
  add_resource("nurse",3)%>%
  add_resource("doctor",4)%>%
  add_resource("administration",2)

env

# create patient trajectory

patient <- trajectory(name = "Patient Path", verbose = T)

patient

## draw model

patient %>% 
  
  seize("nurse",1) %>% ## need to define resources 
  timeout(function() rnorm(1,15)) %>% 
  release("nurse",1) %>%
  
  seize("doctor",1) %>% ## need to define resources 
  timeout(function() rnorm(1,20)) %>% 
  release("doctor",1) %>%
  
  seize("administration",1) %>% ## need to define resources 
  timeout(function() rnorm(1,5)) %>% 
  release("administration",1) 

## create patient generator
??add_generator
env %>%
  add_generator(name_prefix = "sim_patient_" , 
                trajectory = patient,
                distribution = function() rnorm(1,5,0.5))

env

env %>% run(until = 600)


## plot the model

plot(patient)
plot(patient, verbose = T)

## look under the hood

myarrivals = get_mon_arrivals(env)
env
119-74
45 - (38 + 2 + 3 + 1)

myarrivals_per_resource = get_mon_arrivals(env, per_resource = T)

myresources = get_mon_resources(env)

library(simmer)
library(simmer.plot)


# create patient trajectory

patient <- trajectory(name = "Patient Path", verbose = T)

patient

## draw model

patient %>% 
  
  seize("nurse",1) %>% ## need to define resources 
  timeout(function() rnorm(1,15)) %>% 
  release("nurse",1) %>%
  
  seize("doctor",1) %>% ## need to define resources 
  timeout(function() rnorm(1,20)) %>% 
  release("doctor",1) %>%
  
  seize("administration",1) %>% ## need to define resources 
  timeout(function() rnorm(1,5)) %>% 
  release("administration",1) 


time1 = Sys.time()


envs <- lapply(1:100, function(i) {
  simmer("outpatient clinic") %>%
    add_resource("nurse",3)%>%
    add_resource("doctor",4)%>%
    add_resource("administration",2) %>%
    add_generator(name = "sim_patient_" , 
                  trajectory = patient,
                  distribution = function() rnorm(1,5,0.5)) %>%
    run(600) %>%
    wrap()
})

time2 = Sys.time()

time2-time1


resources <- get_mon_resources(envs)
p1=plot(resources, metric = "utilization")


p2=plot(resources, metric = "usage", c("nurse", "doctor","administration"),
        items = c( "queue", "server"))

arrivals <- get_mon_arrivals(envs)


p3=plot(arrivals, metric = "waiting_time")
p4=plot(arrivals, metric = "activity_time")

library(gridExtra)
grid.arrange(p1,p2,p3,p4)

## same priority

library(simmer)
library(simmer.plot)



envs <- lapply(1:10, function(i) { 
  
  
  env <- simmer("hospital")
  
  patient <- trajectory("patients' path") %>%
    
    
    branch(function() sample(c(1,2),size=1,replace=T,prob=c(0.5,0.5)), 
           continue=c(T,T), 
           trajectory("A") %>%
             
             
             # static / dynamic values
             
             ## static
             # c(1st value = priority, preemptible level must be larger, restart=T/F)
             set_prioritization(values = c(3,7,T)) %>%
             
             ## add a consultation activity
             seize("doctor", 1) %>%
             timeout(function() rnorm(1, 20)) %>%
             release("doctor", 1) %>%
             
             ## give label no. 1 to patients passed through this path
             set_attribute("Arm",1)  %>%
             log_(message = "Priority A Patient finished Arm 1 !" )
           
           ,
           trajectory("B") %>%
             
             
             # static values
             
             set_prioritization(values = c(3,7,T)) %>%
             
             ## add a consultation activity
             seize("doctor", 1) %>%
             timeout(function() rnorm(1, 20)) %>%
             release("doctor", 1) %>%
             
             set_attribute("Arm",2) %>%## give label no. 2 to patients passed through this path
             log_(message = "Priority B Patient finished Arm 2!" )
           
    )
  
  
  
  
  
  
  env %>%
    
    add_resource("doctor", 2) %>%
    
    add_generator("patient", patient, function() 5, mon=2)
  
  
  env %>% 
    run(until=600)
  
})


#arriv<-get_mon_arrivals(envs)
#resourc<-get_mon_resources(envs)
attr<-get_mon_attributes(envs)
#arr_per_res<-get_mon_arrivals(envs, T)


table(attr$value)
table(attr$replication, attr$value)


## Diffrent priority B has more priority

library(simmer)
library(simmer.plot)



envs <- lapply(1:10, function(i) { 
  
  
  env <- simmer("hospital")
  
  patient <- trajectory("patients' path") %>%
    
    
    branch(function() sample(c(1,2),size=1,replace=T,prob=c(0.5,0.5)), 
           continue=c(T,T), 
           trajectory("A") %>%
             
             
             
             set_prioritization(values = c(3,7,T)) %>%
             
             ## add a consultation activity
             seize("doctor", 1) %>%
             timeout(function() rnorm(1, 20)) %>%
             release("doctor", 1) %>%
             
             ## give label no. 1 to patients passed through this path
             set_attribute("Arm",1)  %>%
             log_(message = "Priority A Patient finished Arm 1 !" )
           
           ,
           trajectory("B") %>%
             
             
             
             set_prioritization(values = c(5,7,T)) %>%
             
             ## add a consultation activity
             seize("doctor", 1) %>%
             timeout(function() rnorm(1, 20)) %>%
             release("doctor", 1) %>%
             
             set_attribute("Arm",2) %>%## give label no. 2 to patients passed through this path
             log_(message = "Priority B Patient finished Arm 2!" )
           
    )
  
  
  
  
  
  
  env %>%
    
    add_resource("doctor", 2) %>%
    
    add_generator("patient", patient, function() 5, mon=2)
  
  
  env %>% 
    run(until=600)
  
})


#arriv<-get_mon_arrivals(envs)
#resourc<-get_mon_resources(envs)
attr_d<-get_mon_attributes(envs)
#arr_per_res<-get_mon_arrivals(envs, T)


table(attr_d$value)
table(attr_d$replication, attr_d$value)
