# ============================================================
#  Symulacje Algorytmów Planowania Przydziału Procesora
#  FCFS, SJF, Round Robin
# ============================================================

if (!requireNamespace("simmer", quietly = TRUE))      install.packages("simmer")
if (!requireNamespace("simmer.plot", quietly = TRUE)) install.packages("simmer.plot")
if (!requireNamespace("ggplot2", quietly = TRUE))     install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE))       install.packages("dplyr")

library(simmer)
library(simmer.plot)
library(ggplot2)
library(dplyr)

# ============================================================
#  PARAMETRY SYMULACJI (wspólne dla wszystkich algorytmów)
# ============================================================
LAMBDA        <- 0.8   # Intensywność napływu zadań [zadań/jedn. czasu]
MU            <- 1.2   # Średni czas wykonania zadania [jedn. czasu]
QUANTUM       <- 2.0   # Kwant czasu dla Round Robin [jedn. czasu]
NOISE_MEAN    <- 0.0   # Średnia szumu - przełączanie kontekstu
NOISE_SD      <- 0.05  # Odchylenie standardowe szumu
SIM_TIME      <- 200   # Całkowity czas symulacji [jedn. czasu]
SEED          <- 42    # Ziarno losowości

# ============================================================
#  FUNKCJE POMOCNICZE
# ============================================================
next_arrival_gap <- function() rexp(1, rate = LAMBDA)
generate_service_time <- function() rexp(1, rate = MU)
generate_noise <- function() max(0, rnorm(1, mean = NOISE_MEAN, sd = NOISE_SD))

# ============================================================
#  1. FCFS
# ============================================================
simulate_fcfs <- function() {
  set.seed(SEED)
  env <- simmer("FCFS")
  
  trajectory_fcfs <- trajectory("FCFS Trajectory") %>%
    set_attribute("service_time", function() generate_service_time() + generate_noise()) %>%
    seize("cpu", 1) %>%
    timeout(function() get_attribute(env, "service_time")) %>%
    release("cpu", 1)
  
  env %>%
    add_resource("cpu", capacity = 1, queue_size = Inf) %>%
    add_generator("job", trajectory_fcfs, function() next_arrival_gap(), mon = 2) %>%
    run(until = SIM_TIME)
  
  return(env)
}

# ============================================================
#  2. SJF
# ============================================================
simulate_sjf <- function() {
  set.seed(SEED)
  env <- simmer("SJF")
  
  trajectory_sjf <- trajectory("SJF Trajectory") %>%
    set_attribute("service_time", function() generate_service_time() + generate_noise()) %>%
    # W SJF mniejszy czas wykonania = wyższy priorytet 
    # (w simmer większa liczba = wyższy priorytet, więc dajemy minus)
    set_attribute("priority", function() -get_attribute(env, "service_time")) %>%
    seize("cpu", 1, priority = function() get_attribute(env, "priority")) %>%
    timeout(function() get_attribute(env, "service_time")) %>%
    release("cpu", 1)
  
  env %>%
    add_resource("cpu", capacity = 1, queue_size = Inf) %>%
    add_generator("job", trajectory_sjf, function() next_arrival_gap(), mon = 2) %>%
    run(until = SIM_TIME)
  
  return(env)
}

# ============================================================
#  3. ROUND ROBIN
# ============================================================
simulate_rr <- function() {
  set.seed(SEED)
  
  job_registry <- new.env(hash = TRUE, parent = emptyenv())
  stats <<- list(
    arrivals = 0L, 
    completions = 0L,
    waiting_times = numeric(0),
    turnaround_times = numeric(0)
  )
  
  env <- simmer("RR")
  
  build_rr_trajectory <- function(env) {
    trajectory("Round Robin") %>%
      set_attribute("job_id", function() {
        stats$arrivals <<- stats$arrivals + 1L
        jid <- stats$arrivals
        svc <- generate_service_time()
        assign(as.character(jid), list(
          arrive_time = now(env),
          service_time = svc,
          remaining = svc
        ), envir = job_registry)
        jid
      }) %>%
      seize("cpu", 1) %>%
      timeout(function() {
        jid <- get_attribute(env, "job_id")
        state <- get(as.character(jid), envir = job_registry)
        noise <- generate_noise()
        slice <- min(QUANTUM, state$remaining) + noise
        state$remaining <- max(0, state$remaining - QUANTUM)
        assign(as.character(jid), state, envir = job_registry)
        slice
      }) %>%
      release("cpu", 1) %>%
      branch(
        function() {
          jid <- get_attribute(env, "job_id")
          state <- get(as.character(jid), envir = job_registry)
          if (state$remaining <= 0) 1L else 2L
        },
        continue = c(FALSE, TRUE),
        trajectory("done") %>% timeout(function() {
            jid <- get_attribute(env, "job_id")
            state <- get(as.character(jid), envir = job_registry)
            t_now <- now(env)
            
            stats$completions <<- stats$completions + 1L
            wt <- t_now - state$arrive_time - state$service_time
            tt <- t_now - state$arrive_time
            
            stats$waiting_times <<- c(stats$waiting_times, max(0, wt))
            stats$turnaround_times <<- c(stats$turnaround_times, tt)
            0
        }),
        trajectory("requeue") %>% rollback(4L)
      )
  }
  
  env %>%
    add_resource("cpu", capacity = 1, queue_size = Inf) %>%
    add_generator("job", build_rr_trajectory(env), function() next_arrival_gap(), mon = 2) %>%
    run(until = SIM_TIME)
  
  return(env)
}

# ============================================================
#  URUCHOMIENIE I ANALIZA
# ============================================================
cat("=== Rozpoczynanie symulacji ===\n")
env_fcfs <- simulate_fcfs()
env_sjf <- simulate_sjf()
env_rr <- simulate_rr()

analyze_results <- function(env, name) {
  arr <- get_mon_arrivals(env, per_resource = TRUE)
  if(nrow(arr) == 0) return(NULL)
  
  arr$system_time <- arr$end_time - arr$start_time
  arr$wait_time <- arr$system_time - arr$activity_time
  
  cat(sprintf("\n--- Metryki wydajności: %s ---\n", name))
  cat(sprintf("Zadania ukończone:         %d\n", nrow(arr)))
  cat(sprintf("Średni czas oczekiwania:   %.4f j.c.\n", mean(arr$wait_time, na.rm=TRUE)))
  cat(sprintf("Średni czas obrotu (TAT):  %.4f j.c.\n", mean(arr$system_time, na.rm=TRUE)))
  cat(sprintf("Przepustowość:             %.4f zad/j.c.\n", nrow(arr) / SIM_TIME))
}

analyze_results(env_fcfs, "FCFS")
analyze_results(env_sjf, "SJF")

# Analiza dla Round Robin z własnego logowania
cat("\n--- Metryki wydajności: Round Robin ---\n")
cat(sprintf("Zadania ukończone:         %d\n", stats$completions))
cat(sprintf("Średni czas oczekiwania:   %.4f j.c.\n", mean(stats$waiting_times, na.rm=TRUE)))
cat(sprintf("Średni czas obrotu (TAT):  %.4f j.c.\n", mean(stats$turnaround_times, na.rm=TRUE)))
cat(sprintf("Przepustowość:             %.4f zad/j.c.\n", stats$completions / SIM_TIME))

