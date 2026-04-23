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
SIM_TIME      <- 2000  # Całkowity czas symulacji [jedn. czasu]
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

# Ekstrakcja spójnych danych do jednego df dla wszystkich algorytmów
extract_waiting_times <- function(env, name) {
  arr <- get_mon_arrivals(env, per_resource = TRUE)
  if(nrow(arr) == 0) return(data.frame(Algorytm = character(), wait_time = numeric()))
  wait_time <- arr$end_time - arr$start_time - arr$activity_time
  return(data.frame(Algorytm = name, wait_time = wait_time))
}

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

# ============================================================
#  WIZUALIZACJE (Porównanie algorytmów)
# ============================================================

cat("\n=== Generowanie wykresów ===\n")

# Zestawienie czasów oczekiwania dla boxplota
df_wait_fcfs <- extract_waiting_times(env_fcfs, "FCFS")
df_wait_sjf <- extract_waiting_times(env_sjf, "SJF")
df_wait_rr <- data.frame(Algorytm = "Round Robin", wait_time = stats$waiting_times)

df_wait <- bind_rows(df_wait_fcfs, df_wait_sjf, df_wait_rr)

# Paleta kolorów
colors <- c("FCFS" = "#F8766D", "SJF" = "#00BA38", "Round Robin" = "#619CFF")

# Wykres 1: Skrzypcowy (Boxplot) dla czasów oczekiwania
p1 <- ggplot(df_wait, aes(x = Algorytm, y = wait_time, fill = Algorytm)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 1) +
  scale_fill_manual(values = colors) +
  labs(
    title = "Porównanie rozkładu czasów oczekiwania (Wait Time)",
    subtitle = "Pudło pokazuje medianę i kwartyle, kropki to wartości odstające",
    x = "Algorytm planowania",
    y = "Czas oczekiwania [j.c.]"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Przygotowanie danych kolejek z monitora zasobów (z wyjątkiem RR - potrzebne są z resources)
res_fcfs <- get_mon_resources(env_fcfs) %>% mutate(Algorytm = "FCFS")
res_sjf <- get_mon_resources(env_sjf)  %>% mutate(Algorytm = "SJF")
res_rr  <- get_mon_resources(env_rr)   %>% mutate(Algorytm = "Round Robin")

df_queue <- bind_rows(res_fcfs, res_sjf, res_rr)

# Wykres 2: Długość kolejki procesora w czasie
p2 <- ggplot(df_queue, aes(x = time, y = queue, color = Algorytm)) +
  geom_step(alpha = 0.6, linewidth = 0.8) +
  scale_color_manual(values = colors) +
  facet_wrap(~ Algorytm, nrow = 3) +
  labs(
    title = "Wykres długości kolejki CPU w czasie symulacji",
    x = "Czas symulacji [j.c.]",
    y = "Długość kolejki (oczekujące zadania)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Wykres 3: Utylizacja CPU w czasie (okno kroczące)
window_size <- 50  # szerokość okna w jednostkach czasu (przy 2000 SIM_TIME to da płynny wykres)

calculate_utilization <- function(res_df) {
  res_df %>%
    filter(resource == "cpu") %>%
    mutate(
      window = floor(time / window_size) * window_size,
      dt = lead(time, default = SIM_TIME) - time,
      busy = server > 0
    ) %>%
    group_by(Algorytm, window) %>%
    summarise(
      utilization = sum(dt[busy]) / window_size,
      .groups = "drop"
    )
}

df_util <- bind_rows(
  calculate_utilization(res_fcfs),
  calculate_utilization(res_sjf),
  calculate_utilization(res_rr)
)

p3 <- ggplot(df_util, aes(x = window, y = pmin(utilization, 1), fill = Algorytm, color = Algorytm)) +
  geom_area(alpha = 0.4) +
  geom_line(linewidth = 0.8) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.05)) +
  facet_wrap(~ Algorytm, nrow = 3) +
  labs(
    title = "Porównanie wykorzystania procesora (CPU Utilization)",
    subtitle = sprintf("Wyliczane metodą okna kroczącego (szerokość = %d j.c.)", window_size),
    x = "Czas symulacji [j.c.]",
    y = "Wykorzystanie CPU"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Wyświetlanie / zapisywanie
ggsave("porownanie_czasu_oczekiwania.png", plot = p1, width = 8, height = 5, dpi = 150)
ggsave("porownanie_kolejek.png", plot = p2, width = 8, height = 8, dpi = 150)
ggsave("porownanie_obciazenia.png", plot = p3, width = 8, height = 8, dpi = 150)

cat("Wykresy zostały zapisane jako:\n - porownanie_czasu_oczekiwania.png\n - porownanie_kolejek.png\n - porownanie_obciazenia.png\n")


