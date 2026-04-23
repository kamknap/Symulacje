# Załadowanie niezbędnej biblioteki
library(simmer)

# --- ZMIENNE GLOBALNE (Parametry do modyfikacji przez użytkownika) ---
# Te zmienne będą później stanowić panel sterowania dla symulacji[cite: 7, 8].

lambda <- 2       # Intensywność napływu zadań [cite: 9]
mu <- 5           # Średni czas wykonania zadania [cite: 10]
szum_mu <- 0.5    # Średnia opóźnienia z tytułu przerwań systemowych
szum_sigma <- 0.1 # Odchylenie standardowe dla szumu (modelowane rozkładem normalnym) [cite: 11, 12]

# --- GLOBALNA LISTA ZADAŃ (dla algorytmu SJF) ---
# Przechowuje informacje o czasach wykonania poszczególnych zadań
zadania_lista <- list()
licznik_zadan <- 0

# --- 1. INICJALIZACJA ŚRODOWISKA ---
# Tworzymy puste środowisko symulacyjne dla naszego procesora.
env <- simmer("Symulacja_Procesora_SJF")

# --- 2. FUNKCJA DO WYLICZENIA CZASU WYKONANIA ---
# Funkcja ta jest wywoływana dla każdego zadania i generuje jego czas trwania
# Ta wartość jest następnie używana do sortowania w algorytmie SJF
wylicz_czas_wykonania <- function() {
  # Czas trwania zadania (rozkład wykładniczy) [cite: 10, 16]
  czas_wykonania <- rexp(n = 1, rate = 1/mu)
  
  # Implementacja szumu: Przerwania systemowe (rozkład normalny) [cite: 11, 12, 17]
  szum <- max(0, rnorm(n = 1, mean = szum_mu, sd = szum_sigma))
  
  # Całkowity czas spędzony w procesorze
  return(czas_wykonania + szum)
}

# --- 2. SZKIELET TRAJEKTORII (Wejście -> Kolejka SJF -> CPU -> Wyjście) --- [cite: 18]
# Definiujemy ścieżkę, jaką przechodzi każde wygenerowane zadanie
# Algorytm SJF realizujemy poprzez priorytetyzację zadań
trajektoria_procesu <- trajectory("Proces") %>%
  
  # a) GENERACJA CZASU TRWANIA (przed kolejką)
  # Każde zadanie otrzymuje losowy czas wykonania, który będzie używany do sortowania
  set_attribute("czas_trwania", function() {
    wylicz_czas_wykonania()
  }) %>%
  
  # a) WEJŚCIE I KOLEJKA (Zajęcie zasobu z priorytetyzacją SJF)
  # Zadanie ustawia się w kolejce do CPU z priorytetem na podstawie czasu trwania
  # Niższy priorytet = szybsza obsługa w kolejce (SJF)
  seize("CPU", amount = 1,
        priority = function() {
          # Priorytet jest równy czasowi trwania zadania
          # Zadania krótsze otrzymują niższe wartości priorytetu (wyższe priority)
          get_attribute(env, "czas_trwania")
        }) %>%
  
  # b) CPU (Przetwarzanie)
  # Zadanie jest przetwarzane przez czas równy jego wygenerowanemu czasowi trwania
  timeout(function() {
    get_attribute(env, "czas_trwania")
  }) %>%
  
  # c) WYJŚCIE (Zwolnienie zasobu)
  # Zadanie kończy się i zwalnia procesor dla kolejnego procesu w kolejce
  release("CPU", amount = 1)

# --- 3. GENERATORY ZDARZEŃ I REJESTRACJA ZASOBÓW ---
# Łączymy wszystko w jedną całość: dodajemy procesor i ustalamy, jak często pojawiają się zadania.
env %>%
  # Dodanie procesora (jeden rdzeń = capacity 1, nieskończona kolejka = Inf)
  # Algorytm SJF jest realizowany poprzez priorytetyzację zasobów
  add_resource("CPU", capacity = 1, queue_size = Inf) %>%
  
  # Dodanie generatora napływających zadań
  add_generator(name_prefix = "Zadanie_", 
                trajectory = trajektoria_procesu, 
                
                # Funkcja określająca odstępy czasu między zadaniami
                distribution = function() {
                  
                  # Zgodnie z konspektem używamy rozkładu Poissona do odstępów [cite: 9, 15]
                  odstep_czasu <- rpois(n = 1, lambda = lambda)
                  
                  # Małe zabezpieczenie: jeśli wylosuje się 0, zadania pojawią się w tej samej milisekundzie.
                  return(odstep_czasu)
                })

# --- TESTOWE URUCHOMIENIE ---
# Symulujemy pracę przez 1000 jednostek czasu dla algorytmu SJF
env %>% run(until = 1000)

# --- ANALIZA WYNIKÓW ALGORYTMU SJF ---
# Pobieramy wyniki monitorowania zasobu CPU
cpu_results <- get_mon_resources(env)

# Podejrzenie statusu środowiska po zakończeniu symulacji
print(env)

# Wyświetlenie statystyk dotyczących zasobu CPU
print("=== STATYSTYKI ALGORYTMU SJF (Shortest Job First) ===")
print(head(cpu_results, 20))

# Szczegółowe informacje o każdym zadaniu (ścieżka przez system)
arrivals <- get_mon_arrivals(env, per_resource = TRUE)
print("=== SZCZEGÓŁY ZADAŃ W SYSTEMIE ===")
print(head(arrivals, 20))

# Obliczenie podstawowych metryk wydajności
if (nrow(arrivals) > 0) {
  # Czas spędzony w systemie = end_time - start_time
  arrivals$system_time <- arrivals$end_time - arrivals$start_time
  
  # Czas oczekiwania = activity_time - czas_wykonania
  # activity_time zawiera zarówno oczekiwanie jak i przetwarzanie
  # Przybliżenie: czas czekania = system_time - średnia_activity_time
  arrivals$wait_time <- arrivals$system_time - arrivals$activity_time
  
  # Liczba obsłużonych zadań
  liczba_zadan <- nrow(arrivals)
  
  # Średni czas spędzony w systemie
  sredni_czas_w_systemie <- mean(arrivals$system_time, na.rm = TRUE)
  
  # Średni czas oczekiwania w kolejce
  sredni_czas_czekania <- mean(arrivals$wait_time, na.rm = TRUE)
  
  print(sprintf("=== METRYKI WYDAJNOŚCI ALGORYTMU SJF ==="))
  print(sprintf("Liczba obsłużonych zadań: %d", liczba_zadan))
  print(sprintf("Średni czas w systemie: %.4f j.c.", sredni_czas_w_systemie))
  print(sprintf("Średni czas oczekiwania w kolejce: %.4f j.c.", sredni_czas_czekania))
  print(sprintf("Średni czas przetwarzania: %.4f j.c.", mean(arrivals$activity_time, na.rm = TRUE)))
}