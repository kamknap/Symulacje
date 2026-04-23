# Załadowanie niezbędnej biblioteki
library(simmer)

# --- ZMIENNE GLOBALNE (Parametry do modyfikacji przez użytkownika) ---
# Te zmienne będą później stanowić panel sterowania dla symulacji[cite: 7, 8].

lambda <- 2       # Intensywność napływu zadań [cite: 9]
mu <- 5           # Średni czas wykonania zadania [cite: 10]
szum_mu <- 0.5    # Średnia opóźnienia z tytułu przerwań systemowych
szum_sigma <- 0.1 # Odchylenie standardowe dla szumu (modelowane rozkładem normalnym) [cite: 11, 12]

# --- 1. INICJALIZACJA ŚRODOWISKA ---
# Tworzymy puste środowisko symulacyjne dla naszego procesora.
env <- simmer("Symulacja_Procesora")

# --- 2. SZKIELET TRAJEKTORII (Wejście -> Kolejka -> CPU -> Wyjście) --- [cite: 18]
# Definiujemy ścieżkę, jaką przechodzi każde wygenerowane zadanie.
trajektoria_procesu <- trajectory("Proces") %>%
  
  # a) WEJŚCIE I KOLEJKA (Zajęcie zasobu)
  # Zadanie ustawia się w kolejce do CPU. Domyślnie simmer używa tu kolejki FCFS.
  seize("CPU", amount = 1) %>%
  
  # b) CPU (Przetwarzanie z uwzględnieniem zapotrzebowania na moc i szumu)
  timeout(function() {
    
    # Parametry procesów: Czas trwania zadania (rozkład wykładniczy) [cite: 10, 16]
    # Uwaga: w funkcji rexp() używamy 'rate = 1/mu', gdzie mu to średni czas.
    czas_wykonania <- rexp(n = 1, rate = 1/mu) 
    
    # Implementacja szumu: Przerwania systemowe (rozkład normalny) [cite: 11, 12, 17]
    # Używamy funkcji max(0, ...), aby zapobiec wygenerowaniu ujemnego czasu opóźnienia.
    szum <- max(0, rnorm(n = 1, mean = szum_mu, sd = szum_sigma))
    
    # Całkowity czas spędzony w procesorze
    return(czas_wykonania + szum)
  }) %>%
  
  # c) WYJŚCIE (Zwolnienie zasobu)
  # Zadanie kończy się i zwalnia procesor dla kolejnego procesu w kolejce.
  release("CPU", amount = 1)

# --- 3. GENERATORY ZDARZEŃ I REJESTRACJA ZASOBÓW ---
# Łączymy wszystko w jedną całość: dodajemy procesor i ustalamy, jak często pojawiają się zadania.
env %>%
  # Dodanie procesora (jeden rdzeń = capacity 1, nieskończona kolejka = Inf)
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
# Symulujemy pracę przez 100 jednostek czasu, aby sprawdzić, czy nie ma błędów.
env %>% run(until = 100)

# Podejrzenie statusu środowiska po zakończeniu testu
print(env)