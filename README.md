# Latte

Kompilator umożliwia generacje kodu LLVM jak i x86 w wersji 64-bitowej.

## Instalacja Rust

Kompilator został napisany w języku Rust, więc aby go uruchomić na maszynie musi zostać zainstalowany ten język. Do poprawnego działania projektu wystarczy, aby na maszynie działały narzędzia `cargo` oraz `rustc`.

W przypadku braku Rusta na maszynie, najłatwiej go zainstalować używając komendy:

`curl https://sh.rustup.rs -sSf | sh`

A następnie wykonując przelogowanie w celu poprawnego załadowania środowiska Rust. Niecierpliwi, zamiast tego mogą skorzystać z komendy:

`source $HOME/.cargo/env`

Bo wykonaniu powyższych kroków Rust powinien zostać zainstalowany na maszynie, wraz z jego podstawowymi narzędziami
- rustup - instalator
- rustc - kompilator
- cargo - manager pakietów

W celu odinstalowania Rusta z maszyny należy skorzystać z komendy:

`rustup self uninstall`

## Kompilacja

`make`

Buduje cały projekt (z pominięciem generowania parsera). W wyniku powstają pliki `latc`, `latc_x86_64` oraz `latc_llvm`.

`make x86`

Buduje plik `latc_x86_64` oraz `latc`

`make llvm`

Buduje plik `latc_llvm` oraz `latc`

`make generate`

Generuje pliki statyczne parsera (`src/parser/src/grammar.rs`). Wywołanie `make` nie wykonuje tej operacji z racji na to, że parser musi zostać wygenerowany na nowo tylko w przypadku zmian w gramatyce, a sam proces jest dosyć czasochłonny.

Dodatkowo make posiada kilka konfigurowalnych flag:

- `SILENT` - jeśli ustawione na `1` to kompilacja jest wykonywana w trybie "cichym" (domyślnie: `1`)
- `ANALYSIS` - jeśli ustawione na `0` to analiza semantyczna jest pomijana (domyślnie: `1`)
- `CODEGEN` - jeśli ustawione na `0` to generowanie kodu jest pomijane (domyślnie: `1`)
- `MEM2REG` - jeśli ustawione na `0` to generowany kod nie jest w postaci SSA (domyślnie: `1`)
- `OPTIMIZE` - jeśli ustawione na `0` to kod generowany jest bez optymalizacji (domyślnie: `1`)
- `REGALLOC` - jeśli ustawione na `0` to kod x86 generowany jest bez alokacji rejestrów (domyślnie: `1`)

**UWAGA:** kompilacja x86 aktualnie nie jest możliwa przy wyłączonym `MEM2REG` , a kompilacja z `REGALLOC` nie będzie działała jeśli wyłączymy optymalizacji, gdyż wyłącza to `MEM2REG`.

## Uruchomienie

Kompilacja plików Latte za pomocą `latc`, `latc_llvm`, latc_x86_64` działa w sposób opisany w treści zadania.

## Zależności

Projekt, prócz bibliotek systemów języka Rust, wykorzystuje pakiet [lalrpop](https://github.com/lalrpop/lalrpop), umożliwiający generacje plików parsera dla danej gramatyki.

Gramatyka języka (`src/parser/src/grammar.lalrpop`) jest przeniesieniem gramatyki BNF z treści zadania na format zdefiniowany przez lalrpop. Gramatyka jest w 100% autorska.

Projekt posiada następujące zależności:
- `codespan`, `codespan-reporting` - wypisywanie błędów w estetycznym formacie
- `ansi_term` - używanie kolorów/formatowania w konsoli
- `lalrpop-util`, regex` - biblioteki wymagane przez wygenerowany plik parsera (`src/parser/src/grammar.rs`), nie są w żadnej sposób używane przez sam kod kompilatora

Ponadto, do poprawnego działania kompilator wymaga narzędzia [clang](https://clang.llvm.org/) używanego do kompilacji funkcji bibliotecznych oraz linkowania jej z wygenerowanym kodem.

## Optymalizacje

Kompilator posiada zaimplementowane następujące optymalizacje:
- generowanie kodu LLVM w postaci SSA (mem2reg)
- optymalizacje stałych (constants folding & propagationa - bez typu `string`)
- redukcja ścieżek prostych w CFG
- popragacja skoków w blokach prostych złożonych jedynie z instrukcji skoku (zamiast skakać do takiego bloku, kod skacze do bloku, który on wskazuje)
- eliminacja martwego kodu (unreachable code & unused assignments elimination)
- eliminacja trywialnych funkcji phi
- eliminacja wspólnych podwyrażeń

## Rozszerzenia

Kompilator wspiera tablice wielowymiarowe z pętlą _for-each_ w postaci:

    for (int x: t) {
      ...
    }

gdzie `x` jest **referencją** na kolejne elementy tablicy `t`. Dzięki temu, możliwa jest zmiana wartości tablicy w pęli, bez używania indeksowania, co myślę że jest bardzo przydatne.
Dla typów prostych (nietablicowych) zmienna `x` jest również **copy-on-write**.

## Alokacja rejestrów

Kompilator posiada zaimplementowaną alokacje rejestrów dla binarki x86-64. Wykorzystuje tu wszystkie rejestry poza `RAX`, `RBP`, `RSP`, `RCX`, `RDX`. Pierwsze 3 rejestry są pomijane z oczywistych względów. `RCX` oraz `RDX` nie alokuje, gdyż wykorzystywane są przy dzieleniu oraz czasem do offloadowania wartości przy operacjach tablicowych.

Alokacja `RCX`, `RDX` prawdopodobnie byłaby możliwa przy przemyślanym odkładaniu ich wartości na stos (biorąc poprawkę na rejestr w którym powinien znaleźć się końcowy wynik).

Mimo to alokacja działa dobrze. We wszystkich publicznych testach alokacja nie spilluje żadnych zmiennych tj. całość wykonania programów odbywa się tylko na rejestrach.

## Struktura projektu
Całość kodu źródłowego kompilatora znajduje się w folderze `src` i składa się z następujących pakietów:

- `cli/src/main.rs` - entrypoint kompilatora
- `base` - kod wspólny
	- `ast.rs` - definicja drzewa składniowego
	- `symbol_table.rs` - implementacja generycznej tablicy symboli
	- `types.rs` - definicja ogólnie używanych typów
- `parser` - kod parsera
	- `grammar.lalrpop` - składnia języka
	- `grammar.rs` - wygenerowany kod parsera języka
- `analyzer` - analiza semantyczna kompilatora
	- `analyzer.rs` - implementacja analizy semantycznej
	- `errors.rs` - definicja błędów dla analizy i pochodnych
	- `types.rs` - definicja typów używanych przy analizie semantycznej
	- `evaluation.rs` - kod odpowiedzialny za upraszczanie stałych w drzewie AST
- `llvm` - kompilacja do LLVM
	- `compiler.rs` - translacja z AST do kodu pośrednio bazowanego na LLVM
	- `instructions.rs` - defincja kodu pośredniego, typów itp.
	- `operators.rs` - definicja operatorów
	- `control_flow_graph.rs` - obliczanie grafu przepływu sterowania
    - `mem2reg` - konwersja kodu do postaci SSA
                - `dominance.rs` - obliczanie dominance frontiers i drzewa dominatorów
                - `transform.rs` - właściwy kod konwersji do SSA
    - `optimizations` - zawiera wszelakie zaimplementowane optymalizacje
- `x86` - kompilacja do x86
	- `compiler.rs` - translacja z LLVM do x86
	- `instructions.rs` - defincja kodu pośredniego, typów itp.
	- `operators.rs` - definicja operatorów
        - `register_allocation` - kod alokacji rejestrów
                - `liveness.rs` - analiza żywotności zmiennych
                - `interference_graph.rs` - obliczanie grafu interferencji zmiennych
                - `colouring.rs` - kolorowanie grafu interferencji

W projekcie znajdują się pliki `lib.rs` oraz `main.rs`, które w Ruscie odpowiedzialne są kolejno za implementacje interfejsu danego pakietu oraz samej binarki.
Ponadto w folderze `lib` można znaleźć plik `runtime.c`, zawierający implementacje funkcji bibliotecznych.
