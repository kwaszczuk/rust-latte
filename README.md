# Latte

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

Buduje cały projekt (z pominięciem generowania parsera). W wyniku powstają pliki `latc`, `latc_x86`

`make x86`

Buduje plik `latc_x86`

`make generate`

Generuje pliki statyczne parsera (`src/parser/src/grammar.rs`). Wywołanie `make` nie wykonuje tej operacji z racji na to, że parser musi zostać wygenerowany na nowo tylko w przypadku zmian w gramatyce, a sam proces jest dosyć czasochłonny.

## Uruchomienie

Kompilacja plików Latte za pomocą `latc` oraz `latc_x75` działa w sposób opisany w treści zadania.

## Zależności

Projekt, prócz bibliotek systemów języka Rust, wykorzystuje pakiet [lalrpop](https://github.com/lalrpop/lalrpop), umożliwiający generacje plików parsera dla danej gramatyki.

Gramatyka języka (`src/parser/src/grammar.lalrpop`) jest przeniesieniem gramatyki BNF z treści zadania na format zdefiniowany przez lalrpop. Gramatyka jest w 100% autorska.

Projekt posiada następujące zależności:
- `codespan`, `codespan-reporting` - wypisywanie błędów w estetycznym formacie
- `ansi_term` - używanie kolorów/formatowania w konsoli
- `lalrpop-util`, regex` - biblioteki wymagane przez wygenerowany plik parsera (`src/parser/src/grammar.rs`), nie są w żadnej sposób używane przez sam kod kompilatora

## Struktura projektu
Całość kodu źródłowego kompilatora znajduje się w folderze `src` i składa się z następujących pakietów:

Kod kompilatora został podzielony na 4 pakiety:
- `base` - kod wspólny
	-  definicja drzewa składniowego - `ast.rs`
	-  implementacja generycznej tablicy symboli - `symbol_table.rs`
	-  definicja ogólnie używanych typów - `types.rs`
- `parser` - kod parsera
	- `grammar.lalrpop` - składnia języka
	- `grammar.rs` - wygenerowany kod parsera języka
- `analyzer` - analiza semantyczna kompilatora
	- `analyzer.rs` - implementacja analizy semantycznej
	- `errors.rs` - definicja błędów dla analizy i pochodnych
	- `types.rs` - definicja typów używanych przy analizie semantycznej

Ponadto w projekcie znajdują się pliki `lib.rs` oraz `main.rs`, które w Ruscie odpowiedzialne są kolejno za implementacje interfejsu danego pakietu oraz samej binarki.

## Poprawność programów

Analiza semantyczna jest zgodna z podanymi w treści zadania testami z jednym wyjątkiem.
W przypadku sprawdzania istnienia returnów w funkcjach, ten jest wymagany zawsze, nawet jeśli kod nie jest osiągalny z racji na tautologie w wyrażeniu warunkowym lub pętli `while`. Zatem funkcja:

```
int f() {
	if (true) {
		return 1;
	} else {}
```

zostanie uznana ze niepoprawną, gdyż brakuje w niej `return` na samym końcu bloku, lub w bloku dla `else`.
