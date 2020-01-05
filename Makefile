.PHONY: all
all: llvm x86

CFLAGS=--release
ifeq ($(SILENT),1)
	CFLAGS += --message-format short
endif

ifeq ($(ANALYSIS),0)
  	CFLAGS += --features "no-analysis"
endif

ifeq ($(CODEGEN),0)
  	CFLAGS += --features "no-codegen"
endif

ifeq ($(MEM2REG),0)
  	CFLAGS += --features "llvm/no-mem2reg"
endif

ifeq ($(OPTIMIZE),0)
  	CFLAGS += --features "llvm/no-optimizations"
endif

.PHONY: llvm
llvm: BINARY_NAME=latc_llvm
llvm:
	clang -S -emit-llvm lib/runtime.c -o lib/runtime.ll
	llvm-as -o lib/runtime.bc lib/runtime.ll
	cargo build $(CFLAGS) --features "emit-llvm"
	cp target/release/$(BINARY_NAME) .
	cp target/release/latc .

.PHONY: x86
x86: BINARY_NAME=latc_x86_64
x86:
	clang -c -O3 lib/runtime.c -o lib/runtime.o
	cargo build $(CFLAGS) --features "emit-x86_64"
	cp target/release/$(BINARY_NAME) .
	cp target/release/latc .

parser:
	cargo build --release --message-format short --manifest-path src/parser/generate/Cargo.toml

clean:
	rm -rf target/ latc* lib/*.bc lib/*.ll
