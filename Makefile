.PHONY: all
all: latc llvm

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

build_x86 build_latc build_llvm:
	clang -S -emit-llvm lib/runtime.c -o lib/runtime.ll
	llvm-as -o lib/runtime.bc lib/runtime.ll
	cargo build $(CFLAGS) --bin $(BINARY_NAME) --all --exclude generate_parser
	cp target/release/$(BINARY_NAME) .

parser:
	cargo build --release --message-format short --manifest-path src/parser/generate/Cargo.toml

clean:
	rm -rf target/ latc* lib/*.bc lib/*.ll

.PHONY: x86
x86: BINARY_NAME=latc_x86
x86: build_x86

.PHONY: llvm
llvm: BINARY_NAME=latc_llvm
llvm: build_llvm

.PHONY: latc
latc: BINARY_NAME=latc
latc: build_latc
