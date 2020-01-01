.PHONY: all
all: latc llvm

CFLAGS=--release
ifeq ($(SILENT),1)
	CFLAGS += --message-format short
endif

ifeq ($(PARSER_ONLY),1)
  	CFLAGS += --features "no-analysis"
endif

ifeq ($(ANALYSIS_ONLY),1)
  	CFLAGS += --features "no-codegen"
endif

build_x86 build_latc build_llvm:
	cargo build $(CFLAGS) --bin $(BINARY_NAME) --all --exclude generate_parser
	cp target/release/$(BINARY_NAME) .

parser:
	cargo build --release --message-format short --manifest-path src/parser/generate/Cargo.toml

clean:
	rm -rf target/ latc*

.PHONY: x86
x86: BINARY_NAME=latc_x86
x86: build_x86

.PHONY: llvm
llvm: BINARY_NAME=latc_llvm
llvm: build_llvm

.PHONY: latc
latc: BINARY_NAME=latc
latc: build_latc
