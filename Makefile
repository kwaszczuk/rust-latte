.PHONY: all
all: latc llvm

build_x86 build_latc build_llvm:
	cargo build --release --bin $(BINARY_NAME) --all --exclude generate_parser
	# cargo build --release --bin $(BINARY_NAME) --message-format short --all --exclude generate_parser
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
