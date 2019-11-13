.PHONY: all
all: llvm jvm insc

build_llvm build_jvm build_insc:
	cargo build --release --bin $(BINARY_NAME) --message-format short --all --exclude generate_parser
	cp target/release/$(BINARY_NAME) .

generate:
	cargo build --release --message-format short --manifest-path src/parser/generate/Cargo.toml

clean:
	rm -rf target/ insc*

.PHONY: llvm
llvm: BINARY_NAME=insc_llvm
llvm: build_llvm

.PHONY: jvm
jvm: BINARY_NAME=insc_jvm
jvm: build_jvm

.PHONY: insc
insc: BINARY_NAME=insc
insc: build_insc
