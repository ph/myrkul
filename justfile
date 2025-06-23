check:
	cargo fmt --check
	cargo clippy

test:
	cargo test

test-local:
	act -W .github/workflows/test-and-build.yml

build:
	cargo build