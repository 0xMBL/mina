-- TODO: Automatically push, tag, and update images #4862
-- NOTE: minaToolchainStretch is also used for building Ubuntu Bionic packages in CI
-- NOTE: minaToolchainBullseye is also used for building Ubuntu Focal packages in CI
{
  toolchainBase = "codaprotocol/ci-toolchain-base:v3",
  minaToolchainBullseye = "gcr.io/o1labs-192920/mina-toolchain@sha256:dac82734cbe5b6ed9c9b63fd5951f5417cbd8469b5bf4f0eedd21ff8feab2b11",
  minaToolchainBuster = "gcr.io/o1labs-192920/mina-toolchain@sha256:bf08dbe1d9320863d4d774a63c9356cc6788ed1b7011846035ecddafa582442c",
  minaToolchainStretch = "gcr.io/o1labs-192920/mina-toolchain@sha256:f672b8649d4cd6b89e61a0b823e3e268555b0aca8e685c97877e216ae7826579",
  delegationBackendToolchain = "gcr.io/o1labs-192920/delegation-backend-production@sha256:8ca5880845514ef56a36bf766a0f9de96e6200d61b51f80d9f684a0ec9c031f4",
  elixirToolchain = "elixir:1.10-alpine",
  rustToolchain = "codaprotocol/coda:toolchain-rust-e855336d087a679f76f2dd2bbdc3fdfea9303be3",
  nodeToolchain = "node:14.13.1-stretch-slim",
  ubuntu1804 = "ubuntu:18.04",
  xrefcheck = "serokell/xrefcheck@sha256:8fbb35a909abc353364f1bd3148614a1160ef3c111c0c4ae84e58fdf16019eeb"
}
