PROJECT = grpc_client
PROJECT_DESCRIPTION = Erlang gRPC client
PROJECT_VERSION = 0.1.0

# Whitespace to be used when creating files from templates.
SP = 4

DEPS = grpc_lib http2_client
dep_grpc_lib = git https://github.com/Bluehouse-Technology/grpc_lib
dep_http2_client = git https://github.com/Bluehouse-Technology/http2_client

include erlang.mk
