# grpc_client

An implementation of a gRPC client in Erlang. 

An implementation of the server side is alo available: [grpc](https://github.com/Bluehouse-Technology/grpc).

## Build
grpc_client uses [erlang.mk](https://erlang.mk/) as build tool. On Unix systems it can be built
with: 

```
make
```

`make edoc` can be used to generate documentation for the individual
modules from edoc comments in those modules.

See the [erlang.mk documentation](https://erlang.mk/guide/installation.html#_on_windows) for
an explanation on how the tool can be used in a Windows environment.

## Testing
There are no separate tests provided for grpc_client, but the tests for
[grpc](https://github.com/Bluehouse-Technology/grpc) (the server side) use
and test also th client side.

## Dependencies

- [http2_client](https://github.com/Bluehouse-Technology/http2_client) is
  the default HTTP/2 client. Other HTTP/2 clients can also be used, an
  adapter for the [Chatterbox](https://github.com/willemdj/chatterbox) is
  provided (note that this needs a fork of Chatterbox with some additional
  features).

- [grpc_lib](https://github.com/Bluehouse-Technology/grpc_lib) supplies some
  functions that are shared between the client and the server.

## gRPC functionality

The [tutorial](https://github.com/Bluehouse-Technology/grpc/blob/master/doc/tutorial.md)
that is provided with the server covers also the client.

## Acknowledgements

The development of this application was championed by [Holger Winkelmann](https://github.com/hwinkel) of [Travelping](https://github.com/travelping). [Travelping](https://github.com/travelping) also kindly provided sponsorship for the initial development. The development was done by [Willem de Jong](https://github.com/willemdj).

## License

Apache 2.0

