%% @private For each connection a process is started by the HTTP/2 client.
%% For each new stream a 'client_stream' process is started
%% (see grpc_client_stream.erl). The connection process sends messages to the 
%% stream process when data comes in.
-module(grpc_client_connection).

-export([
         new/4,
         stop/1,
         new_stream/1,
         new_stream/2,
         rst_stream/3,
         send_headers/3,
         send_body/4,
         ping/2,
         peercert/1
        ]).

-type connection() :: #{http_connection := pid(),
                        host := binary(),
                        scheme := binary()}.

-type stream_id() :: integer().

-export_type([connection/0,
              stream_id/0]).

-type connection_option() :: grpc_client:connection_option().
-type stream_option() :: grpc_client:stream_option().

-spec new(Transport::tcp|ssl,
          Host::string(),
          Port::integer(),
          Options::[connection_option()]) -> {ok, connection()}.
%% @doc Open a new http/2 connection and check authorisation.
new(Transport, Host, Port, Options) ->
    {GrpcOptions, H2Options} = process_options(Options, Transport),
    H2Client = proplists:get_value(http2_client, GrpcOptions, http2_client),
    ConnectResult = H2Client:new_connection(Transport, Host, Port, H2Options),
    case ConnectResult of
        {ok, Pid} ->
            verify_server(#{http_connection => Pid,
                            host => list_to_binary(Host),
                            scheme => scheme(Transport),
                            client => H2Client},
                          GrpcOptions);
        _ ->
            ConnectResult
    end.

new_stream(Connection) ->
    new_stream(Connection, []).

-spec new_stream(Connection::connection(),
                Options::[stream_option()]) -> {ok, stream_id()}.
new_stream(#{http_connection := Pid,
            client := Client}, Options) ->
    Client:new_stream(Pid, Options).

rst_stream(#{http_connection := Pid, client := Client}, StreamId, ErrorCode) ->
    Client:rst_stream(Pid, StreamId, ErrorCode).

send_headers(#{http_connection := Pid,
              client := Client}, StreamId, Headers) ->
    Client:send_headers(Pid, StreamId, Headers, []).

send_body(#{http_connection := Pid,
            client := Client}, StreamId, Body, Opts) ->
    Client:send_data(Pid, StreamId, Body, Opts).

ping(#{http_connection := Pid,
      client := Client}, Timeout) ->
    Client:ping(Pid, Timeout).

-spec stop(Connection::connection()) -> ok.
%% @doc Stop an http/2 connection.
stop(#{http_connection := Pid,
      client := Client}) ->
    Client:close(Pid).

-spec peercert(Connection::connection()) -> {ok, Cert::binary()} |
                                            {error, Reason::term()}.
%% @doc The peer certificate is returned as a DER-encoded binary.
%% The certificate can be decoded with public_key:pkix_decode_cert/2.
peercert(#{http_connection := Pid,
           client := Client}) ->
    Client:peercert(Pid).

%%% ---------------------------------------------------------------------------
%%% Internal functions
%%% ---------------------------------------------------------------------------

verify_server(#{scheme := <<"http">>} = Connection, _) ->
    {ok, Connection};
verify_server(Connection, Options) ->
    case proplists:get_value(verify_server_identity, Options, false) of
        true ->
            verify_server_identity(Connection, Options);
        false ->
            {ok, Connection}
    end.

verify_server_identity(Connection, Options) ->
    case peercert(Connection) of
        {ok, Certificate} ->
            validate_peercert(Connection, Certificate, Options);
        _ ->
            stop(Connection),
            {error, no_peer_certificate}
    end.

validate_peercert(#{host := Host} = Connection, Certificate, Options) ->
    Server = proplists:get_value(server_host_override,
                                 Options, Host),
    case public_key:pkix_verify_hostname(Certificate,
                                         [{dns_id, Server}]) of
        true ->
            {ok, Connection};
        false ->
            stop(Connection),
            {error, invalid_peer_certificate}
    end.

scheme(ssl) -> <<"https">>;
scheme(tcp) -> <<"http">>.

%% The 'http2_options' must be passed on to the HTTP/2 client.
%%
%% To make sure that the ssl connection meets the gRPC standard, some options are 
%% added if ssl is to be used.
process_options(Opts, Transport) ->
    {TransportOpts0, OtherOpts} =
        grpc_lib:keytake(transport_options, Opts, []),
    TransportOpts =
        case Transport of
            ssl -> 
                MandatorySslOpts = [{verify, verify_peer}, {fail_if_no_peer_cert, true},
                                    {client_preferred_next_protocols, {client, [<<"h2">>]}}],
                {_Ignore, ValidSslOpts} = proplists:split(TransportOpts0,
                                                          [verify, fail_if_no_peer_cert]),
                ValidSslOpts ++ MandatorySslOpts;
            _ ->
                TransportOpts0
        end,
    {H2Opts, GrpcOpts} =  grpc_lib:keytake(http2_options, OtherOpts, []),
    {GrpcOpts, [{transport_options, TransportOpts} | H2Opts]}.
