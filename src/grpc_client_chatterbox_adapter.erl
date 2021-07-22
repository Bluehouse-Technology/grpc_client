%%%-------------------------------------------------------------------
%%% Licensed to the Apache Software Foundation (ASF) under one
%%% or more contributor license agreements.  See the NOTICE file
%%% distributed with this work for additional information
%%% regarding copyright ownership.  The ASF licenses this file
%%% to you under the Apache License, Version 2.0 (the
%%% "License"); you may not use this file except in compliance
%%% with the License.  You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%

%% @doc Adapter for Chatterbox, so that the Chatterbox HTTP/2 client can
%% be used for the HTTP/2 client functionality required for gRPC.
%%
%% Note that this assumes that a fork of Chatterbox is used:
%% https://github.com/willemdj/chatterbox
-module(grpc_client_chatterbox_adapter).

-export([
         new_connection/4,
         new_stream/2,
         send_headers/4,
         send_data/4,
         ping/2,
         close/1,
         rst_stream/3,
         peercert/1
        ]).


-type connection() :: term().
-type stream_id() :: term().
-type header() :: [{Name::binary(), Value::binary()}].
-type connection_option() :: {transport_options, [ssl:ssl_option()]}.
-type send_option() :: {end_stream, boolean()}.

-spec new_connection(Transport::ssl|tcp,
                     Host::string(),
                     Port::integer(),
                     Options::[connection_option()]) -> {ok, pid()} |
                                                        {error, term()}.
new_connection(Transport, Host, Port, Options) ->
    process_flag(trap_exit, true),
    Scheme = case Transport of
                 tcp -> http;
                 ssl -> https
             end,
    TransportOptions = proplists:get_value(transport_options, Options, []),
    try
        h2_client:start_link(Scheme, Host, Port, TransportOptions)
    catch
        _Type:Reason ->
            {error, Reason}
    end.


-spec new_stream(Connection::term(),
                 Options::[term()]) -> {ok, integer()} |
                                       {error, term()}.
new_stream(Connection, _) ->
    R = h2_connection:new_stream(Connection),
    {ok, R}.

-spec send_headers(connection(), stream_id(), [header()],
                   Options::[send_option()]) -> ok | {error, term()}.
%% @doc Send headers.
send_headers(Pid, StreamId, Headers, Options) ->
    h2_connection:send_headers(Pid, StreamId, Headers, send_options(Options)).

-spec send_data(connection(), stream_id(), Data::binary(),
                Options::[send_option()]) -> ok | {error, term()}.
%% @doc Send a data frame.
send_data(Pid, StreamId, Data, Options) ->
    h2_connection:send_body(Pid, StreamId, Data, send_options(Options)).


-spec rst_stream(connection(), stream_id(), ErrorCode::integer()) ->
  ok | {error, term()}.
%% @doc Send a RST_STREAM frame
rst_stream(_Pid, _StreamId, _ErrorCode) ->
    %% Not supported
    ok.

-spec ping(connection(), timeout()) ->
    {ok, RoundTripTime::integer()} | {error, term()}.
%% @doc Send a PING frame.
%% TODO: take care of the return value.
ping(Pid, _Timeout) ->
    h2_connection:send_ping(Pid),
    {ok, 0}.

-spec close(connection()) -> ok.
%% @doc Close the connection.
close(Pid) ->
    h2_connection:stop(Pid).

-spec peercert(pid()) -> {ok, Cert::binary()} | {error, Reason::term()}.
%% @doc The peer certificate is returned as a DER-encoded binary.
%% The certificate can be decoded with public_key:pkix_decode_cert/2.
peercert(Pid) ->
    h2_connection:get_peercert(Pid).

%%% ---------------------------------------------------------------------------
%%% Internal functions
%%% ---------------------------------------------------------------------------

send_options(Options) ->
    case lists:keyfind(end_stream, 1, Options) of
        false -> [{send_end_stream, false}];
        {_, false} -> [{send_end_stream, false}];
        {_, true} ->  [{send_end_stream, true}]
    end.
