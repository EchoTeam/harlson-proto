-module(harlson_proto).
-export([encode/1, decode/1]).

-include("rls_client.hrl").

-compile({inline, [enc/1, dec/1]}).
-compile(inline).
-compile({inline_size, 200}).

-define(_int, big-unsigned-integer-unit:32).
-define(_short, big-unsigned-integer-unit:8).
-define(int, 1/?_int).
-define(short, 1/?_short).

%% ENCODER
-spec encode(rls_query()) -> binary().
encode(Query) ->
    erlang:iolist_to_binary(enc(Query)).

% Query
enc({update_metrics, QMetrics}) ->
    [<<"UPME">>, [enc_metric(M) || #q_metric{} = M <- QMetrics]];
enc({update_limits, QLimits}) ->
    [<<"UPLI">>, [enc_limit(L) || #q_limit{} = L <- QLimits]];
enc(get_over_limit) ->
    [<<"GOVL">>];
enc(stop) ->
    [<<"STOP">>].

% QMetric
enc_metric(#q_metric{key = Key, endpoint = Endpoint,
                     level = Level, count = Count}) ->
    [enc_bs(Key), enc_bs(atom_to_list(Endpoint)), enc_bs(atom_to_list(Level)),
     <<Count:?int>>].

enc_limit(#q_limit{level = Level, endpoint = Endpoint,
                   limit = Limit}) ->
    [enc_bs(atom_to_list(Level)), enc_bs(atom_to_list(Endpoint)),
     <<Limit:?int>>].

-spec enc_bs(nonempty_string()) -> iodata().
enc_bs(String) ->
    Length = length(String),
    [<<Length:?short>>, String].


%% DECODER
-spec decode(binary()) -> rls_reply().
decode(Binary) ->
    dec(Binary).

dec(<<"ROVL", Count:?int, Tail/binary>>) ->
    {OverLimit, _} = read_times(Count, Tail, fun dec_r_overlimit/1),
    {overlimit, OverLimit}.

dec_r_overlimit(<<KeySize:?short, Key:KeySize/binary, 
                  EpSize:?short, Endpoint:EpSize/binary,
                  OverLimitChange/binary>>) ->
    {OvChange, RestBin} = dec_overlimit_change(OverLimitChange),
    {#r_overlimit{key      = binary_to_list(Key), 
                  endpoint = list_to_atom(binary_to_list(Endpoint)), 
                  change   = OvChange}, RestBin}.

dec_overlimit_change(<<0:?short, Value:?int, Throttle:?int, Rest/binary>>) ->
    {#overlimit_add{value    = Value,
                    throttle = Throttle * 1.0 / 1000}, Rest};
dec_overlimit_change(<<1:?short, Rest/binary>>) ->
    {overlimit_removed, Rest}.
        

-spec read_times(non_neg_integer(), binary(), fun((binary()) -> {X, binary()})) 
        -> {[X], binary()}.
read_times(0, Binary, _Fun) ->
    {[], Binary};
read_times(N, Binary, Fun) when N > 0 ->
    {Ret, RestBinary} = Fun(Binary),
    {RestRet, RestRestBinary} = read_times(N - 1, RestBinary, Fun),
    {[Ret | RestRet], RestRestBinary}.


