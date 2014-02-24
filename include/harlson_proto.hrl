
-type appkey()   :: nonempty_string().
-type endpoint() :: atom().
-type level()    :: atom().

-record(q_metric, {
        key      :: {appkey(), endpoint()},
        count    :: non_neg_integer()
        }).

-record(q_level, {
        key      :: appkey(),
        level    :: level()
        }).

-record(q_limit, {
        level    :: level(),
        endpoint :: endpoint(),
        limit    :: non_neg_integer()
        }).

-record(r_overlimit, {
        key      :: {appkey(), endpoint()},
        change   :: overlimit_change()
        }).

-record(overlimit_add, {
        value    :: non_neg_integer(),
        throttle :: float()
        }).

-type overlimit_change() :: overlimit_removed | #overlimit_add{}.

-type rls_query() :: {update_metrics, [#q_metric{}]} 
                   | {update_levels,  [#q_level{}]}
                   | {update_limits,  [#q_limit{}]}
                   | get_over_limit
                   | stop.

-type rls_reply() :: {overlimit, [#r_overlimit{}]}.

