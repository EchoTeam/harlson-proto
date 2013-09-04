
-record(q_metric, {
        key      :: nonempty_string(),
        endpoint :: rls_client:endpoint(),
        level    :: rls_client:level(),
        count    :: non_neg_integer()
        }).

-record(q_limit, {
        level    :: rls_client:level(),
        endpoint :: rls_client:endpoint(),
        limit    :: non_neg_integer()
        }).

-record(r_overlimit, {
        key      :: nonempty_string(),
        endpoint :: rls_client:endpoint(),
        change   :: overlimit_change()
        }).

-record(overlimit_add, {
        value    :: non_neg_integer(),
        throttle :: float()
        }).

-type overlimit_change() :: overlimit_removed | #overlimit_add{}.

-type rls_query() :: {update_metrics, [#q_metric{}]} 
                   | {update_limits,  [#q_limit{}]}
                   | get_over_limit
                   | stop.

-type rls_reply() :: {overlimit, [#r_overlimit{}]}.

