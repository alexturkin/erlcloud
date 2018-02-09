-module(erlcloud_budgets_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlcloud_aws.hrl").

%% API
-export([]).

-define(EHTTPC, erlcloud_httpc).

-define(ACC_ID, <<"123456789123">>).
-define(BUDGET_NAME, <<"budget-name">>).
-define(NEXT_TOKEN, <<"next_token">>).

-define(BUDGET_INPUT,
    #{<<"BudgetLimit">> =>
          #{<<"Amount">> => <<"1000">>,<<"Unit">> => <<"USD">>},
      <<"BudgetName">> => <<"test">>,
      <<"BudgetType">> => <<"COST">>,
      <<"CalculatedSpend">> =>
          #{<<"ActualSpend">> =>
                #{<<"Amount">> => <<"0">>,<<"Unit">> => <<"USD">>}},
      <<"CostFilters">> => #{<<"Region">> => [<<"us-east-2">>]},
      <<"CostTypes">> =>
          #{<<"IncludeCredit">> => true,<<"IncludeDiscount">> => true,
            <<"IncludeOtherSubscription">> => true,
            <<"IncludeRecurring">> => true,<<"IncludeRefund">> => true,
            <<"IncludeSubscription">> => true,
            <<"IncludeSupport">> => true,<<"IncludeTax">> => true,
            <<"IncludeUpfront">> => true,<<"UseAmortized">> => false,
            <<"UseBlended">> => false},
      <<"TimePeriod">> =>
          #{<<"End">> => 3.7064736e9,<<"Start">> => 1.5174432e9},
      <<"TimeUnit">> => <<"MONTHLY">>}
).


-define(GET_BUDGET,  #{<<"Budget">>  => ?BUDGET_INPUT}).
-define(GET_BUDGETS_1_PAGE, #{<<"Budgets">> => [?BUDGET_INPUT], <<"NextToken">> => ?NEXT_TOKEN}).
-define(GET_BUDGETS_2_PAGE, #{<<"Budgets">> => [?BUDGET_INPUT]}).
-define(GET_BUDGETS_ALL, #{<<"Budgets">> => [?BUDGET_INPUT, ?BUDGET_INPUT]}).

setup() ->
    meck:new(?EHTTPC, [passthrough]),
    meck:expect(?EHTTPC, request, 6, fun do_erlcloud_httpc_request/6),
    [?EHTTPC].

erlcloud_budgets_test_() ->
    {
        foreach,
        fun setup/0,
        fun meck:unload/1,
        [
            fun test_describe_budget/0,
            fun test_describe_budgets/0,
            fun test_describe_budgets_pagination/0,
            fun test_describe_budgets_all/0
        ]
    }.


%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------

test_describe_budget() ->
    Request  = #{<<"BudgetName">> => ?BUDGET_NAME, <<"AccountId">> => ?ACC_ID},
    Expected = {ok, ?GET_BUDGET},
    TestFun  =
      fun(Config) ->
          erlcloud_budgets:describe_budget(?ACC_ID, ?BUDGET_NAME, Config)
      end,
    do_test(Request, Expected, TestFun).

test_describe_budgets() ->
    Request  = #{<<"AccountId">> => ?ACC_ID},
    Expected = {ok, ?GET_BUDGETS_1_PAGE},
    TestFun  =
      fun(Config) ->
          erlcloud_budgets:describe_budgets(?ACC_ID, Config)
      end,
    do_test(Request, Expected, TestFun).


test_describe_budgets_pagination() ->
    Options = #{<<"MaxResults">> => 1,
                <<"NextToken">>  => ?NEXT_TOKEN},
    Request  = maps:merge(Options, #{<<"AccountId">> => ?ACC_ID}),
    Expected = {ok, ?GET_BUDGETS_2_PAGE},
    TestFun  =
      fun(Config) ->
          erlcloud_budgets:describe_budgets(?ACC_ID, Options, Config)
      end,
    do_test(Request, Expected, TestFun).

test_describe_budgets_all() ->
    Request1  = #{<<"AccountId">> => ?ACC_ID},
    Request2  = #{<<"AccountId">> => ?ACC_ID, <<"NextToken">> => ?NEXT_TOKEN},
    Expected = {ok, ?GET_BUDGETS_ALL},
    TestFun  =
      fun(Config) ->
          erlcloud_budgets:describe_budgets_all(?ACC_ID, Config)
      end,
    do_test_pagination([Request1, Request2], Expected, TestFun).


%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

do_test(Request, ExpectedResult, TestedFun) ->
    Config = erlcloud_budgets:new("test-access-key", "test-secret-key"),
    ?assertEqual(ExpectedResult, TestedFun(Config)),
    Encoded = jsx:encode(Request),
    ?assertMatch([{_, {?EHTTPC, request, [_, post, _, Encoded, _, _]}, _}],
                 meck:history(?EHTTPC)).

do_test_pagination(Requests, ExpectedResult, TestedFun) ->
    Config = erlcloud_budgets:new("test-access-key", "test-secret-key"),
    ?assertEqual(ExpectedResult, TestedFun(Config)),
    Bodies = [ jsx:encode(R) || R <- Requests ],
    [ ?assertMatch({_, {?EHTTPC, request, [_, post, _, B, _, _]}, _}, H)
                    || {B, H} <- lists:zip(Bodies, meck:history(?EHTTPC)) ].

do_erlcloud_httpc_request(_, post, Headers, Body, _, _) ->
    Target = proplists:get_value("x-amz-target", Headers),
    ["AWSBudgetServiceGateway", Operation] = string:tokens(Target, "."),
    Input = jsx:decode(Body, [return_maps]),
    MaybeToken = maps:get(<<"NextToken">>, Input, undefined),
    RespBody =
        case {Operation, MaybeToken} of
            {"DescribeBudget", _} -> ?GET_BUDGET;
            {"DescribeBudgets", undefined} -> ?GET_BUDGETS_1_PAGE;
            {"DescribeBudgets", ?NEXT_TOKEN} -> ?GET_BUDGETS_2_PAGE
        end,
    {ok, {{200, "OK"}, [], jsx:encode(RespBody)}}.
