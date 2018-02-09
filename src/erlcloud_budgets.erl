-module(erlcloud_budgets).


%% Budgets API implementation
%% https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/api-reference.html

-include("erlcloud_aws.hrl").

%% Initialization API
-export([configure/2, configure/3, configure/4, configure/5,
         new/2, new/3, new/4, new/5]).

%% API
-export([
    describe_budget/3,
    describe_budgets/2,
    describe_budgets/3,
    describe_budgets_all/2
    % describe_notifications/1,
    % describe_subscribers/1
]).


%%------------------------------------------------------------------------------
%% Initialization API
%%------------------------------------------------------------------------------

-spec new(string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                budgets_host       = Host,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                budgets_host       = Host,
                budgets_port       = Port,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec new(string(), string(), string(), non_neg_integer(), string()) ->
    aws_config().
new(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    #aws_config{access_key_id     = AccessKeyID,
                secret_access_key = SecretAccessKey,
                budgets_host       = Host,
                budgets_port       = Port,
                budgets_scheme     = Scheme,
                retry             = fun erlcloud_retry:default_retry/1}.

-spec configure(string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey)),
    ok.

-spec configure(string(), string(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host)),
    ok.

-spec configure(string(), string(), string(), non_neg_integer()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host, Port)),
    ok.

-spec configure(string(), string(), string(), non_neg_integer(), string()) -> ok.
configure(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    put(aws_config, new(AccessKeyID, SecretAccessKey, Host, Port, Scheme)),
    ok.


%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

%% @doc
%% Budgets API:
%% https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/describe-budget.html
%%
%% `
%% erlcloud_budgets:describe_budget(<<"123456789123">>, <<"budget">>, Config).
%% `
%%


%% TODO binary or string?
%% TODO default config or not? (I vote NOT)
-spec describe_budget(binary(), binary(), aws_config()) ->
    {ok, map()} | {error, any()}.
describe_budget(AccountId, BudgetName, Config) ->
    request(Config, <<"DescribeBudget">>, #{
        <<"BudgetName">> => BudgetName,
        <<"AccountId">> => AccountId
    }).


%% @doc
%% Budgets API:
%% https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/list-budget.html
%%
%% `
%% Opts = #{<<"MaxResults">> => 10},
%% {ok, R} =
%%       erlcloud_budgets:describe_budgets(<<"123456789123">>, Opts, Config).
%% Budgets = maps:get(<<"Budgets">>, R).
%% NextToken = maps:get(<<"NextToken">>, R).
%%
%% Opts2 = #{<<"MaxResults">> => 10, <<"NextToken">>  => NextToken},
%% {ok, R2} =
%%       erlcloud_budgets:describe_budgets(<<"123456789123">>, Opts2, Config).
%% `
%%


%% TODO shall we remove "Budgets" from the map and provide list?
%% TODO what should be the output of pagination, just map or separate token?
-spec describe_budgets(binary(), aws_config()) -> {ok, map()} | {error, any()}.
describe_budgets(AccountId, Config) ->
    describe_budgets(AccountId, #{}, Config).

-spec describe_budgets(binary(), map(), aws_config()) -> {ok, map()} | {error, any()}.
describe_budgets(AccountId, Options, Config) ->
    request(Config, <<"DescribeBudgets">>, maps:merge(Options, #{
        <<"AccountId">> => AccountId
    })).



%% does listing of all budgets:
%% `
%% erlcloud_budgets:describe_budget_all(<<"123456789123">>, Config).
%% `

-spec describe_budgets_all(binary(), aws_config()) -> {ok, map()} | {error, any()}.
describe_budgets_all(AccountId, Config) ->
    Fun = fun(Pagination) ->
        describe_budgets(AccountId, Pagination, Config)
    end,
    list_all(Fun, <<"Budgets">>).


%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

request(Config0, Operation, Input) ->
    case erlcloud_aws:update_config(Config0) of
        {ok, Config} ->
            Body       = jsx:encode(Input),
            Headers    = get_headers(Config, Operation, Body),
            AwsRequest = #aws_request{service         = budgets,
                                      uri             = get_url(Config),
                                      method          = post,
                                      request_headers = Headers,
                                      request_body    = Body},
            io:format("doing request ~p~n", [AwsRequest]),
            request(Config, AwsRequest);
        {error, Reason} ->
            {error, Reason}
    end.

request(Config, Request) ->
    Result = erlcloud_retry:request(Config, Request, fun handle_result/1),
    case erlcloud_aws:request_to_return(Result) of
        {ok, {_, <<>>}}     -> {ok, #{}};
        {ok, {_, RespBody}} ->
            io:format("result: ~p~n", [RespBody]),
            {ok, jsx:decode(RespBody, [return_maps])};
        {error, _} = Error  -> Error
    end.

handle_result(#aws_request{response_type = ok} = Request) ->
    Request;
handle_result(#aws_request{response_type    = error,
                            error_type      = aws,
                            response_status = Status} = Request)
  when Status >= 500 ->
    Request#aws_request{should_retry = true};
handle_result(#aws_request{response_type = error,
                           error_type    = aws} = Request) ->
    Request#aws_request{should_retry = false}.

get_headers(#aws_config{budgets_host = Host} = Config, Operation, Body) ->
    Headers = [{"host",         Host},
               {"x-amz-target", make_amz_target(Operation)},
               {"content-type", "application/x-amz-json-1.1"}],
    Region = erlcloud_aws:aws_region_from_host(Host),
    erlcloud_aws:sign_v4_headers(Config, Headers, Body, Region, "budgets").

get_url(#aws_config{budgets_scheme = Scheme,
                    budgets_host   = Host,
                    budgets_port   = Port}) ->
    Scheme ++ Host ++ ":" ++ integer_to_list(Port).

make_amz_target(Operation) ->
    "AWSBudgetServiceGateway." ++ binary_to_list(Operation).

list_all(Fun, ObjName) ->
    list_all_(Fun, ObjName, #{}, []).

list_all_(Fun, ObjName, Pagination, Acc) ->
    case Fun(Pagination) of
        {ok, #{ObjName := Objects, <<"NextToken">> := NextToken}} ->
            NextPage = #{<<"NextToken">> => NextToken},
            list_all_(Fun, ObjName, NextPage, Objects ++ Acc);
        {ok, #{ObjName := Objects}} ->
            {ok, #{ObjName => Objects ++ Acc}};
        {error, _Reason} = Error ->
            Error
    end.