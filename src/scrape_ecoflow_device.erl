-module(scrape_ecoflow_device).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).
-define(metrics_collect_period, 1000).

-record(state, {
	connect :: pid(),
	metrics = #{} :: map(),
	timer :: undefined | reference(),
	device :: undefined | list(),
	declared_keys = #{} :: map()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(MqttData :: map()) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(MqttData) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [MqttData], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([#{
	mqtt_url := MqttUrl,
	mqtt_port := MqttPort,
	mqtt_acc := MqttAcc,
	mqtt_pass := MqttPass,
	mqtt_client_id := MqttClientId,
	user_device := UserDevice
}]) ->
	{ok, Connection, false} = mqtt_client:connect(#{
		transport => {ssl, #{host => MqttUrl, port => MqttPort, verify => verify_none}},
		username => MqttAcc,
		password => MqttPass,
		client_id => MqttClientId
	}),
	lists:foreach(fun(Device) ->
		Topic = <<"/app/device/property/", Device/binary>>,
		{ok, _} = mqtt_client:subscribe(Connection, [{Topic, 0}])
	end, UserDevice),
	{ok, #state{connect = Connection, device = UserDevice}, {continue, start_metrics}}.



handle_continue(start_metrics, State = #state{device = UserDevice}) ->
	case application:get_env(scrape_ecoflow, prometheus) of
		{ok, false} ->
			{noreply, State};
		{ok, _} ->
			prometheus_gauge:declare([{name, ecoflow_online}, {labels, [device]}, {help, "1 if device is online"}]),
			lists:foreach(fun(Device) ->
				prometheus_gauge:set(ecoflow_online, [binary_to_list(Device)], 1)
			end, UserDevice),
			{noreply, reset_metrics_timer(State)};
		_ ->
			{noreply, State}
	end.
handle_call(_Request, _From, State = #state{}) ->
	{reply, ok, State}.
handle_cast(_Request, State = #state{}) ->
	{noreply, State}.

handle_info(collect_metrics, #state{metrics = Metrics, device = Device, declared_keys = DKeys} = State) ->
	DeclaredKeys = lists:foldl(fun(SN, Acc) ->
		DeviceMetrics = maps:get(SN, Metrics, #{}),
		OldDKeys = maps:get(SN, Acc, []),
		Keys = maps:keys(DeviceMetrics),
		NewDKeys = [
			begin
				prometheus_gauge:declare([{name, binary_to_atom(K)}, {labels, [device]}, {help, "Ecoflow metric"}]),
				K
			end || K <- Keys, not lists:member(K, OldDKeys)
		],
		AllDeclaredKeys = lists:usort(OldDKeys ++ NewDKeys),
		lists:foreach(fun(Key) ->
			Val = maps:get(Key, DeviceMetrics),
			Val1 = case Val of
				[E|_] -> E;
				_ -> Val
			end,
			prometheus_gauge:set(binary_to_atom(Key), [binary_to_list(SN)], Val1)
		end, AllDeclaredKeys),
		Acc#{SN => AllDeclaredKeys}
	end, DKeys, Device),
	{noreply, reset_metrics_timer(State#state{declared_keys = DeclaredKeys})};

handle_info({mqtt_client, _Pid, {publish, <<"/app/device/property/", SN/binary>>, Data, _}},
	State = #state{metrics = Metrics}) ->
	OldMetrics = maps:get(SN, Metrics, #{}),
	#{<<"params">> := Params} = jsx:decode(Data),
	NewMetrics = maps:from_list([{convert_key(K), V} || {K, V} <- maps:to_list(Params)]),
%%	io:format("OldMetrics ~p~n~n~n", [{SN, OldMetrics}]),
	{noreply, State#state{metrics = Metrics#{SN => maps:merge(OldMetrics, NewMetrics)}}}.


terminate(_Reason, _State = #state{}) ->
	ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
convert_key(Key) ->
	Key1 = binary_to_list(Key),
	Key2 = lists:flatten([
		begin
			case C of
				V when V >= 65 andalso V =< 90 -> ["_", V];
				46 -> "_";
				_ -> C
			end
		end || C <- Key1
	]),
	Key3 = string:lowercase(Key2),
	Key4 = list_to_binary(Key3),
	<<"ecoflow_", Key4/binary>>.

reset_metrics_timer(#state{timer = T} = State) ->
	is_reference(T) andalso erlang:cancel_timer(T),
	State#state{timer = erlang:send_after(?metrics_collect_period, self(), collect_metrics)}.