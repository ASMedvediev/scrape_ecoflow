-module(scrape_ecoflow).

%% API
-export([
	connect/0
]).


connect() ->
	{ok, UserData} = authorize(),
	{ok, MqttData} = get_mqtt_credentials(UserData),
	MqttData.


%% internal functions

authorize() ->
	Url = "https://api.ecoflow.com/auth/login",
	Headers = [{"lang", "en_US"}, {"content-type", "application/json"}],
	{ok, UserName} = application:get_env(scrape_ecoflow, user_name),
	{ok, UserPass} = application:get_env(scrape_ecoflow, user_password),
	Data = jsx:encode([
		{<<"email">>, UserName},
		{<<"password">>, base64:encode(UserPass)},
		{<<"scene">>, <<"IOT_APP">>},
		{<<"userType">>, <<"ECOFLOW">>}
	]),
	case httpc:request(post, {Url, Headers, "application/json", Data}, [], [{body_format, binary}]) of
		{ok, {{_, 200, _}, _, Res}} ->
			#{<<"data">> := #{
				<<"token">> := Token,
				<<"user">> := #{
					<<"name">> := Name,
					<<"userId">> := UserId
				}}} = jsx:decode(Res),
			io:format("Successfully logged in: ~p~n", [Name]),
			{ok, #{
				token => Token,
				user_id => UserId
			}};
		Err ->
			io:format("Error: ~p~n", [Err]),
			{error, bad_authorize}
	end.


get_mqtt_credentials(#{token := Token, user_id := UserId}) ->
	Url = "https://api.ecoflow.com/iot-auth/app/certification",
	Headers = [{"lang", "en_US"}, {"authorization", <<"Bearer ", Token/binary>>}],
	_Data = jsx:encode([{<<"userId">>, UserId}]),
	case httpc:request(get, {Url, Headers}, [], [{body_format, binary}]) of
		{ok, {{_, 200, _}, _, Res}} ->
			#{<<"data">> := #{
				<<"url">> := MqttUrl,
				<<"port">> := MqttPort,
				<<"certificateAccount">> := MqttAcc,
				<<"certificatePassword">> := MqttPass
			}} = jsx:decode(Res),
			io:format("Successfully extracted account: ~p~n", [MqttAcc]),
			{ok, #{
				mqtt_url => binary_to_list(MqttUrl),
				mqtt_port => binary_to_integer(MqttPort),
				mqtt_acc => binary_to_list(MqttAcc),
				mqtt_pass => binary_to_list(MqttPass),
				mqtt_client_id => <<"ANDROID_6415a6c4-1963-4fa5-96f1-03b59f64ddf6_", UserId/binary>>
			}};
		Err ->
			io:format("Error: ~p~n", [Err]),
			{error, bad_mqtt_credentials}
	end.