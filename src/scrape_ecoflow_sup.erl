-module(scrape_ecoflow_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
	MqttData = scrape_ecoflow:connect(),
	{ok, UserDevice} = application:get_env(scrape_ecoflow, user_device),
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
	    #{id => scrape_ecoflow_device,
		    start => {scrape_ecoflow_device, start_link, [MqttData#{user_device => UserDevice}]},
		    shutdown => infinity}
    ],
    {ok, {SupFlags, ChildSpecs}}.
