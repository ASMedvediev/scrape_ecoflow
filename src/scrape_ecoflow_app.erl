-module(scrape_ecoflow_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	application:get_env(scrape_ecoflow, prometheus) =:= {ok, httpd} andalso
		begin
			{ok, _} = application:ensure_all_started(prometheus_httpd),
			prometheus_httpd:start()
		end,
	scrape_ecoflow_sup:start_link().

stop(_State) ->
	ok.

%% internal functions
