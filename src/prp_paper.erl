-module(prp_paper).

-compile([export_all]).

-include_lib("webmachine/include/webmachine.hrl").
-include("include/prp_datatypes.hrl").

init(Config) ->
	prp_schema:init_tables(),
	prp_schema:fill_with_dummies(),
	{{trace, "traces"}, Config}.    %% debugging


content_types_provided(RD, Ctx) ->
	{[ {"text/html", to_html}, {"application/json", to_json} ], RD, Ctx}.

allowed_methods(RD, Ctx) ->
	{['GET', 'POST', 'PUT', 'DELETE', 'HEAD'], RD, Ctx}.

resource_exists(RD, Ctx) ->
	Id = wrq:path_info(id, RD),
	{prp_schema:paper_exists(Id), RD, Ctx}.

to_html(RD, Ctx) ->
	Id = wrq:path_info(id, RD),
	Resp = "<html><body>" ++ Id ++ "</body></html>",
	{Resp, RD, Ctx}.
