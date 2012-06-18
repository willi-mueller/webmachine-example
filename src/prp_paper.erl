-module(prp_paper).

-compile(export_all).

-include_lib("webmachine/include/webmachine.hrl").
-include("include/prp_datatypes.hrl").


init(Config) ->
	prp_schema:init_tables(),
	prp_schema:fill_with_dummies(),
	{{trace, "traces"}, Config}.    %% debugging


content_types_provided(RD, Ctx) ->
	{[ {"text/html", to_html}, {"application/json", to_json} ], RD, Ctx}.


content_types_accepted(RD, Ctx) ->
	{ [ {"application/json", from_json} ], RD, Ctx }.


allowed_methods(RD, Ctx) ->
	{['GET', 'POST', 'PUT', 'DELETE', 'HEAD'], RD, Ctx}.


resource_exists(RD, Ctx) ->
	Id = wrq:path_info(id, RD),
	{prp_schema:paper_exists(Id), RD, Ctx}.


to_html(RD, Ctx) ->
	Id = wrq:path_info(id, RD),
	Resp = "<html><body>" ++ Id ++ "</body></html>",
	{Resp, RD, Ctx}.


to_json(RD, Ctx) ->
	Id = wrq:path_info(id, RD),
	{paper, Id2, Title} = prp_schema:read_paper(Id),

	Resp = mochijson:encode({struct, [
		{id, integer_to_list(Id2)},
		{title, Title}
	]}),

	{Resp, RD, Ctx}.


from_json(RD, Ctx) ->
	Id = new_path(RD),

	<<"title=", Title/binary>> = wrq:req_body(RD),
	Title1 = binary_to_list(Title),

	prp_schema:create_paper(list_to_integer(Id), Title1),

	JSON = build_json(Id, Title1),
	Resp = wrq:set_resp_body(JSON, RD),
	{true, Resp, Ctx}.


delete_resource(RD, Ctx) ->
	Id = wrq:path_info(id, RD),
	prp_schema:delete_paper(Id),
	io:format("~p, After Delete: ~p~n", [?LINE, prp_schema:paper_exists(Id)]),
	{true, RD, Ctx}.


post_is_create(RD, Ctx) ->
	{true, RD, Ctx}.


allow_missing_post(RD, Ctx) ->
	{true, RD, Ctx}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%

new_path(RD) ->
	case wrq:path_info(id, RD) of
		undefined->
			["paper", ID] = string:tokens(wrq:disp_path(RD), "/"),
			ID;
		Id -> Id
	end.


build_json(Id, Title) ->
	list_to_binary( "{" ++  "\"id\":" ++ "\"" ++ Id ++ "\"" ++ ", " ++
		"\"title\":" ++ "\"" ++ Title ++ "\"" ++ "}" ).

generate_id() ->
	mnesia:table_info(paper, size) + 1.

