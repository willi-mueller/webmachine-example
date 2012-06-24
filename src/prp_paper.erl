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
	Id = id_from_path(RD),

	<<"title=", Title/binary>> = wrq:req_body(RD),
	Title1 = binary_to_list(Title),

	prp_schema:create_paper(list_to_integer(Id), Title1),

	JSON = paper2json(Id, Title1),
	Resp = wrq:set_resp_body(JSON, RD),

	case resource_exists(Resp, Ctx) of
		{true, _, _}  -> {true, Resp, Ctx};
		{false, _, _} ->
			R = wrq:set_resp_header("Location", Id, Resp),
			{true, R, Ctx}
	end.


delete_resource(RD, Ctx) ->
	Id = wrq:path_info(id, RD),
	prp_schema:delete_paper(Id),
	io:format("~p, After Delete: ~p~n", [?LINE, prp_schema:paper_exists(Id)]),
	{true, RD, Ctx}.


%%%%%%%%%
% POST
%%%%%%%%%

post_is_create(RD, Ctx) ->
	{true, RD, Ctx}.


allow_missing_post(RD, Ctx) ->
	{true, RD, Ctx}.


create_path(RD, Ctx) ->
	Path = "/paper/" ++ integer_to_list(generate_id()),
	io:format("~p Created Path:~p~n", [?LINE, Path]),
	{Path, RD, Ctx}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec id_from_path(_) -> list().
%% _ should be wm_reqdata() but this type is not exported
id_from_path(RD) ->
	case wrq:path_info(id, RD) of
		undefined->
			["paper", Id] = string:tokens(wrq:disp_path(RD), "/"),
			Id;
		Id -> Id
	end.


-spec paper2json(list(), list()) -> list().
paper2json(Id, Title) ->
	mochijson:encode({struct, [
					{id, Id},
					{title, Title} ]}).


-spec generate_id()->integer().
generate_id() ->
	mnesia:table_info(paper, size) + 1.

