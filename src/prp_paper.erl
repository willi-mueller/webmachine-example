-module(prp_paper).

-export([init/1, content_types_provided/2, content_types_accepted/2,
		allowed_methods/2, resource_exists/2, to_html/2, to_json/2,
		from_json/2, delete_resource/2, post_is_create/2, create_path/2,
		allow_missing_post/2]).


-include_lib("webmachine/include/webmachine.hrl").


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
	{paper, Id1, Title} = prp_schema:read_paper(Id),
	Resp = paper2json(Id1, Title),
	{Resp, RD, Ctx}.


from_json(RD, Ctx) ->
	from_json(RD, Ctx, get_title(RD)).

-spec from_json(wm_reqdata(), any(), {error, no_data})
			-> {{halt, 400}, wm_reqdata(), any()};
		(wm_reqdata(), any(), string())
			-> {boolean(), wm_reqdata(), any()}.
from_json(RD, Ctx, {error, no_data}) ->
	signal_malformed(RD, Ctx).

from_json(RD, Ctx, Title) ->
	Id = id_from_path(RD),
	Resp = set_location_header_if_not_exists(RD, Ctx, Id),
	prp_schema:create_paper(list_to_integer(Id), Title),
	Resp1 = wrq:set_resp_body(paper2json(Id, Title), Resp),
	{true, Resp1, Ctx}.


delete_resource(RD, Ctx) ->
	Id = wrq:path_info(id, RD),
	prp_schema:delete_paper(Id),
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
	{Path, RD, Ctx}.


%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_title(wm_reqdata()) -> string() | {atom(), atom()}.
get_title(RD) ->
	case wrq:req_body(RD) of
	<<"title=", Title/binary>> ->
		binary_to_list(Title);
	_Else ->
		{error, no_data}
	end.


-spec id_from_path(wm_reqdata()) -> string().
id_from_path(RD) ->
	case wrq:path_info(id, RD) of
		undefined->
			["paper", Id] = string:tokens(wrq:disp_path(RD), "/"),
			Id;
		Id -> Id
	end.


-spec set_location_header_if_not_exists(wm_reqdata(), any(), string()) -> wm_reqdata().
set_location_header_if_not_exists(RD, Ctx, Id) ->
	case resource_exists(RD, Ctx) of
		{false, _, _} ->
			wrq:set_resp_header("Location", Id, RD);
		{true, _, _}  -> RD
	end.

-spec signal_malformed_request(wm_reqdata(), any())-> {{halt, 400}, wm_reqdata(), any()}
signal_malformed_request(RD, Ctx) ->
	{{halt, 400}, RD, Ctx};


-spec paper2json(integer(), string()) -> string();
				(string(), string()) -> string().
paper2json(Id, Title) when is_integer(Id) ->
	paper2json(integer_to_list(Id), Title);
paper2json(Id, Title) ->
	mochijson:encode({struct, [
					{id, Id},
					{title, Title} ]}).


-spec generate_id()->integer().
generate_id() ->
	mnesia:table_info(paper, size) + 1.
