-module(prp_schema).

-export([
	create_paper/2,
	read_paper/1,
	delete_paper/1,
	paper_exists/1,
	init_tables/0,
	clear_tables/0,
	fill_with_dummies/0,
	delete_dummies/0
]).

-compile([export_all]).

-include("include/prp_datatypes.hrl").


%%%%%%%%%%%%%%%%%%%%%
%% Storage API
%%%%%%%%%%%%%%%%%%%%

create_paper(ID, Title) ->
	io:format("~p Paper created: ~p, ~p~n", [?LINE, ID, Title]),
	Write = fun() -> mnesia:write({paper, ID, Title}) end,
	case mnesia:transaction(Write) of
		{atomic, ok} -> ok
	end.


read_paper(Id) when is_list(Id)->
	read_paper(list_to_integer(Id));

read_paper(ID) ->
	Read = fun() -> mnesia:read(paper, ID) end,
	case mnesia:transaction(Read) of
		{atomic, [Paper]} -> Paper;
		{atomic, []} -> {error, not_exists}
	end.


update_paper(Id, Title) ->
	Read = fun() -> mnesia:write({paper, Id, Title}) end,
	case mnesia:transaction(Read) of
		{atomic, ok} -> ok
	end.


delete_paper(Id) when is_list(Id)->
	delete_paper(list_to_integer(Id));

delete_paper(ID) ->
	Delete = fun() -> mnesia:delete({paper, ID}) end,
	case mnesia:transaction(Delete) of
		{atomic, ok} ->
			ok
	end.


paper_exists(Id) when is_list(Id)->
	paper_exists(list_to_integer(Id));

paper_exists(Id) ->
	Read = fun() -> mnesia:read(paper, Id) end,
	case mnesia:transaction(Read) of
		{atomic, []} ->
			io:format("~p, Id: ~p Exists: ~p ~n", [?LINE, Id, false]),
			false;
		{atomic,[{paper, _, _}]} ->
			io:format("~p, Id: ~p Exists: ~p ~n", [?LINE, Id, true]),
			true
	end.


%%%%%%%%%%%%%%%%%%%%%
%% Schema
%%%%%%%%%%%%%%%%%%%%

init_tables() ->
	mnesia:create_table(paper, [{attributes, record_info(fields, paper)}]).

clear_tables() ->
	mnesia:clear_table(paper).

delete_tables() ->
	mnesia:delete_table(paper).


%%%%%%%%%%%%%%%%%%%%%
%% Fixtures
%%%%%%%%%%%%%%%%%%%%


fill_with_dummies() ->
	Fill = mnesia:transaction(fun() ->
		mnesia:write({paper, 1, "1"}),
		mnesia:write({paper, 2, "2"}),
		mnesia:write({paper, 3, "3"}) end),

	case Fill of
		{atomic, ok} -> ok
	end.


delete_dummies() ->
	mnesia:transaction(fun() ->
				[ mnesia:delete({paper, ID}) || ID <- lists:seq(1, 3) ] end).

