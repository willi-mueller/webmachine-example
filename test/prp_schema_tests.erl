-module(prp_schema_tests).

-include_lib("eunit/include/eunit.hrl").


create_paper_test() ->
	?assertEqual(ok, prp_schema:create_paper(999, "ABC")),
	?assertEqual([{paper, 999, "ABC"}], mnesia:dirty_read(paper, 999)),
	mnesia:dirty_delete(paper, 999),
	?assertEqual([], mnesia:dirty_read(paper, 999)).


read_paper_test() ->
	mnesia:dirty_write({paper, 123, "ABC"}),
	?assertEqual({paper, 123, "ABC"}, prp_schema:read_paper(123)),
	mnesia:dirty_delete(paper, 123),
	?assertEqual({error, not_exists}, prp_schema:read_paper(123)).


update_paper_test() ->
	?assertEqual({paper, 1, "1"}, prp_schema:read_paper(1)),
	?assertEqual(ok, prp_schema:update_paper(1, "DEF")),
	?assertEqual({paper, 1, "DEF"}, prp_schema:read_paper(1)),
	mnesia:dirty_write({paper, 1, 1}).


delete_paper_test() ->
	?assertEqual(ok, prp_schema:create_paper(99, "ABC")),
	?assertEqual(ok, prp_schema:delete_paper(99)).


exists_paper_test() ->
	?assertEqual(true, prp_schema:paper_exists(1)),
	?assertEqual(true, prp_schema:paper_exists("1")),
	?assertEqual(false, prp_schema:paper_exists(999)).


permanent_save_test() ->
	?assertEqual(ok, mnesia:dirty_write({paper, 123, "abc"})),
	?assertEqual([{paper,123,"abc"}], mnesia:dirty_read(paper, 123)),
	mnesia:dirty_delete(paper, 999).


fill_with_dummies_test() ->
	?assertEqual(ok, prp_schema:fill_with_dummies()),
	?assertEqual([{paper, 2, "2"}], mnesia:dirty_read(paper, 2)).


delete_dummies_test() ->
	prp_schema:fill_with_dummies(),
	?assertEqual([{paper, 1, "1"}] , mnesia:dirty_read(paper, 1)),
	prp_schema:delete_dummies(),
	?assertEqual([], mnesia:dirty_read(paper, 1)),
	prp_schema:fill_with_dummies().
