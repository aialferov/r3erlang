-module(r3erlang).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) -> r3erlang_prv:init(State).
