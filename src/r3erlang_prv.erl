-module(r3erlang_prv).

-export([
    init/1,
    do/1,
    format_error/1
]).

-define(Usage,
    "Usage: rebar3 erlang [Options]~n"
    "~n"
    "Options:~n"
    "   path    Prints output Erlang path~n"
).

-define(Provider, [
    {name, erlang},
    {module, ?MODULE},
    {deps, [app_discovery]},
    {desc, "Rebar3 plugin for creating an Erlang release"}
]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, rebar_state:add_provider(State, providers:create(?Provider))}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        ["path"] -> io:format(erlang_dir(State));
        [] -> make_release(State);
        _Other -> io:format(?Usage)
    end,
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

make_release(State) ->
    r3erlang_release:make(app_name(State), apps_dir(State), erlang_dir(State)),
    io:format("Erlang ~s is written into ~s~n",
              [r3erlang_release:erlang_version(), erlang_dir(State)]).

app_name(State) ->
    App = hd(rebar_state:project_apps(State)),
    binary_to_atom(rebar_app_info:name(App), utf8).

apps_dir(State) -> rebar_dir:deps_dir(State).
erlang_dir(State) -> filename:join(rebar_dir:base_dir(State), "erl").
