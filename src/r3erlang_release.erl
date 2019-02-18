-module(r3erlang_release).

-export([
    make/3,

    erlang_apps/2,
    erlang_version/0,

    erts_version/0
]).

-include("r3erlang_makefile.hrl").

make(AppName, AppsDir, OutDir) ->
    ErlangApps = erlang_apps(AppName, AppsDir),
    make_release("erlang", erlang_version(), ErlangApps, OutDir).

make_release(Name, Version, Apps, OutDir) ->
    Release = {release, {Name, Version}, {erts, erts_version()}, Apps},
    ReleaseContent = io_lib:format("~p.~n", [Release]),

    ok = filelib:ensure_dir(OutDir ++ "/"),
    ok = file:set_cwd(OutDir),

    IsOldErlang = is_old_erlang(),
    TarOptsAll = [{erts, code:root_dir()}, {dirs, [include]}],
    TarOpts = if IsOldErlang -> TarOptsAll;
             not IsOldErlang -> [no_warn_sasl|TarOptsAll] end,

    ok = file:write_file(Name ++ ".rel", ReleaseContent),
    ok = systools:make_script(Name, [no_warn_sasl]),
    ok = systools:make_tar(Name, TarOpts),

    {ok, TarFiles} = erl_tar:table(Name ++ ".tar.gz", [compressed]),
    lists:foreach(fun file:delete/1, TarFiles),
    ok = erl_tar:extract(Name ++ ".tar.gz", [compressed]),

    ok = filelib:ensure_dir("bin/"),
    [file:copy(Name ++ ".boot", path(["bin", File])) ||
     File <- ["start.boot", "start_clean.boot", "no_dot_erlang.boot"]],

    ok = file:write_file("Makefile", io_lib:format(?Makefile, [])),

    ok = file:delete(path(["releases", erlang_version(), Name ++ ".rel"])),
    ok = file:delete(path(["releases", Name ++ ".rel"])),
    ok = file:delete(path(["releases", erlang_version(), "start.boot"])),
    ok = file:del_dir(path(["releases", erlang_version()])),
    ok = file:del_dir(path(["releases"])),

    lists:foreach(fun file:delete/1, filelib:wildcard(Name ++ ".*")).

erlang_apps(RootAppName, AppsDir) ->
    IsLocal = is_app_local(RootAppName, AppsDir),
    IsLocal andalso begin
        true = code:add_path(path([AppsDir, RootAppName, "ebin"]))
    end,

    IsLoaded = is_app_loaded(RootAppName),
    IsLoaded orelse begin ok = application:load(RootAppName) end,

    {ok, RootAppVersion} = application:get_key(RootAppName, vsn),
    {ok, DepAppNames} = application:get_key(RootAppName, applications),

    DepApps = [erlang_apps(DepAppName, AppsDir) || DepAppName <- DepAppNames],
    Apps = lists:usort(lists:flatten([{RootAppName, RootAppVersion}|DepApps])),

    [{AppName, AppVersion} || {AppName, AppVersion} <- Apps,
     not is_app_local(AppName, AppsDir)].

is_app_loaded(AppName) ->
    lists:keymember(AppName, 1, application:loaded_applications()).

is_app_local(AppName, AppsDir) ->
    filelib:is_dir(path([AppsDir, AppName])).

erlang_version() ->
    lists:droplast(read_otp_release_file("OTP_VERSION")).

erts_version() ->
    VersionsContent = read_otp_release_file("installed_application_versions"),
    Versions = [list_to_tuple(string_split(VersionContent, "-")) ||
                VersionContent <- string_split(VersionsContent, "\n")],
    proplists:get_value("erts", Versions).

read_otp_release_file(FileName) ->
    OtpReleaseVersion = erlang:system_info(otp_release),
    Path = [code:root_dir(), "releases", OtpReleaseVersion, FileName],
    {ok, Binary} = file:read_file(filename:join(Path)),
    binary_to_list(Binary).

path(PathList) -> filename:join(PathList).

string_split(String, Separators) ->
    IsOldErlang = is_old_erlang(),
    if IsOldErlang -> string:tokens(String, Separators);
    not IsOldErlang -> string:split(String, Separators, all) end.

is_old_erlang() ->
    list_to_integer(erlang:system_info(otp_release)) < 20.
 
%del_dir_r(Dir) ->
%    Paths = filelib:wildcard(Dir ++ "/**"),
%    case lists:partition(fun filelib:is_dir/1, Paths) of
%        {Dirs, Files} ->
%            io:format("Deleting ~s~n", [Dir]),
%            lists:foreach(fun file:delete/1, Files),
%            lists:foreach(fun file:del_dir/1, lists:reverse(lists:sort(Dirs))),
%            file:del_dir(Dir);
%        [] -> ok
%    end.
