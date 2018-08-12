-module(r3erlang_release).

-export([
    make/3,

    erlang_apps/2,
    erlang_version/0,

    erts_version/0
]).

make(AppName, AppsDir, OutDir) ->
    ErlangApps = erlang_apps(AppName, AppsDir),
    make_release("erlang", erlang_version(), ErlangApps, OutDir).

make_release(Name, Version, Apps, OutDir) ->
    Release = {release, {Name, Version}, {erts, erts_version()}, Apps},
    ReleaseContent = io_lib:format("~p.~n", [Release]),

    ok = filelib:ensure_dir(OutDir ++ "/"),
    ok = file:set_cwd(OutDir),

    ok = file:write_file(Name ++ ".rel", ReleaseContent),
    ok = systools:make_script(Name, [no_warn_sasl]),
    ok = systools:make_tar(Name, [no_warn_sasl, {erts, code:root_dir()}]),

    {ok, TarFiles} = erl_tar:table(Name ++ ".tar.gz", [compressed]),
    lists:foreach(fun file:delete/1, TarFiles),
    ok = erl_tar:extract(Name ++ ".tar.gz", [compressed]),

    ok = filelib:ensure_dir("bin/"),
    [file:copy(Name ++ ".boot", path(["bin", File])) ||
     File <- ["start.boot", "no_dot_erlang.boot"]],

    {ok, ErtsFiles} = file:list_dir(path(["erts-" ++ erts_version(), "bin"])),
    lists:foreach(fun(File) ->
        Target = path(["..", "erts-" ++ erts_version(), "bin", File]),
        Link = path(["bin", File]),
        file:delete(Link),
        ok = file:make_symlink(Target, Link)
    end, ErtsFiles),

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
    string:trim(read_otp_release_file("OTP_VERSION")).

erts_version() ->
    VersionsContent = read_otp_release_file("installed_application_versions"),
    Versions = [list_to_tuple(string:split(VersionContent, "-")) ||
                VersionContent <- string:split(VersionsContent, "\n", all)],
    proplists:get_value("erts", Versions).

read_otp_release_file(FileName) ->
    OtpReleaseVersion = erlang:system_info(otp_release),
    Path = [code:root_dir(), "releases", OtpReleaseVersion, FileName],
    {ok, Binary} = file:read_file(filename:join(Path)),
    binary_to_list(Binary).

path(PathList) -> filename:join(PathList).
