~/rebar3/rebar3 compile && erl -noshell -pa _build/default/lib/*/ebin -eval "homecontrol_app:start($1,$2)"
