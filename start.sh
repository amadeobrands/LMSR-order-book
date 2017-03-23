#if you want to use a different port, then start like this:

./rebar get-deps
./rebar compile #this line checks if any modules were modified, and recompiles them if they were. only needed during testing.
rm -r data/*
erl -pa ebin deps/*/ebin/ -eval "application:ensure_all_started(lmsr_order_book)"
