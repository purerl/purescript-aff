-module(test_main@foreign).
-export([synchronousUnexpectedThrowError/0]).

synchronousUnexpectedThrowError() -> fun () ->
  error("sync-error")
end.
