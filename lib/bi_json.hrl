%% @author Johannes Huning <johannes.huning@wooga.net>
%% @author John-Paul Bader <john-paul.bader@wooga.net>
%% @copyright 2012 Wooga GmbH

% During development JSON will be written in the proplist format, that is an
% unordered, potentially nested, list of tuples. However, proplists do not
% allow for modification in the sense we desire it. Thus JSON is ultimatively
% stored as orddicts, with functions or macros hiding the underlying
% representation (orddict) used.

%% @doc Implementation used to represent JSON structures.
-define (JSON_IMPL, orddict).

%% @doc Literal marking an empty JSON object.
-define (JSON_EMPTY_OBJ, []).
