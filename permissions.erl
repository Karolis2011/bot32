-module(permissions).
-compile(export_all).

-include("definitions.hrl").

get_with_rank(Rank) ->
	orddict:fetch_keys(orddict:filter(fun(_,V) -> lists:member(Rank, V) end,
			config:require_value(config, [permissions]))).

rankof(Usr) -> rankof(Usr, #{id=>none}, #{id=>none}).
rankof(Usr, Channel) -> rankof(Usr, Channel, #{id=>none}).
rankof(Usr, none, none) -> rankof(Usr);
rankof(Usr, Channel, none) -> rankof(Usr, Channel);
rankof(#{id:=UserId}, #{id:=ChannelId}, #{id:=GuildId}) ->
	Permissions = config:require_value(config, [permissions]),
	orddict:fold(fun
			({user, Id}, Perms, PermsSoFar) ->
				if Id == UserId -> lists:umerge(lists:usort(Perms), PermsSoFar);
					true -> PermsSoFar
				end;
			({chan, Id}, Perms, PermsSoFar) ->
				if Id == ChannelId -> lists:umerge(lists:usort(Perms), PermsSoFar);
				   true -> PermsSoFar
				end;
			({guild, Id}, Perms, PermsSoFar) ->
				if Id == GuildId -> lists:umerge(lists:usort(Perms), PermsSoFar);
					true -> PermsSoFar
				end
		end, [user], Permissions).

rankof_chan(Channel) ->
	case config:get_value(config, [permissions, list_to_binary(Channel)]) of
		'$none' -> [user];
		List -> List
	end.

hasperm(_, user) -> true;
hasperm(User, Perm) -> lists:member(Perm, rankof(User)).

hasperm(_, _, user) -> true;
hasperm(User, Chan, Perm) -> lists:member(Perm, rankof(User, Chan)).
