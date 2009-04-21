%% Author: Kristian Poslek
%% Created: 2009.04.10

-module(helper).
-export([hash/1, lookup/3, isNextPeer/3, extractLowerElements/2, extractHigherElements/2, isInList/2]).

%% +++++++++++++++++++++++ hash/1
%% Used to calculate the key for a given peer or resource.
hash(Term) ->
	erlang:phash(Term, round(math:pow(2, 32))).

%% +++++++++++++++++++++++ isNextPeer/3
%% Used to determine if the peer belongs in this area.
isNextPeer(RequestKey, PeerKey, PeerNextKey) ->
	if 
		%% Prevents anyone from using the same name.
		RequestKey == PeerKey ->
			same;
		%% If the entering peer has the lowest value.
		((RequestKey < PeerNextKey) and (PeerKey >= PeerNextKey)) ->
			foundFirst;
		%% The key is lower than the key of the inviter to the ring.
		RequestKey < PeerKey ->
			lower;
		%% The entering peer has the highest key and is the next peer.
		((PeerKey == PeerNextKey) and (RequestKey > PeerKey)) ->
			foundLast;
		%% The entering peer is the last peer in the ring. 
		(RequestKey > PeerKey) and (PeerKey > PeerNextKey) ->
			foundLast;
		%% The entering peer is greater than the scope of the inviter.
		RequestKey > PeerNextKey ->
			greater;			
		%% The entering peer is really the next peer.
		((PeerKey < RequestKey) and (RequestKey < PeerNextKey)) ->
		  	found
	end.
	

%% +++++++++++++++++++++++ lookup/3
%% Used to check if a resource belong to a given key range.
lookup(ResourceKey, PeerKey, NextPeerKey) ->
	if 
		%% When the Resource really is between peers.
		((ResourceKey > PeerKey) and (ResourceKey < NextPeerKey)) ->
			true;
		%% Protection if the first peer (lower one) should get the resource.
		((ResourceKey < PeerKey) and (PeerKey > NextPeerKey) and (ResourceKey > NextPeerKey)) ->
			false;
		%% When the Resource is on the last peer.
		((ResourceKey < PeerKey) and (PeerKey > NextPeerKey)) ->
			true;
		%% When there's only one peer.
		PeerKey == NextPeerKey ->
			true;
		%% Additional guard for the last peer.
		((ResourceKey > PeerKey) and (PeerKey > NextPeerKey)) ->
			true;
		true ->
			false
	end.


%% +++++++++++++++++++++++ isInList/2
%% Checks if the request key is in the key list.
isInList(Key, List) ->
	NewList = lists:filter(fun({ResKey,_}) -> ResKey == Key end, List),
	if
		NewList == [] ->
			false;
		true ->
			true
	end.
	

%% +++++++++++++++++++++++ extractLowerElements
%% Extracts elements of a list that are lower than the second argument.
extractLowerElements([], _) ->
	[];

extractLowerElements([Element|Rest], Value) ->
  	{Key, Resource} = Element,
	if 
		Key < Value ->
			[{Key, Resource}|extractLowerElements(Rest, Value)];
		true ->
			extractLowerElements(Rest, Value)
	end.


%% +++++++++++++++++++++++ extractHigherElements
%% Extracts elements of a list that are higher than the second argument.
extractHigherElements([], _) ->
	[];

extractHigherElements([Element|Rest], Value) ->
  	{Key, Resource} = Element,
	if 
		Key > Value ->
			[{Key, Resource}|extractHigherElements(Rest, Value)];
		true ->
			extractHigherElements(Rest, Value)
	end.