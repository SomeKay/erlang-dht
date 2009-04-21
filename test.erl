%% Author: Kristian Poslek
%% Created: 2009.04.11

-module(test).
-export([addItems/1]).

addItems(Name) ->
	Name!{put,blek},
	Name!{put,krakow},
	Name!{put,ab},
	Name!{put,dokument},
	Name!{put,zakon},
	Name!{put,mp3},
	Name!{put,zavrzlama},
	Name!{put,skafoskafnjak},
	Name!{put,brana},
	Name!{put,kilo},
	Name!{put,tristih}.