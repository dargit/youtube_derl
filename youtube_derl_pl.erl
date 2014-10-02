%  youtube_derl_pl.erl
%  
%  Copyright 2014 Dario Conti
%  
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
%  MA 02110-1301, USA.
%  
%v.0.1  


-module(youtube_derl_pl).
-export([get_pl_data/1]).

get_pl_data(Url)->
	case httpc:request(Url) of
		{ok,{{_, 200, _}, _, Body}} ->
			%get playlist title
			Title = parse_html_tag(Body,"<h1 class=\"pl-header-title\">","</h1>"),
			%get playlist elements
			Elements_table = parse_html_tag(Body,"<tbody id=\"pl-load-more-destination\">","</tbody>"),
			Elements_list = re:split(Elements_table,"<tr class=\"pl-video yt-uix-tile \"",[{return,list},trim]),
			{Title,get_elements_from_list(Elements_list)}; %return {Playlist_title,[{Title,Video_id}]}		
		{error,Reason} ->
			erlang:error(Reason)
	end.

get_elements_from_list([H|T])->
	case re:run(H,"(data-title=\"(\n|.)*?\")",[{capture,[1]}]) of
		{match,[{_,_}]} -> 
			[{parse_html_tag(H,"data-title=\"","\""),
				parse_html_tag(H,"data-video-id=\"","\"")} | get_elements_from_list(T)];
			
		nomatch -> get_elements_from_list(T)
	end;

get_elements_from_list([])->
	[].

parse_html_tag(Html,Open_tag,Close_tag)->

	case re:run(Html,"(?<="++Open_tag++")(\n|.)*?(?="++Close_tag++")",[{capture,first,list}]) of
		{match,[Content]} ->
			string:strip(re:replace(Content,"\n","",[global,{return,list}]));
		nomatch ->
			erlang:error(nomatch)
	end.
