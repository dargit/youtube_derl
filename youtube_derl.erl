%  youtube_derl.erl
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
%  
%v.0.2.5

-module(youtube_derl).
-export([download/1, download/4, download/5, get_videoid/1, 
		get_best_format/1, get_available_formats/1,
		test/0]).

-define(DELAY,2000).
-define(TIMEOUT,10000).

-record(formats, {formats = [{"18","mp4","360p H.264/AAC 96kbit/s"},
									{"22","mp4","720p H.264/AAC 192kbit/s"},
									{"82","mp4","360p H.264/AAC 96kbit/s"},
									{"83","mp4","240p H.264/AAC 96kbit/s"},
									{"84","mp4","720p H.264/AAC 192kbit/s"},
									{"85","mp4","1080p H.264/AAC 192kbit/s"},
									{"5","flv","240p H.263/MP3 64kbit/s"},
									{"34","flv","360p H.264/AAC 128kbit/s"},
									{"35","flv","480p H.264/AAC 128kbit/s"}]
						}).

-record(var, {	videoinfo_url 		= "https://www.youtube.com/get_video_info?video_id=",
				playlistinfo_url 	= "https://www.youtube.com/playlist?list=",
				skip 				= 0,
				verbose 			= 0,
				format				= "",
				output_folder 		= "out/",
				playlist_folder		= "",
				formats_priority 	= ["85", "22", "84", "18", "82", 
										"83", "35", "34", "5"],
				type 				= video,
				index				= 1,
				partial_content_range = 0,
				status_code			= 200
				}).

init(Output_folder)->
	inets:start(),
	ssl:start(),

	case file:read_file_info(Output_folder) of
		{error, enoent} ->
			file:make_dir(Output_folder);
		_ -> []
	end.

download(Url)->
	Var = #var{},
	download(Url,Var#var.skip,Var#var.verbose,Var#var.output_folder).

download(Url,Format,Skip,Verbose,Output_folder)->
	init(Output_folder),
	
	case string:rstr(Output_folder,"/") of
		0 -> Output_folder_n = Output_folder++"/";
		_ -> Output_folder_n = Output_folder
	end,
	
	lists:foldl(fun(Y,M)->
		{New_var,Complete_url} = get_complete_url(Y),

		lists:foldl(fun(X,N)->
							Video_info = getvideo_info(X),
							down_request(Video_info,
											Format,
											Complete_url,
											N,
											M,
											length(Url),
											Skip,
											Output_folder_n,
											New_var#var.playlist_folder),		
						N+1 end, 1, Complete_url),
					M+1 end, 1, Url).

download(Url,Skip,Verbose,Output_folder)->
	lists:foldl(fun(Y,M)->
		init(Output_folder),
			
		case string:rstr(Output_folder,"/") of
			0 -> Output_folder_n = Output_folder++"/";
			_ -> Output_folder_n = Output_folder
		end,
		
		{Var, Complete_url} = get_complete_url(Y),

		lists:foldl(fun(X,N)->
							Video_info = getvideo_info(X),
							case get_best_format(Video_info, inside) of
								{_,[{Best_format,_,_}]} ->
									down_request(Video_info,
													Best_format,
													Complete_url,
													N,
													M,
													length(Url),
													Skip,
													Output_folder_n,
													Var#var.playlist_folder);
							
								{F,[]} ->
									down_request(Video_info,
													F,
													Complete_url,
													N,
													M,
													length(Url),
													Skip,
													Output_folder_n,
													Var#var.playlist_folder);								

								{error, Reason} ->
									%only print and continue
									io:format("----------~n~p~n",[Reason])
							end,
							timer:sleep(?DELAY),
						N+1 end, 1, Complete_url),
					M+1 end, 1, Url).

down_request(Video_info,Format,Complete_url,N,M,Lm,Skip,Output_folder,Playlist_folder)->
	case get_download_link(Video_info,Format) of
		{ok,Download_link,{Video_title,Extension,Itag_value}} ->		
			io:format("Current url: ~p/~p~n",[M,Lm]),
			io:format("Current playlist element: ~p/~p~n",[N,length(Complete_url)]),
			output_download(Download_link,
								Video_title,
								Extension,
								Itag_value,
								Skip,
								Output_folder,
								Playlist_folder);
		[] ->
			erlang:error(no_download_url)
	end.

get_videoid(Url)->
	Var = #var{},
	PosVid = re:run(Url,"(?<=v=)((.*?(?=&))|.*)",[{capture,first,list}]),
	PosPl = re:run(Url,"(?<=list=)((.*?(?=&))|.*)",[{capture,first,list}]),
	PosIndex = re:run(Url,"(?<=index=)((.*?(?=&))|.*)",[{capture,first,list}]),
	if
		PosVid =/= nomatch -> %video url passed 
			{match,[T]} = PosVid,
			{videoId,T,Var#var.index};
		PosPl =/= nomatch -> %playlist url passed
			{match,[T]} = PosPl,
			if
				PosIndex =/= nomatch ->
					{match,[I]} = PosIndex,
					{I_Integer,_} = string:to_integer(I),
					{playlistId,T,I_Integer};
				true ->
					{playlistId,T,Var#var.index}
			end;
		true -> %only video id passed, or invalid url/video id
			{videoId,Url,Var#var.index}
	end.

get_complete_url(Url)->
	Var = #var{},
	{Type,U,Index} = get_videoid(Url),
	Videoinfo_url = Var#var.videoinfo_url,
	Playlistinfo_url = Var#var.playlistinfo_url,
	case Type of
		videoId ->
			{Var,[Videoinfo_url ++ U]};
		playlistId ->
			CompletePlaylist_url = Playlistinfo_url ++ U,
			{Playlist_title,Playlist_links_list} = youtube_derl_pl:get_pl_data(CompletePlaylist_url),
			New_var = Var#var{playlist_folder = Playlist_title}, 
			io:format("~n*****~nDonwnload playlist: ~p - (~p elements)~n*****",[Playlist_title,length(Playlist_links_list)]),
			if
				Index > Var#var.index ->
					io:format("~nindex is present, starting from element: ~p~n",[Index]),
					{New_var,get_playlist_elements_complete_url(Videoinfo_url,lists:sublist(Playlist_links_list,
																					Index,length(Playlist_links_list)))};
				true ->
					{New_var,get_playlist_elements_complete_url(Videoinfo_url,Playlist_links_list)}
			end	
	end.

get_playlist_elements_complete_url(Videoinfo_url,[H|T])->
	{_,I} = H,
	[Videoinfo_url++I | get_playlist_elements_complete_url(Videoinfo_url,T)];

get_playlist_elements_complete_url(_,[])->
	[].
	
getvideo_info(Complete_url)->
	%get Body
	case httpc:request(Complete_url) of
		{ok,{{_,200,_},_,Body}} ->
			Body;
		{error, Reason} ->
			save_log(Reason, "No video info data"),	
			erlang:error(Reason),
			exit(1)
	end.

get_title(Video_info)->
	case re:run(Video_info,"(?<=title=)(.*?(?=&)|.*)",[{capture,first,list}]) of
		{_,[Content]} ->
			re:replace(http_uri:decode(http_uri:decode(Content)), 
								"\\+", " ", [global, {return, list}]);
		nomatch ->
			Reason = "Error retrieving video title", 
			save_log(Reason, Video_info),
			{error,Reason}
	end.
	
get_links_list(Video_info)->
	[_|Links_list] = re:split(Video_info,"%2Curl%3D|%26url%3D|=url%3D",[{return,list},trim]),
	Links_list.

get_download_link(Video_info,Selected_format)->
	%get video title
	Video_title = get_title(Video_info),
	io:format("~n----------~nVideo title: ~p~n",[Video_title]),
	
	Links_list = get_links_list(Video_info),
	
	%get the selected format link
	Selected_list = get_download_link_from_list(Links_list,Selected_format),
	Var = re:split(Selected_list,"%26",[{return,list},trim]),

	Link_encoded = lists:flatten(lists:filter(fun(X) -> string:str(X,"http")>0 end, Var)),

	Download_link1 = http_uri:decode(http_uri:decode(Link_encoded)),
	Download_link2 = string:sub_string(Download_link1,
										string:str(Download_link1,"http"),
										length(Download_link1)),
	%filter eventually "," char and other things after
	{match, [Download_link_final]} = re:run(Download_link2,"((.*?(?=,))|.*)",[{capture,first,list}]),
	
	F = get_format_info(Selected_format),
	if 
		F =/= [] ->
			[{Itag_value,Extension,Description}] = F;
		true -> %try to get the extension of unknown files from the header
			Headers = get_format_info_from_headers(Download_link_final),
			[{Itag_value,Extension,Description}] = [{Selected_format,
													re:run(proplists:get_value("content-type",Headers),
															"(?<=/)(.*?(?=;)|.*)",
															[{capture,first,list}]),
													[]}]
	end,
																			
	{ok,Download_link_final,{Video_title,Extension,Itag_value}}.

get_download_link_from_list([H|T],Format)->
	M = re:run(H,"(((%2526|%253F)itag%253D)" ++ Format ++ "(%2526|%26|%2C))",[{capture,[1]}]),
	lists:flatten([	if
						M =/= nomatch ->
							H;
						true ->
							get_download_link_from_list(T,Format)
						end
					]);

get_download_link_from_list([],_)->
	[].

output_download(Url,Title,Extension,Itag_value,Skip,Default_output_folder,Playlist_output_folder)->
	Title_Formatted = re:replace(Title, "/", " ", [global, {return, list}]),
	
	case file:read_file_info(Default_output_folder++Playlist_output_folder) of
		{error, enoent} ->
			file:make_dir(Default_output_folder++Playlist_output_folder);
		_ -> []
	end,	

	Filename = Default_output_folder++
				Playlist_output_folder++"/"++
				Title_Formatted ++ 
				"-(" ++ Itag_value ++ ")" ++ 
				"." ++ Extension,
	New_filename = set_filename(Filename,1),	

	if
		(Filename =/= New_filename andalso Skip == 1) ->
			{ok, io:format("File ~p already exists, skipped~n", [Filename])};	
		true ->
			case httpc:request(get,{Url,[]},[],[{sync, false},{stream, self}]) of
				{ok, RequestId} ->
					down(RequestId,Filename,null,0,0,0,0);	
				{error,Reason} ->
					erlang:error(Reason)
			end
	end.

down(RequestId,Filename,FileDescriptor,N,Downloaded,C_length,Previous_perc) ->
	receive
		{http,{RequestId, stream_start, Headers}} ->
			{Content_length,_} = string:to_integer(proplists:get_value("content-length", Headers)),	
	
			io:format("Output file: ~p~n", [Filename]),
			io:format("Download size: ~pb~n", [Content_length]),
			io:format("Downloading...~n"),

			case file:open(Filename, [write, raw, append, delayed_write]) of
				{ok, Fd} ->
					down(RequestId,Filename,Fd,1,0,Content_length,0);
				{error, "Failed to save file."} ->
					exit(1)
			end;

		{http, {RequestId, stream, BinBodyPart}} ->
			file:write(FileDescriptor, BinBodyPart),
			
			Current_part_size = length(binary:bin_to_list(BinBodyPart)),
			Current_downloaded_value = Downloaded + Current_part_size,
			Current_perc = (Current_downloaded_value / C_length) * 100, 
 
			if
				(Current_perc - round(Previous_perc) >= 1) ->
					{ok,C} = io:columns(),
					K = lists:seq(1,100),		
					if 
						(C rem 2) == 1 -> C2 = C-1; 
						true -> C2 = C
					end, 
					io:format("\r\tProgress: ~p% [~s~s] ",[trunc(Current_perc),lists:flatten(lists:duplicate(trunc(Current_perc/100*(C2/2)),"=")),
																lists:flatten(lists:duplicate(trunc((length(K)-Current_perc)/100*(C2/2))," "))]),		
					
					down(RequestId,Filename,FileDescriptor,
							N+1,Current_downloaded_value,
							C_length,Current_perc);
				true ->
					down(RequestId,Filename,FileDescriptor,
							N+1,Current_downloaded_value,
							C_length,Previous_perc)
			end;
			
		{http, {RequestId, stream_end, _}} ->
			Current_downloaded_value = Downloaded,
			Current_perc = (Current_downloaded_value / C_length) * 100,
			if 
				Current_perc < 100 ->
					io:format("~nSorry there was an error during download, check your internet connection and try again.~nThe chunk will be deleted.~n"),
					file:close(FileDescriptor),	
					%delete the file
					file:delete(Filename),
					save_log("Timeout ", Filename),
					{error, download_error};				
				true ->
					io:format("~nDone.~n"),
					file:close(FileDescriptor),
					{ok, download_done}
			end		
		after ?TIMEOUT ->
			file:close(FileDescriptor),	
			io:format("~nSorry timeout, check your internet connection and try again.~nThe chunk will be deleted.~n"),
			%delete the file
			file:delete(Filename),
			save_log("Timeout ", Filename),
			{error, timeout}
	end.

set_filename(Filename,N) ->
	case file:read_file_info(Filename) of
		{ok, _} ->
			%check the presence of "_n" at the end of the file
			if 
				N=<1 ->
					New_filename = filename:rootname(Filename) ++ "_" ++
									integer_to_list(N) ++ 
									filename:extension(Filename),
					set_filename(New_filename,N+1);
				true ->
					Filename_rootName = filename:rootname(Filename),
					New_filename = string:sub_string(Filename_rootName,1,
													string:str(Filename_rootName,"_")) ++ 
									integer_to_list(N) ++ 
									filename:extension(Filename),
					set_filename(New_filename,N+1)
			end;				
		{error, enoent} ->
			Filename
	end.

get_best_format(Url)->
	init(#var.output_folder),
	{_,[Complete_url]} = get_complete_url(Url),
	get_best_format(getvideo_info(Complete_url),inside).

get_best_format(Video_info, inside)->
	Var = #var{},
	Formats_priority = Var#var.formats_priority,
	Formats_available = get_available_formats(
							get_links_list(Video_info),inside),

	if
		Formats_available =/= [] ->	
			F = (Formats_priority -- (Formats_priority -- Formats_available)),
			if
				F =/= [] ->
					[H|_] = F;
				true ->
					H = lists:max(Formats_available)
			end,
			Format_info = get_format_info(H),
			%if get_format_info() returns [] there aren't valid video formats 
			if
				Format_info =/= [] ->
					{H,Format_info};
				true -> %TODO i need to get the file info from the header instead
					{H,Format_info}
			end;
		true ->
			Error_code = get_errors_from_video_info(Video_info,"errorcode"),
			Reason_from_video_info = get_errors_from_video_info(Video_info,"reason"),
			Reason = "No available formats found, Error-code:"++Error_code++" - "++Reason_from_video_info,
			save_log(Reason, Video_info),
			{error,Reason}
	end.

get_available_formats(Url)->
	init(#var.output_folder),
	{_,[Complete_url]} = get_complete_url(Url),
	get_available_formats(get_links_list(getvideo_info(Complete_url)),inside).
	
get_available_formats([H|T],inside)->
	M = re:run(H,"(?<=(%2526|%253F)itag%253D)[0-9]+(?=(%2526|%26|%2C))",[{capture,first,list}]),
	if
		M =/= nomatch ->
			{match,[Content]} = M,
			[ Content | get_available_formats(T,inside)];
		true ->
			get_available_formats(T,inside)
	end;
	

get_available_formats([],inside)->
	[].

get_format_info(Itag_value)->
	Formats = #formats{},
	Formats_list = Formats#formats.formats,
	[Ls || {N,_,_}=Ls <- Formats_list , N == Itag_value ].
	
	
get_format_info_from_headers(Url)->
		case httpc:request(head,{Url,[]},[],[]) of
			{ok,{{_,200,_},Headers,_}} ->
				Headers;	
			{error,Reason} -> {error,Reason}
		end.

get_errors_from_video_info(Video_info,Value)->
	case re:run(Video_info,"(?<="++Value++"=)(.*?(?=&)|.*)",[{capture,first,list}]) of
		{match,[Content]} ->
			Content;
		nomatch-> []
	end.

save_log(Reason, Video_info)->
	{ok, Fd} = file:open("error_log", [write, append]),
	file:write(Fd,lists:flatten(io_lib:format("~p", [erlang:localtime()]))++
					lists:flatten(lists:duplicate(50, "-"))++
					"\n"++Reason++"\n\n"++
					Video_info++"\n\n"),
	file:close(Fd).	

%test function, only for DEBUG
test()->
	Url = ["INSERT_URL_HERE"],

	io:format("~n***START TESTING***~n~p~n",[get_available_formats(Url)]),
	io:format("~p~n------------~n",[get_best_format(Url)]),
	Var = #var{skip = 1},
	spawn(?MODULE, download, [Url,
								Var#var.skip,
								Var#var.verbose,
								Var#var.output_folder]).
