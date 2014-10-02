youtube_derl
============

simple youtube downloader in Erlang

Copyright 2014 Dario Conti

<b>README</b>

Still in development, some features are present for helping in future modifications.<br>
Only for educational purpose.<br>
Use at your own risk, the author is not responsible for any damages that may result from direct or indirect use of this software.<br>

<b>USAGE:</b>

youtube_derl:download([URL]).<br>
youtube_derl:download([URL1,URL2,...,URLn]).<br>
youtube_derl:download([URL,Skip,Verbose,Output_folder]).<br>
youtube_derl:download([URL,FORMAT_CODE,Skip,Verbose,Output_folder]).

URL = string (eg. "https://www.youtube.com/watch?v=VIDEO_CODE")<br>
URL = string (eg. "https://www.youtube.com/playlist?list=PLAYLIST_CODE")<br>
URL = string (eg. "https://www.youtube.com/playlist?list=PLAYLIST_CODE&index=N_INTEGER")<br>
Skip = integer()	0,1<br>
Verbose = integer()	0,1<br>
Output_folder -> string (eg. "out/")<br>
FORMAT_CODE = string (eg. "22")

youtube_derl:get_videoid(URL) -> {videoId, VIDEO_ID, INDEX} | {playlistId, PLAYLIST_ID, INDEX}

<b>ONLY FOR SINGLE VIDEO NOT FOR PLAYLIST URL</b><br>
youtube_derl:get_best_format(URL) -> {FORMAT_CODE,[{FORMAT_CODE,FORMAT_EXTENSION,FORMAT_DESCRIPTION}]}<br>

youtube_derl:get_available_formats(URL) -> [FORMAT_CODE]

