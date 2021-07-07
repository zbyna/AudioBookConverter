Simple ffmpeg based application for transcoding, audio extraction and splitting of media files.
It provides the possibility to add, delete and update time point(s) where to split using the built-in media player.
Playlist generation (m3u8) is possible too.

## Input:
- one or more audio or video files
- required segment¨s information:
    - one size for constant repeating segment
    - starting points where to split (comma separated) for several different segments
    - possibility to format time values like:
        - ````HH:MM:SS```` or ````MM ````
        - combined both split by comma  ````HH:MM:SS,MM````    
- possibility to generate playlist

## Output:
- media file segments with the needed time length and required format

![](https://i.imgur.com/QbO3Ha6.png)
