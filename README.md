![image](https://github.com/zbyna/AudioBookConverter/assets/3373705/508f46f7-f32d-47ee-94ed-a3a6d4dcf26d)


Simple ffmpeg based application for transcoding, audio extraction and splitting of media files.

It provides the possibility:
- to add, delete and update time point(s) where to split manually or using the built-in media player
- to create user defined output part names
- to use input file internal chapters names and their timecodes as a base for custom splitting    
- playlist generation (m3u8).
  
## Input:
### - one or more audio or video files
### - required time segments´ information:
   #### 1. Manually
       1. time
          - one size for constant repeating segment
          - starting points where to split (comma separated) for several different segments
          - possibility to format time values like:
              - HH:MM:SS or MM
              - combined both split by comma  HH:MM:SS,MM
      2. name
          - generic file name_00x will be used

   
   ![image](https://github.com/zbyna/AudioBookConverter/assets/3373705/6db5ece4-ea12-42ae-a422-d950df6b6b07)

   #### 2. using input file internal chapters
      - enabling flag Use chapters in the 4th row of Input table
      - internal chapters time codes and names will be used for output

   ![image](https://github.com/zbyna/AudioBookConverter/assets/3373705/39ecee18-b38e-4a5c-ae9e-0087356ae8e1)

   #### 3. using built-in media player
      1. time
        - from time slider position using  Add command
      2. name
        - user defined text in a row next to the time point

   ![image](https://github.com/zbyna/AudioBookConverter/assets/3373705/ed930604-20a1-4783-afa2-dd04faea2c07)

    
### - possibility to generate playlist
![image](https://github.com/zbyna/AudioBookConverter/assets/3373705/44632408-1061-4548-aeb0-ebc6d8f0270b)


## Output:
### - media file segments with the needed time length and required format


