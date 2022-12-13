unit base;

{$mode objfpc}{$H+}
{$MODESWITCH TYPEHELPERS}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, TATools, Forms, Controls, Graphics,
  Dialogs, ActnList, ComCtrls, StdCtrls, Buttons, EditBtn, ExtCtrls, windows,
  ValEdit, process, LazFileUtils, fpjson, jsonparser, dateutils, localizedforms,
  DefaultTranslator, Grids, strutils, playerform
  // FPC 3.0 fileinfo reads exe resources as long as you register the appropriate units
  , fileinfo
  , winpeimagereader {need this for reading exe info}
  , Generics.Collections;


const
  BUF_SIZE = 2048;
  ffmpeg = 'ffmpeg.exe';
  ffprobe = 'ffprobe.exe';

type

  { TTLabeledEditHelper }

  TTLabeledEditHelper = class helper for TLabeledEdit
    function toHHMMSS:String;

  end;

  { TTJSONDataHelper }

  TTJSONDataHelper = class helper for TJSONData
    function FindPathDef(const APath: TJSONStringType; defaultValue:String = '"None"'):TJSONData;
  end;

  { TFileChaptersItem}

  TFileChaptersItem = specialize TObjectDictionary <String, TStringList >;  

  { TFilesChapters}

  TFilesChapters = specialize TObjectList<TFileChaptersItem>;


  { TSegmentInfoBackup }

  TSegmentInfoBackup = class(TComponent) // backup leVelikostSegmentu and radGrSegment
    public
    segRadioGr : Byte;
    segSize: String;
    procedure BackupSegmentInfo();
    procedure ClearBackup();
    procedure RestoreBackup();
  end;
  
  { TfrmBase }

  TfrmBase = class(TLocalizedForm)
    btnVideo: TButton;
    btnAudioPuvodni: TButton;
    btnAudioMP3: TButton;
    btnSmazLog: TButton;
    btnExit: TButton;
    btnPlay: TButton;
    chcbPlaylist: TCheckBox;
    chcbWithoutSplit: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    ImageList1: TImageList;
    lblPocetSouboruCislo: TLabel;
    lblPocetSouboru: TLabel;
    leVelikostSegmentu: TLabeledEdit;
    memLog: TMemo;
    OpenDialog1: TOpenDialog;
    prbUkazatel: TProgressBar;
    radGrSegment: TRadioGroup;
    SpeedButton1: TSpeedButton;
    stgVlastnosti: TStringGrid;
    procedure btnAudioMP3Click(Sender: TObject);
    procedure btnAudioPuvodniClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnSmazLogClick(Sender: TObject);
    procedure btnVideoClick(Sender: TObject);
    procedure chcbWithoutSplitChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure leVelikostSegmentuEditingDone(Sender: TObject);
    procedure runFFMPEG(exeFile,myParameters:String;progressBegin:Integer);
    procedure FillGridFromFiles;
    procedure SpeedButton1Click(Sender: TObject);
  private

  public
    filesChapters : TFilesChapters;
    segInfoBck :TSegmentInfoBackup;
    procedure HowToSplit(i:integer);
    procedure UpdateTranslation(ALang: String); override;
  end;

 resourcestring
  rsVelikost = 'Velikost';
  rsZaTek = 'Začátek';
  rsJmNo = 'Jméno';
  rsKodek = 'Kodek';
  rsChapters = 'Kapitoly';
  rsUseChapters = 'Použít kapitoly';

var
  frmBase: TfrmBase;


implementation

function  pomSegment: String;
begin
  case frmBase.radGrSegment.ItemIndex of
           0 : Result := '-segment_time ' ;
           1 : Result := '-segment_times ';
          end;
end;

function pomSegmentList(pomFile:String):String;
begin
 // ' , -segment_list out.m3u8'
  if  not frmBase.chcbPlaylist.Checked then
      Result := ''
  else
      Result:= ' , -segment_list ' + AnsiQuotedStr(ExtractFilePath(pomFile)+
                                                    ExtractFileNameOnly(pomFile) + '.m3u8','"');
end;

{$R *.lfm}

{ TSegmentInfoBackup }

procedure TSegmentInfoBackup.BackupSegmentInfo();
begin
  self.segRadioGr:= frmBase.radGrSegment.ItemIndex ;
  //frmBase.memLog.Append(frmBase.radGrSegment.Items[frmBase.radGrSegment.ItemIndex]);
  self.segSize:= frmBase.leVelikostSegmentu.Caption;
  //frmBase.memLog.Append(self.segSize);
end;

procedure TSegmentInfoBackup.ClearBackup();
begin
  self.segRadioGr:=0;
  self.segSize:='';
end;

procedure TSegmentInfoBackup.RestoreBackup;
begin
  frmBase.radGrSegment.ItemIndex := self.segRadioGr;
  frmBase.leVelikostSegmentu.Caption := self.segSize;
end;

{ TTJSONDataHelper }

function TTJSONDataHelper.FindPathDef(const APath: TJSONStringType;
                                      defaultValue: String = '"None"' ): TJSONData;
var
  pomData: TJSONData;
begin
  pomData := self.FindPath(APath);
  if pomData = Nil then
      Result := GetJSON(defaultValue)
  else
      Result := pomData;
end;

{ TTLabeledEditHelper }

function TTLabeledEditHelper.toHHMMSS: String;
var
  pomI, i: Integer;
  pomS: String;
begin
  Result := '';
  pomI:= 0;
  if not(self.Text='') then
     pomI := WordCount(self.Text,[',']);
     begin
       if pomI = 0 then
          // ffmpeg option segment_time X  - cuts file to segments with the same length X
          Result:=TimeToStr(incminute( 0,strtoint(self.Text)))
       else
           begin
             // ffmpeg option segment_times X,Y,Z  - cuts file in time X, Y, Z  see: time vs times :-)
             for i:=1 to pomI do
               begin
                 pomS := ExtractWord(i,self.Text,[',']);
                 if RPos(':', pomS) > 0 then
                    begin
                      Result := Result + ',' + pomS;
                    end
                 else
                   begin
                     Result := Result + ',' + TimeToStr(incminute( 0,strtoint(pomS)));
                   end;
               end;
             // must be used double quotes "00:10:00,00:30:00" due to command line parameters separated
             // already with commas see:
             // http://www.ffmpeg-archive.org/Alternative-options-for-comma-separated-filters-td4666298.html
             // and https://stackoverflow.com/questions/5230166/how-can-i-launch-a-folder-whose-name-contains-a-comma-using-processstartinfo-in/5230195#5230195
             Result:= TrimSet(Result,[',']);
             Result:= '"' + Result +  '"';
           end;
  // great document about TDateTime see:
  // https://www.freepascal.org/~michael/articles/datetime/datetime.pdf
     end;
end;

{ TfrmBase }

procedure TfrmBase.leVelikostSegmentuEditingDone(Sender: TObject);
begin
   if (radGrSegment.ItemIndex = 0) and AnsiContainsStr(leVelikostSegmentu.text,',') then
        begin
           ShowMessage('Only first value is accepted!!!');
           leVelikostSegmentu.Text:= ExtractWord(1,leVelikostSegmentu.Text,[',']);
        end;
end;

procedure TfrmBase.FillGridFromFiles;
var
  jData:TJSONData;
  jObject:TJSONObject;
  i,j: Integer;
  progressMax:Integer;
  streamsInf : String;
  internalChapters: TStringList;
  fileChaptersItem : TFileChaptersItem;
  userChapters :TStringList;
  chaptersCount : Word;
begin
  progressMax:=0;
  stgVlastnosti.ClearRows; // clear all rows (fixed too)
  stgVlastnosti.RowCount:=1; // rows are counted from 0
  stgVlastnosti.FixedRows:=1;
  lblPocetSouboruCislo.Caption := IntToStr(OpenDialog1.Files.Count);
  filesChapters.Clear;
  for i:=0 to OpenDialog1.Files.Count-1 do
    begin
      memLog.Clear;
      runFFMPEG(ffprobe,'-v quiet, -print_format json, -show_format, -show_streams, -show_chapters '+
                         AnsiQuotedStr(OpenDialog1.Files[i],'"'),0);
      // capture json ffmpeg output from memLog component
      jData:=GetJSON(memLog.Text);  // !!! object created it needs to be freed in the end :-)
      jObject:=TJSONObject(jData);
      userChapters := TStringList.Create(True);
      internalChapters := TStringList.Create(True);
      chaptersCount := jObject.FindPath('chapters').Count;
      if  chaptersCount <> 0 then
      // file has internal chapters
      begin
        chaptersCount := jObject.FindPath('chapters').Count;
        // starting from 1 not 0 b/c starting time of 1st chapter is zero
        for j:=1 to chaptersCount-1 do
        begin
          internalChapters.Add(
             FormatDateTime('h:nnn:ss',
                jObject.FindPath('chapters').Items[j].FindPathDef('start_time').AsFloat / (24 * 60 * 60)) )
        end;
        internalChapters.Delimiter:= ',';
        //memLog.Append(internalChapters.DelimitedText);
        //leVelikostSegmentu.Caption:= internalChapters.DelimitedText;
      end;
      userChapters.Delimiter:= ',';
      fileChaptersItem := TFileChaptersItem.Create([doOwnsValues]);
      fileChaptersItem.add('internal',internalChapters);
      fileChaptersItem.add('user',userChapters);
      filesChapters.Add(fileChaptersItem);
      //memLog.Append('User chapters count: '+IntToStr(userChapters.Count));
      //memLog.Append('Internal chapters count: '+IntToStr(internalChapters.Count));
      //memLog.Append(IntToStr(filesChapters[i]['internal'].Count));
      //memlog.Append(intTostr(filesChapters.Items[i]['user'].Count)) ;
      //internalChapters.Free; - TObjectDictionary manages memory of its items automatically
      streamsInf:= '';
      for j:=0 to jObject.FindPath('streams').Count-1 do
      begin
        streamsInf:= streamsInf + (jObject.FindPath('streams').Items[j].FindPathDef('codec_type').AsString +
                     ':'+ jObject.FindPath('streams').Items[j].FindPathDef('codec_name').AsString + ' ');
      end;
      progressMax:=progressMax+round(jObject.FindPath('format.duration').AsFloat);
      stgVlastnosti.InsertRowWithValues( i+1,
                                 [ ExtractFileName(jObject.FindPath('format.filename').AsString),
                                   streamsInf,IntToStr(chaptersCount),'false' ] );
      jData.Free;                  // Object cleaned after 5 min debugging :-)
    end;
   prbUkazatel.Max:=progressMax;
   stgVlastnosti.AutoSizeColumns();
   // enable button for player launching
   btnPlay.Enabled:=True;
end;

procedure TfrmBase.HowToSplit(i: integer);
begin
  // file has user defined chapters - by movie player
  if filesChapters.Items[i]['user'].Count > 0 then
     begin
       leVelikostSegmentu.Caption:= filesChapters.Items[i]['user'].DelimitedText;
       radGrSegment.ItemIndex := 1 ;
     end
  // file has internal chapters and  using them is checked in 4th grid column
  else if (filesChapters.Items[i]['internal'].count > 0) and (stgVlastnosti.Cells[3,i+1] = '1')   then
     begin
       leVelikostSegmentu.Caption:= filesChapters.Items[i]['internal'].DelimitedText;
       radGrSegment.ItemIndex := 1 ;
     end
  else
  // use no chapter but split setting from leVelikostSegmentu and radGrSegment
    begin
      segInfoBck.RestoreBackup();
    end;
end;

procedure TfrmBase.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then FillGridFromFiles;
end;

procedure TfrmBase.btnVideoClick(Sender: TObject);
// ExtractFileDir(pomFile)          i:\Jirka-video-audiobook čárka
// ExtractFileNameOnly(pomFile)     Astrid_Lindgrenová_Děti_z_Bullerbynu
// ExtractFileName(pomFile)         Astrid_Lindgrenová_Děti_z_Bullerbynu.mp4
// ExtractFileExt(pomFile)          .mp4
// ExtractFilePath(pomFile)         i:\Jirka-video-audiobook čárka\
var
  pomFile:String;
  i: Integer;
begin
  prbUkazatel.Position:=0;
  for i:=0 to OpenDialog1.Files.Count-1 do
    begin
      try
       // segment_time vs segment_times see:  leVelikostSegmentu.toHHMMSS
        pomFile:=OpenDialog1.Files[i];
        memLog.Append('velikost segmentu: ' + leVelikostSegmentu.toHHMMSS );
        runFFMPEG(ffmpeg,'-progress stats.txt, -i, '+AnsiQuotedStr(pomFile,'"')+
                         ' -c copy, -map 0, ' + pomSegment + leVelikostSegmentu.toHHMMSS +
                         pomSegmentList(pomFile) + ', -f segment, -reset_timestamps 1,'+
                         AnsiQuotedStr(ExtractFilePath(pomFile)+ExtractFileNameOnly(pomFile)+
                                       '_%03d.mp4','"'),prbUkazatel.Position);
      except
        on E:Exception do
          begin
            memLog.Append('Chyba: ' + e.Message);
            exit;
          end;
      end;
    end;
  memLog.Append('HOTOVO :-)');
end;

procedure TfrmBase.chcbWithoutSplitChange(Sender: TObject);
begin
   radGrSegment.Enabled:= not radGrSegment.Enabled;
   leVelikostSegmentu.Enabled := not leVelikostSegmentu.Enabled;
   chcbPlaylist.Enabled :=  not chcbPlaylist.Enabled;
   // btnPlay can be enabled outside this procedure
   if (sender as TCheckBox).Checked or (stgVlastnosti.Cells[0,1].IsEmpty)  then
      btnPlay.Enabled:= False
   else
       btnPlay.Enabled:= True;
end;

procedure TfrmBase.btnAudioPuvodniClick(Sender: TObject);
var
  pomFile: String;
  i: Integer;
begin
  prbUkazatel.Position:=0;
  segInfoBck.BackupSegmentInfo(); // backup leVelikostSegmentu and radGrSegment;
  for i:=0 to OpenDialog1.Files.Count-1 do
    begin
      pomFile:=OpenDialog1.Files[i];
      if not chcbWithoutSplit.Checked then
         begin
            HowToSplit(i); // decide what to use for splitting
            runFFMPEG(ffmpeg,' -progress stats.txt, -i, '+AnsiQuotedStr(pomFile,'"')+
                       ' -vn, -c copy, -map 0, ' + pomSegment +leVelikostSegmentu.toHHMMSS +
                       pomSegmentList(pomFile) + ', -f segment, -reset_timestamps 1,'+
                       AnsiQuotedStr(ExtractFilePath(pomFile)+ExtractFileNameOnly(pomFile)+
                                     '_%03d.aac','"'),prbUkazatel.Position);
           segInfoBck.RestoreBackup(); // restore leVelikostSegmentu and radGrSegment backup
         end
      else
          begin
             runFFMPEG(ffmpeg,' -progress stats.txt, -i, '+AnsiQuotedStr(pomFile,'"')+
                       ' -vn, -c copy, -map 0, '+
                       AnsiQuotedStr(ExtractFilePath(pomFile)+ExtractFileNameOnly(pomFile)+
                                     '.aac','"'),prbUkazatel.Position);
          end;

    end;
end;

procedure TfrmBase.btnAudioMP3Click(Sender: TObject);
var
  pomFile: String;
  i: Integer;
begin
  prbUkazatel.Position:=0;
  segInfoBck.BackupSegmentInfo(); // backup leVelikostSegmentu and radGrSegment;
  for i:=0 to OpenDialog1.Files.Count-1 do
    begin
      pomFile:=OpenDialog1.Files[i];
      if not chcbWithoutSplit.Checked then
         begin
            HowToSplit(i); // decide what to use for splitting
            runFFMPEG(ffmpeg,' -progress stats.txt, -i, '+AnsiQuotedStr(pomFile,'"')+
                             ' -vn, -c mp3, -map a, ' + pomSegment + leVelikostSegmentu.toHHMMSS +
                             pomSegmentList(pomFile) + ', -f segment, -reset_timestamps 1,'+
                             AnsiQuotedStr(ExtractFilePath(pomFile)+ExtractFileNameOnly(pomFile)+
                                           '_%03d.mp3','"'),prbUkazatel.Position);
           segInfoBck.RestoreBackup(); // restore leVelikostSegmentu and radGrSegment backup
         end
      else
          begin
            runFFMPEG(ffmpeg,' -progress stats.txt, -i, '+AnsiQuotedStr(pomFile,'"')+
                             ' -vn, -c mp3, -map a, ' +
                             AnsiQuotedStr(ExtractFilePath(pomFile)+ExtractFileNameOnly(pomFile)+
                                           '.mp3','"'),prbUkazatel.Position);
          end;
    end;
end;

procedure TfrmBase.runFFMPEG(exeFile,myParameters:String;progressBegin:Integer);
var
  AProcess     : TProcess;
  B: array[0..BUF_SIZE] of Char;
  N: Integer;
  pomS: AnsiString;
  progresFile:TFileStream;
  progresTStrings:TStringList;
  pomS2: String;
begin
  AProcess := TProcess.Create(nil);
  // setup ffmpeg process (Tprocess) parametres
  AProcess.Executable := 'exes\'+exeFile;
  AProcess.Parameters.Delimiter:=',';
  AProcess.Parameters.DelimitedText:=myParameters;
  AProcess.Options := [poUsePipes,poStderrToOutPut,poNoConsole];
  // run ffmpeg
  AProcess.Execute;
  // create objects needed for progress displaying
    // stat.txt is written by ffmpeg process see: -progress stats.txt
  progresFile:=TFileStream.Create('stats.txt',fmCreate or fmShareDenyNone);
    // TStringList needed for simple processing key=value lines in stats.txt
  progresTStrings:=TStringList.Create;
  // repeat until ffmpeg process is finished
  repeat
    // copy ffmpeg process output to memLog component
    N := AProcess.Output.Read(B, BUF_SIZE);
    if N <> 0 then
         begin
           SetLength(pomS, N);
           Move(B, PomS[1], N);
           memLog.Text:= memLog.Text + pomS;
           SendMessage(memLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
         end;
    // read and "parse" stats.txt
    progresTStrings.LoadFromStream(progresFile);
    pomS2:=progresTStrings.Values['out_time_ms'];
    if not (poms2 = '') then
         prbUkazatel.position:=progressBegin+round(StrToFloat(pomS2)/1e6);
    // application needs to be responsive
    Application.ProcessMessages;
  until (N = 0) and (not AProcess.Running);
  // clean up objects including stats.txt file
  progresFile.Free;
  progresTStrings.Free;
  AProcess.Free;
  DeleteFileUTF8('stats.txt');
end;

procedure TfrmBase.UpdateTranslation(ALang: String);
begin
  inherited UpdateTranslation(ALang);
  lblPocetSouboru.Refresh;
end;

procedure TfrmBase.btnExitClick(Sender: TObject);
begin
  frmBase.Close;
end;

procedure TfrmBase.btnPlayClick(Sender: TObject);
var
  modResultFrmPlayer: Integer;
begin
  frmPlayer := TfrmPlayer.Create(Self);
  frmPlayer.Caption:= stgVlastnosti.Cells[0,stgVlastnosti.Row];
  // file to play
  frmPlayer.MPlayer.Filename:= OpenDialog1.Files[stgVlastnosti.Row-1];
  // values will be time points
  radGrSegment.ItemIndex := 1;
  modResultFrmPlayer := frmPlayer.ShowModal;
  if modResultFrmPlayer = mrOK then
       begin
         leVelikostSegmentu.Caption:= frmPlayer.timePointsToString();
         memLog.Append('Added time points from player.');
       end
  else
      begin
         memLog.Append('Time points from player cancelled.');
      end;
  FreeAndNil(frmPlayer);

end;

procedure TfrmBase.FormCreate(Sender: TObject);
var
  FileVerInfo: TFileVersionInfo;
begin
  // fixed file name and its duration for debugging
  //prbUkazatel.Max:=3764;
  //OpenDialog1.FileName:='i:\Jirka-video-audiobook čárka\Astrid_Lindgrenová_Děti_z_Bullerbynu.mp4';
  radGrSegment.Items[0] := rsVelikost;
  radGrSegment.Items[1] := rsZaTek;
  stgVlastnosti.Columns.Items[0].Title.Caption := rsJmNo ;
  stgVlastnosti.Columns.Items[1].Title.Caption := rsKodek;
  stgVlastnosti.Columns.Items[2].Title.Caption := rsChapters ;
  stgVlastnosti.Columns.Items[3].Title.Caption := rsUseChapters;
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    frmBase.Caption:=frmBase.Caption +
         format(' %s',[FileVerInfo.VersionStrings.Values['FileVersion'].Substring(0,5)]);
  finally
    FileVerInfo.Free;
  end;
  memLog.MaxLength:=0;
  stgVlastnosti.ColWidths[0]:=473;
  stgVlastnosti.ColWidths[1]:=100;
  radGrSegment.ItemIndex:=0;
  btnPlay.Enabled:=False;
  stgVlastnosti.SelectedColor:= clHighlight;
  stgVlastnosti.FocusColor:= clDefault;
  filesChapters := TFilesChapters.Create(True);
  segInfoBck := TSegmentInfoBackup.Create(Self);
  segInfoBck.BackupSegmentInfo();
end;

procedure TfrmBase.FormDestroy(Sender: TObject);
begin
  filesChapters.Free;
end;

procedure TfrmBase.btnSmazLogClick(Sender: TObject);
begin
  memLog.Clear;
  prbUkazatel.Position:=0;
end;


end.

