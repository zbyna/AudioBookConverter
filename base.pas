unit base;

{$mode objfpc}{$H+}
{$MODESWITCH TYPEHELPERS}

interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, TATools, Forms, Controls, Graphics,
  Dialogs, ActnList, ComCtrls, StdCtrls, Buttons,
  EditBtn, ExtCtrls,
  ValEdit,process,LazFileUtils,fpjson,jsonparser,dateutils;

const
  BUF_SIZE = 2048;
  ffmpeg = 'ffmpeg.exe';
  ffprobe = 'ffprobe.exe';

type

  { TTLabeledEditHelper }

  TTLabeledEditHelper = class helper for TLabeledEdit
    function toHHMMSS:String;

  end;

  { TfrmBase }

  TfrmBase = class(TForm)
    btnVideo: TButton;
    btnAudioPuvodni: TButton;
    btnAudioMP3: TButton;
    btnSmazLog: TButton;
    btnExit: TButton;
    FileNameEdit1: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    lblPocetSouboru: TLabel;
    leVelikostSegmentu: TLabeledEdit;
    memLog: TMemo;
    prbUkazatel: TProgressBar;
    vleVlastnosti: TValueListEditor;
    procedure btnAudioMP3Click(Sender: TObject);
    procedure btnAudioPuvodniClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnSmazLogClick(Sender: TObject);
    procedure btnVideoClick(Sender: TObject);
    procedure FileNameEdit1AcceptFileName(Sender: TObject; var Value: String);
    procedure FileNameEdit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure runFFMPEG(exeFile,myParameters:String;progressBegin:Integer);
  private

  public

  end;

var
  frmBase: TfrmBase;

implementation

{$R *.lfm}

{ TTLabeledEditHelper }

function TTLabeledEditHelper.toHHMMSS: String;
begin
  if not(self.Text='') then
       Result:=TimeToStr(incminute( 0,strtoint(self.Text)));
  // great document about TDateTime see:
  // https://www.freepascal.org/~michael/articles/datetime/datetime.pdf
end;

{ TfrmBase }

procedure TfrmBase.FileNameEdit1AcceptFileName(Sender: TObject; var Value: String);
// fired before property DialogFile filled
// intend as validation of user input but uncount more files could be selected
begin
end;

procedure TfrmBase.FileNameEdit1Change(Sender: TObject);
// only here is possible to use property DialogFile
var
  jData:TJSONData;
  jObject:TJSONObject;
  i: Integer;
  progressMax:Integer;
begin
//  vleVlastnosti.Rows[0].Text:='aaaaa'+LineEnding+'bbbbb';
//  vleVlastnosti.Keys[2]:='audio 1';
//  vleVlastnosti.Values['audio 1'] :='value 1';
  progressMax:=0;
  vleVlastnosti.Clear;
  lblPocetSouboru.Caption:='Celkem vybráno souborů: ' + IntToStr(FileNameEdit1.DialogFiles.Count);
  for i:=0 to FileNameEdit1.DialogFiles.Count-1 do
    begin
      memLog.Clear;
      runFFMPEG(ffprobe,'-v quiet, -print_format json, -show_format, -show_streams, '+
                         AnsiQuotedStr(FileNameEdit1.DialogFiles[i],'"'),0);
      // capture json ffmpeg output from memLog component
      jData:=GetJSON(memLog.Text);  // !!! object created it needs to be freed in the end :-)
      jObject:=TJSONObject(jData);

      vleVlastnosti.InsertRow(ExtractFileName(jObject.FindPath('format.filename').AsString),
                              'video: '+jObject.FindPath('streams[0].codec_name').AsString,
                              True);
      vleVlastnosti.InsertRow(ExtractFileName(jObject.FindPath('format.filename').AsString),
                              'audio: '+jObject.FindPath('streams[1].codec_name').AsString,
                              True);
      progressMax:=progressMax+round(jObject.FindPath('format.duration').AsFloat);
      jData.Free;                  // Object cleaned after 5 min debugging :-)
    end;
   prbUkazatel.Max:=progressMax;
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
  for i:=0 to FileNameEdit1.DialogFiles.Count-1 do
    begin
      try
        pomFile:=FileNameEdit1.DialogFiles[i];

        runFFMPEG(ffmpeg,'-progress stats.txt, -i, '+AnsiQuotedStr(pomFile,'"')+
                         ' -c copy, -map 0, -segment_time '+leVelikostSegmentu.toHHMMSS +
                         ', -f segment, -reset_timestamps 1,'+
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

procedure TfrmBase.btnAudioPuvodniClick(Sender: TObject);
var
  pomFile: String;
  i: Integer;
begin
  prbUkazatel.Position:=0;
  for i:=0 to FileNameEdit1.DialogFiles.Count-1 do
    begin
      pomFile:=FileNameEdit1.DialogFiles[i];
      runFFMPEG(ffmpeg,' -progress stats.txt, -i, '+AnsiQuotedStr(pomFile,'"')+
                       ' -vn, -c copy, -map 0, -segment_time '+leVelikostSegmentu.toHHMMSS +
                       ', -f segment, -reset_timestamps 1,'+
                       AnsiQuotedStr(ExtractFilePath(pomFile)+ExtractFileNameOnly(pomFile)+
                                     '_%03d.aac','"'),prbUkazatel.Position);
    end;
end;

procedure TfrmBase.btnAudioMP3Click(Sender: TObject);
var
  pomFile: String;
  i: Integer;
begin
  prbUkazatel.Position:=0;
  for i:=0 to FileNameEdit1.DialogFiles.Count-1 do
    begin
      pomFile:=FileNameEdit1.DialogFiles[i];;
      runFFMPEG(ffmpeg,' -progress stats.txt, -i, '+AnsiQuotedStr(FileNameEdit1.FileName,'"')+
                       ' -vn, -c mp3, -map 0, -segment_time '+leVelikostSegmentu.toHHMMSS +
                       ', -f segment, -reset_timestamps 1,'+
                       AnsiQuotedStr(ExtractFilePath(pomFile)+ExtractFileNameOnly(pomFile)+
                                     '_%03d.mp3','"'),prbUkazatel.Position);
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
    SetLength(pomS, N);
    Move(B, PomS[1], N);
    memLog.Append(pomS);
    // read and "parse" stats.txt
    progresTStrings.LoadFromStream(progresFile);
    pomS2:=progresTStrings.Values['out_time_ms'];
    if not (poms2 = '') then
         prbUkazatel.position:=progressBegin+round(StrToFloat(pomS2)/1e6);
    // application needs to be responsive
    Application.ProcessMessages;
  until (N = 0) or (not AProcess.Running);
  // clean up objects including stats.txt file
  progresFile.Free;
  progresTStrings.Free;
  AProcess.Free;
  DeleteFileUTF8('stats.txt');
end;

procedure TfrmBase.btnExitClick(Sender: TObject);
begin
  frmBase.Close;
end;

procedure TfrmBase.FormCreate(Sender: TObject);
begin
  // fixed file name and its duration for debugging
  //prbUkazatel.Max:=3764;
  //FileNameEdit1.FileName:='i:\Jirka-video-audiobook čárka\Astrid_Lindgrenová_Děti_z_Bullerbynu.mp4';
  vleVlastnosti.ColWidths[0]:=473;
  vleVlastnosti.ColWidths[1]:=100;
end;

procedure TfrmBase.btnSmazLogClick(Sender: TObject);
begin
  memLog.Clear;
  prbUkazatel.Position:=0;
end;


end.

