unit playerform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, ActnList, Menus, RTTICtrls,
  MPlayerCtrl,localizedforms,DefaultTranslator, Buttons,LazUTF8
  ,AnchorDocking
  ,main
  ,epiktimer
  ,FileInfo;

type

  { TfrmPlayer }

  TfrmPlayer = class(TLocalizedForm)
    aclAkce: TActionList;
    acPlay: TAction;
    acStop: TAction;
    acPause: TAction;
    acAdd: TAction;
    acClearList: TAction;
    acImportChapters: TAction;
    acUpdate: TAction;
    acDelete: TAction;
    btnAdd: TSpeedButton;
    btnClearList: TSpeedButton;
    btnDelete: TSpeedButton;
    btnPause: TSpeedButton;
    btnPlay: TSpeedButton;
    btnStop: TSpeedButton;
    btnUpdate: TSpeedButton;
    etCustomTimer: TEpikTimer;
    ImageList1: TImageList;
    lblTime: TLabel;
    lbTimePoints: TListBox;
    poImportChapters: TMenuItem;
    poiPlay: TMenuItem;
    poiStop: TMenuItem;
    poiPause: TMenuItem;
    poiAdd: TMenuItem;
    poiSeparator: TMenuItem;
    poiUpdate: TMenuItem;
    poiDelete: TMenuItem;
    poiClearList: TMenuItem;
    MPlayer: TMPlayerControl;
    pomAkce: TPopupMenu;
    btnImportChapters: TSpeedButton;
    trbAudio: TTITrackBar;
    trbProgress: TTrackBar;
    procedure acAddExecute(Sender: TObject);
    procedure acClearListExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acImportChaptersExecute(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acPlayExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure acUpdateExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbTimePointsDblClick(Sender: TObject);
    procedure MPlayerPlaying(ASender: TObject; APosition: single);
    procedure trbAudioChange(Sender: TObject);
    procedure trbProgressChange(Sender: TObject);
    procedure trbProgressMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure trbProgressMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    function timePointsToString():String;
  private

  public
     procedure UpdateTranslation(ALang: String); override;
  end;

resourcestring
  rsPlayerCaption = 'Přehrávač';

var
  frmPlayer: TfrmPlayer;
  usingCustomTimer : Boolean = false; // for use in bad encoded files without pts see: TfrmPlayer.MplayerPlaying()
  elapsedBeforePause : Single = 0;  // time elapsed in customer timer before pause pressed

implementation

uses base;

{$R *.lfm}



{ TfrmPlayer }

var
    changingPosition :Boolean = false;
    buttonPausePressed : Boolean = false;

procedure TfrmPlayer.FormCreate(Sender: TObject);
var
  FileVerInfo: TFileVersionInfo;
begin
  {$IFDEF Linux}
  MPlayer.StartParam := '-vo x11 -zoom -fs';
  {$else $IFDEF Windows}
  MPlayer.StartParam := '-vo direct3d -nofontconfig';
  {$ENDIF}
  MPlayer.Volume:= 50;
  lbTimePoints.Sorted:= True;
  frmPlayer.Caption:=rsPlayerCaption;
  DockMaster.MakeDockable(Self);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(self),TCustomForm(frmMain),alRight,nil);
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    frmMain.Caption:='Audio Book Converter' +
         format(' %s',[FileVerInfo.VersionStrings.Values['FileVersion'].Substring(0,5)]);
  finally
    FileVerInfo.Free;
  end;
  etCustomTimer := TEpikTimer.Create(Application);
end;

procedure TfrmPlayer.lbTimePointsDblClick(Sender: TObject);
var
    pomDateTime : TDateTime;
begin
  if lbTimePoints.ItemIndex <> -1 then
     begin
       pomDateTime:=StrToDateTime(lbTimePoints.Items.Strings[lbTimePoints.ItemIndex]);
       if not MPlayer.Playing then MPlayer.Play;
       // due to 'hh:nnn:ss' is TDateTime aka Double in hours needs to be converted to seconds :-)
       MPlayer.Position := pomDateTime * (24*60*60);
     end;
end;

procedure TfrmPlayer.MPlayerPlaying(ASender: TObject; APosition: Single);
begin
  if (APosition < -1) or usingCustomTimer then
  begin
  // this is a bad encoded file: (mostly badly embedded album art in m4b files)
  // ffmpeg error:
  //  No pts value from demuxer to use for frame! pts after filters MISSING.
  //  Possibly bad interleaving detected.
  //  Use -ni option if this causes playback issues and avoid or fix the program
  //  that created the file.
  // Partially working solution:
  //  see: https://superuser.com/questions/710008/how-to-get-rid-of-ffmpeg-pts-has-no-value-error
  // can be fixed by Mplayer.StartParam := '-novideo -nofontconfig';
    if not usingCustomTimer then
        begin
          lblTime.Caption := 'pts MISSING ...';
          frmBase.memLog.Append('pts MISSING ... using custom Timer!');
          etCustomTimer.Clear;
          etCustomTimer.Start;
          usingCustomTimer := True;
        end;
  APosition := elapsedBeforePause + etCustomTimer.Elapsed;
  //frmBase.memLog.Append('Aposition: ' + FloatToStr(APosition));
  //frmBase.memLog.Append('elapsedBeforePause: ' + FloatToStr(elapsedBeforePause));
  end;
  if (MPlayer.Duration > 0) and (APosition > 0) then
      begin
        changingPosition:= true;
        lblTime.Caption :=  FormatDateTime('hh:nnn:ss', APosition / (24 * 60 * 60)) + ' / ' +
                            FormatDateTime('hh:nnn:ss', MPlayer.Duration / (24 * 60 * 60));
        trbProgress.Position:=  trunc(APosition / MPlayer.Duration * 100);
        Application.ProcessMessages;
        changingPosition:= false;
      end
                                                else
      begin
      end;
      //frmBase.memLog.Append(etCustomTimer.ElapsedDHMS);
      //frmBase.memLog.Append(Format('Poměr: %f',[APosition / MPlayer.Duration]));
      //frmBase.memLog.Append(Format('APosition: %f',[APosition]));
      //frmBase.memLog.Append(Format('Position: %f',[MPlayer.Position]));
      //frmBase.memLog.Append(Format('Duration: %f',[MPlayer.Duration]));
end;

procedure TfrmPlayer.trbAudioChange(Sender: TObject);
begin
  MPlayer.Volume:= trbAudio.Position;
end;

procedure TfrmPlayer.trbProgressChange(Sender: TObject);
var
  newPosition: Single;
begin
   if not changingPosition then
       begin
         newPosition :=  trbProgress.Position/100 * MPlayer.Duration;
         lblTime.Caption :=  FormatDateTime('hh:nnn:ss', newPosition / (24 * 60 * 60)) + ' / ' +
                             FormatDateTime('hh:nnn:ss', MPlayer.Duration / (24 * 60 * 60));
         MPlayer.Position:= newPosition;
         if usingCustomTimer then elapsedBeforePause := newPosition;
       end;
      //frmBase.memLog.Append('trbProgress - ProgressChange - fired');

   // alternative solution maybe using pause of mPlayer when onMouseDown and unpause when onMouseUp
   // not alternative but suplementary :-)

end;

procedure TfrmPlayer.trbProgressMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if  not MPlayer.Paused then        //  Utf8RPos('Pau',acPause.Caption) > 0
     begin
        if usingCustomTimer then
          begin
              etCustomTimer.Stop;
              etCustomTimer.Clear; 
          end;
      end;
end;
          

procedure TfrmPlayer.trbProgressMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if not MPlayer.Paused then                         // Utf8RPos('Pau',acPause.Caption) > 0
     begin
     if usingCustomTimer then etCustomTimer.Start;
     MPlayer.SendMPlayerCommand('osd 3');
     MPlayer.SendMPlayerCommand('osd_show_progression');
     MPlayer.SendMPlayerCommand('speed_set 1.0');

     end;
  //frmBase.memLog.Append('trbProgress - MouseUp - fired');
end;

function TfrmPlayer.timePointsToString: String;
begin
  lbTimePoints.Items.Delimiter:=',';
  Result:= lbTimePoints.Items.DelimitedText;
end;

procedure TfrmPlayer.UpdateTranslation(ALang: String);
begin
  inherited UpdateTranslation(ALang);
end;

procedure TfrmPlayer.acPlayExecute(Sender: TObject);
begin
  if not MPlayer.Paused then
   MPlayer.Play;
end;

procedure TfrmPlayer.acPauseExecute(Sender: TObject);
begin
  MPlayer.Paused:= not MPlayer.Paused;
  if MPlayer.Paused then
   begin
      //btnPause.Caption:= 'PAUSED';
      acPlay.Enabled:= not acPlay.Enabled;
      if usingCustomTimer then
        begin
          elapsedBeforePause := elapsedBeforePause + etCustomTimer.Elapsed;
          etCustomTimer.Stop;
          etCustomTimer.Clear;
        end;  
      
   end
  else
   begin
      //btnPause.Caption:= 'Pause';
      acPlay.Enabled:= not acPlay.Enabled;
      if usingCustomTimer then etCustomTimer.Start;
   end;
end;

procedure TfrmPlayer.acAddExecute(Sender: TObject);
begin
  // probably better to use format 'hh:mmm:ss' because of sorting chapters in book tens hours long
  lbTimePoints.Items.Append(FormatDateTime('hh:nnn:ss', MPlayer.Position / (24 * 60 * 60)));
end;

procedure TfrmPlayer.acClearListExecute(Sender: TObject);
begin
   lbTimePoints.Items.Clear;
end;

procedure TfrmPlayer.acDeleteExecute(Sender: TObject);
var
  i: Integer;
begin
  if lbTimePoints.ItemIndex <> -1 then
      begin
        for i:= lbTimePoints.Count - 1 downto 0 do
          if lbTimePoints.Selected[i] then
            lbTimePoints.Items.Delete(lbTimePoints.ItemIndex);
          lbTimePoints.ClearSelection;
      end;
end;

procedure TfrmPlayer.acImportChaptersExecute(Sender: TObject);
begin
  if frmBase.filesChapters[frmBase.stgVlastnosti.Row-1]['internal'].Count > 0 then
      lbTimePoints.Items.AddStrings(frmBase.filesChapters[frmBase.stgVlastnosti.Row-1]['internal']);
end;

procedure TfrmPlayer.acStopExecute(Sender: TObject);
begin
  if MPlayer.Paused then acPlay.Enabled:=True;
  if usingCustomTimer then
    begin
      etCustomTimer.Stop;
      etCustomTimer.Clear;
      elapsedBeforePause := 0;
      usingCustomTimer := false;
    end;
  MPlayer.Stop;
  lblTime.Caption:= 'HH:MM:SS / HH:MM:SS';
  trbProgress.Position:= 0;
end;

procedure TfrmPlayer.acUpdateExecute(Sender: TObject);
var
   pom : Integer;
begin
  if (lbTimePoints.ItemIndex <> -1) and (lbTimePoints.Selected[lbTimePoints.ItemIndex])  then
      begin
        pom:= lbTimePoints.ItemIndex;
        lbTimePoints.ClearSelection;
        lbTimePoints.Sorted:= false;
        lbTimePoints.Items.Strings[pom] := FormatDateTime(
                                                    'hh:nnn:ss', MPlayer.Position / (24 * 60 * 60));
        lbTimePoints.Sorted:=True;
        lbTimePoints.ItemIndex:= pom;
      end;
end;

end.

