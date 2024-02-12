unit playerform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, ActnList, Menus, RTTICtrls,
  MPVPlayer, BGRAOpenGL, BGRABitmapTypes,localizedforms,DefaultTranslator, Buttons,LazUTF8
  ,AnchorDocking
  ,main
  ,FileInfo;

type

  {TMPVPlayerHelper}

  { TMVPVlayerHelper }

  TMVPVlayerHelper = class helper for TMPVPlayer
  private
      function GetMediaLenInS: Integer;          // seconds TMPVPlayer has only miliseconds
      function GetMediaPosInS: Integer;          //  ------------------- " ----------------
      procedure SetMediaPosInS(AValue: Integer); //  ------------------- " ----------------
  end;

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
    mpvPlayer: TMPVPlayer;
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
    procedure MPVPlayerPlaying(ASender: TObject; APosition: Integer);
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
  draggingPosition:Boolean = False; // changing! position in player´s progressbar

implementation

uses base;

{$R *.lfm}



{ TfrmPlayer }

var
    changingPosition :Boolean = false; // regurally position change during playing  see: TfrmPlayer.MPVPlayerPlaying()

{ TMVPVlayerHelper }

function TMVPVlayerHelper.GetMediaLenInS: Integer;
begin
    Result := self.GetMediaLenInMs div 1000;
end;

function TMVPVlayerHelper.GetMediaPosInS: Integer;
begin
   Result := Self.GetMediaPosInMs div 1000;
end;

procedure TMVPVlayerHelper.SetMediaPosInS(AValue: Integer);
begin
   self.SetMediaPosInMs(AValue * 1000);
end;

procedure TfrmPlayer.FormCreate(Sender: TObject);
var
  FileVerInfo: TFileVersionInfo;
begin
  mpvPlayer.SetAudioVolume(50);
  lbTimePoints.Sorted:= True;
  frmPlayer.Caption:=rsPlayerCaption;
  //DockMaster.MakeDockable(Self);
  //DockMaster.ManualDock(DockMaster.GetAnchorSite(self),TCustomForm(frmMain),alRight,nil);
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    frmMain.Caption:='Audio Book Converter' +
         format(' %s',[FileVerInfo.VersionStrings.Values['FileVersion'].Substring(0,5)]);
  finally
    FileVerInfo.Free;
  end;
end;

procedure TfrmPlayer.lbTimePointsDblClick(Sender: TObject);
var
    pomDateTime : TDateTime;
begin
  if lbTimePoints.ItemIndex <> -1 then
     begin
       pomDateTime:=StrToDateTime(lbTimePoints.Items.Strings[lbTimePoints.ItemIndex]);
       if not mpvPlayer.isPlaying then mpvPlayer.Resume(True);
       // due to 'hh:nnn:ss' is TDateTime aka Double in hours needs to be converted to seconds :-)
       mpvPlayer.SetMediaPosInS(Round(pomDateTime * (24*60*60)));
     end;
end;

procedure TfrmPlayer.MPVPlayerPlaying(ASender: TObject; APosition: Integer);
begin
  APosition:=APosition div 1000; // position supplied by MPVPlayer is in ms
  if draggingPosition then exit;
  //  frmBase.memLog.Append('Raw-Aposition: ' + INtToStr(APosition));
  if (mpvPlayer.GetMediaLenInS  > 0) and (APosition > 0) then
      begin
        changingPosition:= true;
        lblTime.Caption :=  FormatDateTime('hh:nnn:ss', APosition / (24 * 60 * 60)) + ' / ' +
                            FormatDateTime('hh:nnn:ss', mpvPlayer.GetMediaLenInS / (24 * 60 * 60));
        trbProgress.Position:=  trunc(APosition / mpvPlayer.GetMediaLenInS * 100);
        Application.ProcessMessages;
        changingPosition:= false;
      end
                                                else
      begin
      end;
      //frmBase.memLog.Append(Format('Poměr: %f',[APosition / mpvPlayer.Duration]));
      //frmBase.memLog.Append(Format('APosition: %f',[APosition]));
      //frmBase.memLog.Append(Format('Position: %f',[mpvPlayer.Position]));
      //frmBase.memLog.Append(Format('Duration: %f',[mpvPlayer.Duration]));
end;

procedure TfrmPlayer.trbAudioChange(Sender: TObject);
begin
  mpvPlayer.SetAudioVolume(trbAudio.Position);
end;

procedure TfrmPlayer.trbProgressChange(Sender: TObject);
var
  newPosition: Single;
begin
   if not changingPosition then
       begin
         draggingPosition := True;
         newPosition :=  trbProgress.Position/100 * mpvPlayer.GetMediaLenInS;
         lblTime.Caption :=  FormatDateTime('hh:nnn:ss', newPosition / (24 * 60 * 60)) + ' / ' +
                             FormatDateTime('hh:nnn:ss', mpvPlayer.GetMediaLenInS / (24 * 60 * 60));
         mpvPlayer.SetMediaPosInS(Round(newPosition));
       end;
      //frmBase.memLog.Append('trbProgress - ProgressChange - fired');

   // alternative solution maybe using pause of mpvPlayer when onMouseDown and unpause when onMouseUp
   // not alternative but suplementary :-)

end;

procedure TfrmPlayer.trbProgressMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if  not mpvPlayer.IsPaused then        //  Utf8RPos('Pau',acPause.Caption) > 0
     begin
      end;
end;
          

procedure TfrmPlayer.trbProgressMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if not mpvPlayer.IsPaused then                         // Utf8RPos('Pau',acPause.Caption) > 0
     begin
     end;
  draggingPosition := False;
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
  if mpvPlayer.IsPaused then
   mpvPlayer.Resume(True);
end;

procedure TfrmPlayer.acPauseExecute(Sender: TObject);
begin
  mpvPlayer.Pause;
  if mpvPlayer.IsPaused then
   begin
      //btnPause.Caption:= 'PAUSED';
      acPlay.Enabled:= not acPlay.Enabled;
   end
  else
   begin
      //btnPause.Caption:= 'Pause';
      acPlay.Enabled:= not acPlay.Enabled;
   end;
end;

procedure TfrmPlayer.acAddExecute(Sender: TObject);
begin
  // probably better to use format 'hh:mmm:ss' because of sorting chapters in book tens hours long
  lbTimePoints.Items.Append(FormatDateTime('hh:nnn:ss', mpvPlayer.GetMediaPosInS / (24 * 60 * 60)));
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
  if mpvPlayer.IsPaused then acPlay.Enabled:=True;
  mpvPlayer.Stop;
  lblTime.Caption:= 'HH:MM:SS / HH:MM:SS';
  trbProgress.Position:= 0;
  draggingPosition:=False;
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
                                                    'hh:nnn:ss', mpvPlayer.GetMediaPosInS / (24 * 60 * 60));
        lbTimePoints.Sorted:=True;
        lbTimePoints.ItemIndex:= pom;
      end;
end;

end.

