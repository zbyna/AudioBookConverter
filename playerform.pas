unit playerform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, ActnList, Menus, RTTICtrls,
  MPlayerCtrl,localizedforms,DefaultTranslator, Buttons,LazUTF8
  ,AnchorDocking
  ,main;

type

  { TfrmPlayer }

  TfrmPlayer = class(TLocalizedForm)
    aclAkce: TActionList;
    acPlay: TAction;
    acStop: TAction;
    acPause: TAction;
    acAdd: TAction;
    acClearList: TAction;
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
    trbAudio: TTITrackBar;
    trbProgress: TTrackBar;
    procedure acAddExecute(Sender: TObject);
    procedure acClearListExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acPlayExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure acUpdateExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

var
  frmPlayer: TfrmPlayer;

implementation

{$R *.lfm}



{ TfrmPlayer }

var
    changingPosition :Boolean = false;
    buttonPausePressed : Boolean = false;

procedure TfrmPlayer.FormCreate(Sender: TObject);
begin
  {$IFDEF Linux}
  MPlayer.StartParam := '-vo x11 -zoom -fs';
  {$else $IFDEF Windows}
  MPlayer.StartParam := '-vo direct3d -nofontconfig';
  {$ENDIF}
  MPlayer.Volume:= 50;
  lbTimePoints.Sorted:= True;
  DockMaster.MakeDockable(Self);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(self),TCustomForm(frmMain),alRight,nil);
end;

procedure TfrmPlayer.MPlayerPlaying(ASender: TObject; APosition: single);
begin
  if (MPlayer.Duration > 0) and (APosition > 0) then
      begin
        changingPosition:= true;
        lblTime.Caption :=  FormatDateTime('h:nnn:ss', APosition / (24 * 60 * 60)) + ' / ' +
                            FormatDateTime('h:nnn:ss', MPlayer.Duration / (24 * 60 * 60));
        trbProgress.Position:=  trunc(APosition / MPlayer.Duration * 100);
        Application.ProcessMessages;
        //ShowMessage(Format('%f',[APosition / MPlayer.Duration]));
        changingPosition:= false;
      end
                                                else
      lblTime.Caption := 'timescale not set';
      //lblTime.Caption := Format('%f',[MPlayer.Duration]) ;
      //lblTime.Caption :=  FormatDateTime('h:nnn:ss', MPlayer.Duration / (24 * 60 * 60));
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
         lblTime.Caption :=  FormatDateTime('h:nnn:ss', newPosition / (24 * 60 * 60)) + ' / ' +
                             FormatDateTime('h:nnn:ss', MPlayer.Duration / (24 * 60 * 60));
         MPlayer.Position:= newPosition;
       end;

   // alternative solution maybe using pause of mPlayer when onMouseDown and unpause when onMouseUp
   // not alternative but suplementary :-)

end;

procedure TfrmPlayer.trbProgressMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if not buttonPausePressed then        //  Utf8RPos('Pau',acPause.Caption) > 0
     MPlayer.Paused:= true;
end;

procedure TfrmPlayer.trbProgressMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if not buttonPausePressed then                         // Utf8RPos('Pau',acPause.Caption) > 0
     begin
     MPlayer.Paused:= false;
     MPlayer.Position:=trbProgress.Position/100 * MPlayer.Duration;
     MPlayer.SendMPlayerCommand('osd 3');
     MPlayer.SendMPlayerCommand('osd_show_progression');
     MPlayer.SendMPlayerCommand('speed_set 1.0');

     end;
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
      buttonPausePressed:= True;
      acPlay.Enabled:= not acPlay.Enabled;
   end
  else
   begin
      //btnPause.Caption:= 'Pause';
      buttonPausePressed:= False;
      acPlay.Enabled:= not acPlay.Enabled;
   end;
end;

procedure TfrmPlayer.acAddExecute(Sender: TObject);
begin
  lbTimePoints.Items.Append(FormatDateTime('h:nnn:ss', MPlayer.Position / (24 * 60 * 60)));
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

procedure TfrmPlayer.acStopExecute(Sender: TObject);
begin
  if MPlayer.Paused then acPlay.Enabled:=True;
  MPlayer.Stop;
  lblTime.Caption:= 'H:MM:SS / H:MM:SS';
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
                                                    'h:nnn:ss', MPlayer.Position / (24 * 60 * 60));
        lbTimePoints.Sorted:=True;
        lbTimePoints.ItemIndex:= pom;
      end;
end;

end.

