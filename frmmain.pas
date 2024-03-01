unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, Grids,
  JSONPropStorage, DBGrids, StdCtrls, TimeTracker, TimerDictionary, Types, DB,
  SdfData, WeeklyReportManager, Math;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    TotalHoursLabel: TLabel;
    TotalHoursDisplay: TEdit;
    TPSDataset: TSdfDataSet;
    TPSDataSource: TDataSource;
    TimerGrid: TDBGrid;
    ExitMenu: TMenuItem;
    FileMenu: TMenuItem;
    FileOpenDialog: TOpenDialog;
    FileOpenMenu: TMenuItem;
    JSONConfig: TJSONPropStorage;
    MainMenu: TMainMenu;
    UpdateTimer: TTimer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExitMenuClick(Sender: TObject);
    procedure FileOpenMenuClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadTimersFromFile(const FileName: String; TimerDict: TTimerDictionary);
    procedure ClearGrid(Grid: TStringGrid);
    procedure TimerGridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure TimerGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimerGridMouseUp(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    FMouseIsDown: Boolean;
    FOutFolder: String;
    FTimeResolution: Double;
    FWeeklyReportManager: TWeeklyReportManager;
    procedure PaintStartStopButton(Rect: TRect; Pressed: Boolean);
    procedure PaintResetButton(Rect: TRect; Pressed: Boolean);
  public

  end;

var
  MainForm: TfrmMain;
  TimerList: TTimerDictionary;

implementation

{$R *.lfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TimerList := TTimerDictionary.Create();
  FTimeResolution := 0.25;
end;

destructor TfrmMain.Destroy();
begin
  TimerList.Free();
  inherited Destroy();
end;

procedure TfrmMain.ExitMenuClick(Sender: TObject);
begin
     MainForm.Close();
end;

procedure TfrmMain.FileOpenMenuClick(Sender: TObject);
begin
     if (FileOpenDialog.Execute()) then
     begin
       LoadTimersFromFile(FileOpenDialog.FileName, TimerList);
       JSONConfig.StoredValue['ProjectListFile'] := FileOpenDialog.FileName;
       JSONConfig.Save;
     end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FWeeklyReportManager.UpdateReport();
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  if Self.Width < 350 then Self.Width := 350;
  if Self.Height < 300 then Self.Height := 300;

  with TimerGrid do
  begin
    Columns[0].Width := Self.Width - (Columns[1].Width + Columns[2].Width +
      Columns[3].Width) - 24;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  ListFileName: String;
  DayOfWeek: String;
begin
  if FileExists(JSONConfig.JSONFileName) then
  begin
    ListFileName := JSONConfig.StoredValue['ProjectListFile'];
    if (ListFileName <> '') then
    begin
      FOutFolder := JSONConfig.StoredValue['OutFolder'];
      FTimeResolution := JSONConfig.StoredValue['TimeResolution'].ToDouble;
      LoadTimersFromFile(ListFileName, TimerList);
    end;
  end;
  // Only show the time column corresponding to the current day of week
  DayOfWeek := FormatDateTime('dddd', Now);
  TimerGrid.Columns[3].FieldName := DayOfWeek;
  // Set the timer interval for updating things to 1/4 the time resolution
  UpdateTimer.Interval := Round((FTimeResolution * 3600 * 1000) / 4);
  UpdateTimer.Enabled := True;
  // Go ahead and update right out of the gate
  UpdateTimerTimer(UpdateTimer);
end;

procedure TfrmMain.LoadTimersFromFile(const FileName: String; TimerDict: TTimerDictionary);
var
  ProjName: String;
  FileContent: TStringList;
  i: Integer;
  TimeTracker: TTimeTracker;
begin
  FileContent := TStringList.Create;
  try
    FileContent.LoadFromFile(FileName);
    for i := 0 to FileContent.Count - 1 do
    begin
      ProjName := FileContent[i];
      if (ProjName <> '') and (TimerDict.Find(ProjName) = nil) then
      begin
        TimeTracker := TTimeTracker.Create(ProjName);
        TimerDict.Add(ProjName, TimeTracker);
      end;
    end;
    if not Assigned(FWeeklyReportManager) then
      FWeeklyReportManager := TWeeklyReportManager.Create(FOutFolder,
        TPSDataset, TimerList, FTimeResolution);
  finally
    FileContent.Free;
  end;
end;

procedure TfrmMain.ClearGrid(Grid: TStringGrid);
begin
  Grid.RowCount := 1; // Keep the header row
  Grid.Clear; // Clear content, but leaves the fixed row intact
end;

procedure TfrmMain.TimerGridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  ProjName: String;
  Timer: TTimeTracker;
begin
  if (DataCol > 0) then
  begin
    with TimerGrid.Canvas do
    begin
      if (DataCol = 1) then
      begin
        ProjName := TPSDataset.FieldByName('Project').AsString;
        Timer := TimerList[ProjName];
        if Assigned(Timer) then
        begin
          if Timer.IsRunning then
            PaintStartStopButton(Rect, True)
          else
            PaintStartStopButton(Rect, False);
        end;
      end else if (DataCol = 2) then
      begin
        PaintResetButton(Rect, FMouseIsDown);
      end;
    end;
  end;
  TimerGrid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TfrmMain.PaintStartStopButton(Rect: TRect; Pressed: Boolean);
var
  BtnText: String;
  BtnTextHeight, BtnTextWidth: Integer;
begin
  with TimerGrid.Canvas do
  begin
    if Pressed then
    begin
      BtnText := 'Running';
      BtnTextHeight := TextHeight(BtnText);
      BtnTextWidth:= TextWidth(BtnText);
      Brush.Color := clMoneyGreen;
      FillRect(Rect);
      Frame3D(Rect, clBtnShadow, clBtnHighlight, 2);
    end else begin
      BtnText := 'Stopped';
      BtnTextHeight := TextHeight(BtnText);
      BtnTextWidth:= TextWidth(BtnText);
      Brush.Color := clBtnFace;
      FillRect(Rect);
      Frame3D(Rect, clBtnHighlight, clBtnShadow, 2);
    end;
    TextOut(Rect.Left + ((Rect.Right - Rect.Left) div 2) - (BtnTextWidth div 2),
      Rect.Top + ((Rect.Bottom - Rect.Top) div 2) - (BtnTextHeight div 2),
      BtnText);
  end;
end;

procedure TfrmMain.PaintResetButton(Rect: TRect; Pressed: Boolean);
var
  BtnText: String;
  BtnTextHeight, BtnTextWidth: Integer;
begin
  with TimerGrid.Canvas do
  begin
    BtnText := 'Reset';
    BtnTextHeight := TextHeight(BtnText);
    BtnTextWidth:= TextWidth(BtnText);
    Brush.Color := clBtnFace;
    FillRect(Rect);

    if Pressed then
       Frame3D(Rect, clBtnShadow, clBtnHighlight, 2)
    else
      Frame3D(Rect, clBtnHighlight, clBtnShadow, 2);

    TextOut(Rect.Left + ((Rect.Right - Rect.Left) div 2) - (BtnTextWidth div 2),
      Rect.Top + ((Rect.Bottom - Rect.Top) div 2) - (BtnTextHeight div 2),
      BtnText);
  end;
end;

procedure TfrmMain.TimerGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
begin
  FMouseIsDown := True;
  TimerGrid.MouseToCell(X, Y, Col, Row);
  if (Col > 0) and (Row = 2) then
  begin
    TimerGrid.InvalidateCell(2, Row);
  end;
end;

procedure TfrmMain.TimerGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
  Timer: TTimeTracker;
  ProjName: String;
begin
  FMouseIsDown := False;
  TimerGrid.MouseToCell(X, Y, Col, Row);

  // Check if a non-header row and a button column was clicked
  if (Row > 0) and (Col in [1, 2, 3]) then
  begin
    ProjName := TPSDataset.FieldByName('Project').AsString;
    Timer := TimerList[ProjName];
    if Assigned(Timer) then
    begin
      if Col = 1 then
      begin
        if (Timer.IsRunning) then
        begin
          TimerList.Stop(ProjName);
        end else begin
          TimerList.Start(ProjName);
        end;
        TimerGrid.InvalidateCell(1, Row);
      end else if Col = 2 then
      begin
        Timer.Reset;
        TimerGrid.InvalidateCell(2, Row);
      end;
    end;
  end;
end;

procedure TfrmMain.UpdateTimerTimer(Sender: TObject);
var
  TimePassed, Hours: Double;
begin
  FWeeklyReportManager.UpdateReport();

  TimePassed := TimerList.TotalTimeInSeconds;
  // Convert the seconds to hours
  Hours := FWeeklyReportManager.AdjustTimeToResolution(TimePassed);
  // Show it...
  TotalHoursDisplay.Text := Format('%0.2f', [Hours]);
end;

end.
