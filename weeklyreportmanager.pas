unit WeeklyReportManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, TimeTracker, TimerDictionary, SdfData, DB;

type

  { TWeeklyReportManager }

  TWeeklyReportManager = class
  private
    FOutFolder: String;
    FFileName: String;
    FCSVDataset: TSdfDataset;
    FTimerDictionary: TTimerDictionary;
    FTimeResolution: Double;
    FInitialized: Boolean;
    function GetWeekStartDate: TDate;
    function GetWeekEndDate: TDate;
    function FormatDate(ADate: TDate): String;
    function FileNameForWeek(StartDate, EndDate: TDate): String;
    procedure EnsureDirectoryExists;
    procedure InitializeDataset;
    procedure CreateFileStructure;
  public
    constructor Create(const OutFolder: String; CSVDataset: TSdfDataset;
      TimerDict: TTimerDictionary; TimeResolution: Double);
    procedure UpdateReport();
  end;

implementation

{ TWeeklyReportManager }

constructor TWeeklyReportManager.Create(const OutFolder: String;
  CSVDataset: TSdfDataset; TimerDict: TTimerDictionary; TimeResolution: Double);
begin
  inherited Create;
  FInitialized := False;
  FOutFolder := IncludeTrailingPathDelimiter(OutFolder);
  EnsureDirectoryExists;
  FFileName := FOutFolder + FileNameForWeek(GetWeekStartDate, GetWeekEndDate);
  FCSVDataset := CSVDataset;
  FTimerDictionary := TimerDict;
  FTimeResolution := TimeResolution;
  InitializeDataset;
end;

function TWeeklyReportManager.GetWeekStartDate: TDate;
begin
  Result := StartOfTheWeek(Now);
end;

  function TWeeklyReportManager.GetWeekEndDate: TDate;
begin
  Result := EndOfTheWeek(Now);
end;

function TWeeklyReportManager.FormatDate(ADate: TDate): String;
begin
  Result := FormatDateTime('dddd', ADate);
end;

function TWeeklyReportManager.FileNameForWeek(StartDate, EndDate: TDate): String;
begin
  Result := Format('TPS_report_%s_%s.csv',
    [FormatDateTime('yyyy-mm-dd', StartDate),
    FormatDateTime('yyyy-mm-dd', EndDate)]);
end;

procedure TWeeklyReportManager.EnsureDirectoryExists;
begin
  if not DirectoryExists(FOutFolder) then
    CreateDir(FOutFolder);
end;

procedure TWeeklyReportManager.UpdateReport();
var
  i, TimePassed: Integer;
  Hours: Double;
  CurrentField: TField;
  TimeTracker: TTimeTracker;
begin
  if not FInitialized then exit;
  FCSVDataset.DisableControls;
  try
    FCSVDataset.First;
    for i := 0 to FTimerDictionary.Count - 1 do
    begin
      CurrentField := FCSVDataset.FieldByName(FormatDate(Now));
      if Assigned(CurrentField) then
      begin
        TimeTracker := FTimerDictionary.ItemByIndex[i];
        if Assigned(TimeTracker) then
        begin
          TimePassed := TimeTracker.CumulativeTimeInSeconds;
          if TimePassed > 0 then
          begin
            // Convert the seconds to hours
            Hours := TimePassed / 3600;
            // Round and quantize to the time resolution
            Hours := Round(Hours / FTimeResolution) * FTimeResolution;
            if Hours > CurrentField.AsFloat then
            begin
              FCSVDataset.Edit;
              CurrentField.AsFloat := Hours;
              FCSVDataset.Post;
            end;
          end;
        end;
      end;
      FCSVDataset.Next;
    end;
  finally
    FCSVDataset.EnableControls;
  end;
end;

procedure TWeeklyReportManager.InitializeDataset;
var
  i: Integer;
  TimeTracker: TTimeTracker;
  CurrentField: TField;
begin
  with FCSVDataset do
  begin
    FileName := FFileName;
    if not FileExists(FFileName) then CreateFileStructure;
    Open;
    if not Active then Active := True;
    DisableControls;
    try
      First;
      for i := 0 to FTimerDictionary.Count - 1 do
      begin
        CurrentField := FieldByName(FormatDate(Now));
        if Assigned(CurrentField) then
        begin
          TimeTracker := FTimerDictionary.ItemByIndex[i];
          if Assigned(TimeTracker) then
          begin
            TimeTracker.CumulativeTimeInSeconds :=
              Round(CurrentField.AsFloat * 3600);
          end;
        end;
        Next;
      end;
    finally
      EnableControls;
    end;
  end;
  FInitialized := True;
end;

procedure TWeeklyReportManager.CreateFileStructure;
var
  i: Integer;
  FileContent: TStringList;
  TimeTracker: TTimeTracker;
begin
  FileContent := TStringList.Create;
  try
    with FileContent do
    begin
      Add('Project,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday');
      for i := 0 to FTimerDictionary.count - 1 do
      begin
        TimeTracker := FTimerDictionary.ItemByIndex[i];
        if Assigned(TimeTracker) then
          Add('"' + TimeTracker.ProjectName +
            '",0.00,0.00,0.00,0.00,0.00,0.00,0.00');
      end;
      FileContent.SaveToFile(FFileName);
    end;
  finally
    FileContent.Free;
  end;
end;

end.
