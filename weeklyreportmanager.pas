unit WeeklyReportManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, TimeTracker, TimerDictionary, csvdataset, DB;

type

  { TWeeklyReportManager }

  TWeeklyReportManager = class
  private
    FOutFolder: String;
    FFileName: String;
    FCSVDataset: TCSVDataset;
    FTimerDictionary: TTimerDictionary;
    FCSVColumnNames: array[0..7] of String;
    function GetWeekStartDate: TDate;
    function GetWeekEndDate: TDate;
    procedure CreateNewWeekFile();
    function FormatDate(ADate: TDate): String;
    function FileNameForWeek(StartDate, EndDate: TDate): String;
    procedure EnsureDirectoryExists;
    procedure InitializeColumnNames;
    procedure InitializeDataset;
  public
    constructor Create(const OutFolder: String; CSVDataset: TCSVDataset;
      TimerDict: TTimerDictionary);
    procedure UpdateReport();
  end;

implementation

{ TWeeklyReportManager }

constructor TWeeklyReportManager.Create(const OutFolder: String;
  CSVDataset: TCSVDataset; TimerDict: TTimerDictionary);
begin
  inherited Create;
  FOutFolder := IncludeTrailingPathDelimiter(OutFolder);
  EnsureDirectoryExists;
  FFileName := FOutFolder + FileNameForWeek(GetWeekStartDate, GetWeekEndDate);
  FCSVDataset := CSVDataset;
  FTimerDictionary := TimerDict;
  InitializeColumnNames;
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
  Result := FormatDateTime('dddd, m/d/yyyy', ADate);
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

procedure TWeeklyReportManager.CreateNewWeekFile();
var
  SL: TStringList;
  i: Integer;
  Header: String;
begin
  SL := TStringList.Create;
  Header := String.Empty;
  try
    // Add headers with dates, enclosed in double quotes
    for i := Low(FCSVColumnNames) to High(FCSVColumnNames) do
    begin
      Header := Header + '"' + (FCSVColumnNames[i]) + '"';
      if i < High(FCSVColumnNames) then Header := Header + ',';
    end;
    SL.Add(Header);

    // Initialize rows for each project
    for i := 0 to FTimerDictionary.Count - 1 do
      SL.Add('"' + FTimerDictionary.ItemByIndex[i].ProjectName + '"' + ',,,,,,,');
    SL.SaveToFile(FFileName);

  finally
    SL.Free;
  end;
end;

procedure TWeeklyReportManager.UpdateReport();
var
  i: Integer;
  CurrentField: TField;
begin
  FCSVDataset.First;
  for i := 0 to FTimerDictionary.Count - 1 do
  begin
    FCSVDataset.Edit;
    CurrentField := FCSVDataset.FieldByName(FormatDate(Now));
    if Assigned(CurrentField) then
      CurrentField.AsFloat := FTimerDictionary.ItemByIndex[i].CumulativeTime;
    FCSVDataset.Post;
    FCSVDataset.Next;
  end;
  //FCSVDataset.;
end;

procedure TWeeklyReportManager.InitializeColumnNames;
var
  i: Integer;
  StartDate: TDate;
begin
  StartDate := GetWeekStartDate;
  FCSVColumnNames[0] := 'Projects';
  for i := 1 to 7 do
    FCSVColumnNames[i] := FormatDate(StartDate + (i - 1));
end;

procedure TWeeklyReportManager.InitializeDataset;
var
  i: Integer;
  CurrentField: TField;
  FieldValue: String;
begin
  //if not FileExists(FFileName) then CreateNewWeekFile();

  with FCSVDataset do
  begin
    FileName := FFileName;
    //Active := True;
    if not FileExists(FFileName) then
    begin
      for i := Low(FCSVColumnNames) to High(FCSVColumnNames) do
      begin
        if i = 0 then
          FieldDefs.Add(FCSVColumnNames[i], ftString)
        else
          FieldDefs.Add(FCSVColumnNames[i], ftFloat);
      end;
      CreateDataset;
      Active := True;
      //FCSVDataset.First;
      for i := 0 to FTimerDictionary.Count - 1 do
      begin
        Append;
        CurrentField := FieldByName(FCSVColumnNames[0]);
        if Assigned(CurrentField) then
        begin
          CurrentField.AsString := FTimerDictionary.ItemByIndex[i].ProjectName;
        end;
        Post;
      end;
      //SaveToCSVFile(FileName);
    end;
    if not Active then Active := True;
  end;
end;

end.
