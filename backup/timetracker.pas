unit TimeTracker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fgl;

type
  // Define a class to track time for projects
  
  { TTimeTracker }

  TTimeTracker = class
  private
    FLastUpdateTime: TDateTime;
    FIsRunning: Boolean;
    FProjectName: string;
    FCumulativeTime: Double; // In seconds
    FTimeResolution: Double; // In hours
    function GetCumulativeTime: Double; // Returns time in hours for compatibility
    procedure UpdateTime; // Internal method to update cumulative time
    procedure SetProjectName(AValue: string);
  public
    constructor Create(const AProjectName: string);
    procedure Start;
    procedure Stop;
    procedure Reset;
    procedure UpdateCumulativeTime;
    property ProjectName: string read FProjectName write SetProjectName;
    property CumulativeTime: Double read GetCumulativeTime;
    property IsRunning: Boolean read FIsRunning;
    property TimeResolution: Double read FTimeResolution write FTimeResolution;
  end;

  // Define a specialized generic list to hold TTimeTracker objects keyed by project name
  TTimeTrackerList = specialize TFPGMap<string, TTimeTracker>;

implementation

constructor TTimeTracker.Create(const AProjectName: string);
begin
  inherited Create;
  FProjectName := AProjectName;
  FIsRunning := False;
  FCumulativeTime := 0.0;
  FTimeResolution := 0.25; // Default resolution of 15 minutes
end;


procedure TTimeTracker.Start;
begin
  if not FIsRunning then
  begin
    FLastUpdateTime := Now;
    FIsRunning := True;
  end;
end;

procedure TTimeTracker.Stop;
begin
  UpdateTime;
  FIsRunning := False;
end;

procedure TTimeTracker.Reset;
begin
  FIsRunning := False;
  FCumulativeTime := 0.0;
end;

function TTimeTracker.GetCumulativeTime: Double;
var
  Hours: Double;
begin
  if FCumulativeTime <> 0 then Hours := FCumulativeTime / 3600;
  if (Hours = 0) and not FIsRunning then
    Result := 0
  else
  begin
    if Hours < FTimeResolution then
      Result := FTimeResolution
    else
      Result := Round(Hours / FTimeResolution) * FTimeResolution;
  end;
end;

procedure TTimeTracker.SetProjectName(AValue: string);
begin
  if FProjectName <> AValue then
    FProjectName := AValue;
end;

procedure TTimeTracker.UpdateCumulativeTime;
begin
  if FIsRunning then
    UpdateTime;
end;

procedure TTimeTracker.UpdateTime;
var
  NowTime: TDateTime;
begin
  NowTime := Now;
  if FIsRunning then
  begin
    // Directly accumulate elapsed time in seconds
    FCumulativeTime := FCumulativeTime + SecondsBetween(FLastUpdateTime, NowTime);
    FLastUpdateTime := NowTime;
  end;
end;

end.

