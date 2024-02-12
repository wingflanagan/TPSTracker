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
    FIsRunning: Boolean;
    FProjectName: string;
    FLastCheckedTime: TDateTime;
    FCurrentTime: TDateTime;
    FCumulativeTimeInSeconds: Integer;
    procedure SetProjectName(AValue: string);
    function GetCumulativeTimeInSeconds: Integer;
  public
    constructor Create(const AProjectName: string);
    procedure Start;
    procedure Stop;
    procedure Reset;
    property ProjectName: string read FProjectName write SetProjectName;
    property IsRunning: Boolean read FIsRunning;
    property CumulativeTimeInSeconds: Integer read GetCumulativeTimeInSeconds
      write FCumulativeTimeInSeconds;
  end;

implementation

constructor TTimeTracker.Create(const AProjectName: string);
begin
  inherited Create;
  FProjectName := AProjectName;
  FIsRunning := False;
  FCumulativeTimeInSeconds := 0;
end;


procedure TTimeTracker.Start;
begin
  if not FIsRunning then
  begin
    FIsRunning := True;
    FLastCheckedTime := Now;
  end;
end;

procedure TTimeTracker.Stop;
begin
  FIsRunning := False;
end;

procedure TTimeTracker.Reset;
begin
  FIsRunning := False;
  FCumulativeTimeInSeconds := 0;
end;

procedure TTimeTracker.SetProjectName(AValue: string);
begin
  if FProjectName <> AValue then
    FProjectName := AValue;
end;

function TTimeTracker.GetCumulativeTimeInSeconds: Integer;
var
  RightNow: TDateTime;
begin
  if FIsRunning then
  begin
    RightNow := Now;
    FCumulativeTimeInSeconds := FCumulativeTimeInSeconds +
      SecondsBetween(RightNow, FLastCheckedTime);
    FLastCheckedTime := Now;
  end;
  Result := FCumulativeTimeInSeconds;
end;

end.
