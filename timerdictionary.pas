unit TimerDictionary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TimeTracker;

type
  TNameTrackerPair = record
    Name: string;
    Tracker: TTimeTracker;
  end;
  PNameTrackerPair = ^TNameTrackerPair;

  { TTimerDictionary }

  TTimerDictionary = class
  private
    FList: TList;
    function GetItem(Index: String): TTimeTracker;
    procedure SetItem(Index: String; AValue: TTimeTracker);
    function GetTotalTime: Double;
    function GetCount: Integer;
    function GetItemByIndex(Index: Integer): TTimeTracker;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Name: String; AValue: TTimeTracker);
    function Find(const Name: String): TTimeTracker;
    procedure Delete(const Name: String);
    procedure Start(const Name: String);
    procedure Stop(const Name: String);
    procedure StopAll;
    procedure UpdateTrackedTime;
    property Items[Index: String]: TTimeTracker read GetItem write SetItem; default;
    property ItemByIndex[Index: Integer]: TTimeTracker read GetItemByIndex;
    property TotalTime: Double read GetTotalTime;
    property Count: Integer read GetCount;
  end;

implementation

constructor TTimerDictionary.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TTimerDictionary.Destroy;
var
  i: Integer;
  Pair: PNameTrackerPair;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Pair := FList.Items[i];
    Pair^.Tracker.Free;
    Dispose(Pair);
  end;
  FList.Free;
  inherited Destroy;
end;

procedure TTimerDictionary.Add(const Name: String; AValue: TTimeTracker);
var
  Pair: PNameTrackerPair;
begin
  New(Pair);
  Pair^.Name := Name;
  Pair^.Tracker := AValue;
  FList.Add(Pair);
end;

function TTimerDictionary.Find(const Name: String): TTimeTracker;
var
  i: Integer;
  Pair: PNameTrackerPair;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    Pair := FList.Items[i];
    if Pair^.Name = Name then
    begin
      Result := Pair^.Tracker;
      Break;
    end;
  end;
end;

procedure TTimerDictionary.Delete(const Name: String);
var
  i: Integer;
  Pair: PNameTrackerPair;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Pair := FList.Items[i];
    if Pair^.Name = Name then
    begin
      Pair^.Tracker.Free;
      Dispose(Pair);
      FList.Delete(i);
      Break;
    end;
  end;
end;

function TTimerDictionary.GetItem(Index: String): TTimeTracker;
begin
  Result := Find(Index);
end;

procedure TTimerDictionary.SetItem(Index: String; AValue: TTimeTracker);
begin
  Add(Index, AValue);
end;

function TTimerDictionary.GetTotalTime: Double;
var
  i: Integer;
  Pair: PNameTrackerPair;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do
  begin
    Pair := FList.Items[i];
    Result := Result + Pair^.Tracker.CumulativeTime;
  end;
end;

function TTimerDictionary.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TTimerDictionary.GetItemByIndex(Index: Integer): TTimeTracker;
var
  Pair: PNameTrackerPair;
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    Pair := FList.Items[Index];
    Result := Pair^.Tracker;
  end
  else
    Result := nil;
end;

procedure TTimerDictionary.Start(const Name: String);
begin
  StopAll;
  Find(Name).Start;
end;

procedure TTimerDictionary.Stop(const Name: String);
begin
  Find(Name).Stop;
end;

procedure TTimerDictionary.StopAll;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    GetItemByIndex(i).Stop;
end;

procedure TTimerDictionary.UpdateTrackedTime;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    if GetItemByIndex(i).IsRunning then
      GetItemByIndex(i).UpdateCumulativeTime;
  end;
end;

end.

