unit ShellCommandTasks;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,  AccessToDLLIntf,
  Windows, Math;

type

  TShellCommandTask = class(TInterfacedObject, ITask)
  private
    FName: string;
    FDescription: string;
    FStatus: TTaskStatus;
    FProgress: integer;
    FParameters: TStringList;
    FResults: TStringList;
    FLog: TStringList;
    FOnSynchronize: TOnSynchronize;
    FTaskUI: Pointer;
    FCommand: TTaskCommand;
    FCurrentFile: PChar;
  protected
    function GetName: PChar;
    function GetDescription: PChar;
    function GetParameters: TStrings;
    function GetOnSynchronize: TOnSynchronize;
    procedure SetOnSynchronize(Value: TOnSynchronize);
    function GetTaskUI: Pointer;
    procedure SetTaskUI(Value: Pointer);
    function GetProgress: integer;
    function GetStatus: TTaskStatus;
    procedure SetStatus(Value: TTaskStatus);
    function GetResults: TStrings;
    function GetLog: TStrings;
    function GetCommand: TTaskCommand;
    procedure SetCommand(Value: TTaskCommand);
    function GetCurrentFile: PChar;
    procedure SetCurrentFile(Value: PChar);
  public
    constructor Create(aName: string);
    destructor Destroy; override;
    procedure Execute;
    property OnSynchronize: TOnSynchronize read GetOnSynchronize write SetOnSynchronize;
    property Parameters: TStrings read GetParameters;
    property TaskUI: Pointer read GetTaskUI write SetTaskUI;
    property Status: TTaskStatus read GetStatus write SetStatus;
    property Progress: integer read GetProgress;
    property Results: TStrings read GetResults;
    property Log: TStrings read GetLog;
    property Command: TTaskCommand read GetCommand write SetCommand;
    property CurrentFile: PChar read GetCurrentFile write SetCurrentFile;
  end;

  TShellCommandProvider = class(TInterfacedObject, ITaskProvider)
  private
    FName: string;
    FParameters: TStringList;
  protected
    function GetName: PChar;
    function GetParameters: TStrings;
  public
    function NewTask: ITask;
    constructor Create(aName: string);
    destructor Destroy; override;
    property Parameters: TStrings read GetParameters;
  end;



procedure RegisterTaskProvider(const Providers: TInterfaceList); stdcall;

exports
  RegisterTaskProvider;

implementation

uses
  IOUtils, Masks;

procedure RegisterTaskProvider(const Providers: TInterfaceList); stdcall;
var
  TaskProvider: TShellCommandProvider;
begin
  TaskProvider := TShellCommandProvider.Create('Поиск файлов командой find');
  TaskProvider.Parameters.AddObject('Команда=find .\..\..\*.pas "unit"',TObject(ptString));
  Providers.Add(ITaskProvider(TaskProvider));

  TaskProvider := TShellCommandProvider.Create('Архивирование файлов командой tar.exe');
  TaskProvider.Parameters.AddObject('Команда=tar -a -c -f archive.zip *.dcu',TObject(ptString));
  Providers.Add(ITaskProvider(TaskProvider));

end;


{ TShellCommandTask }


  constructor TShellCommandTask.Create(aName: string);
  begin
    inherited Create;
    FName := aName;
    FLog := TStringList.Create;
    FParameters := TStringList.Create;
    FResults := TStringList.Create;
    FStatus := tsWaiting;
  end;

  destructor TShellCommandTask.Destroy;
  begin
    FResults.Free;
    FParameters.Free;
    FLog.Free;
    inherited;
  end;

function GetTime:int64;
var
  ft:FILETIME;
begin
  GetSystemTimeAsFileTime(ft);
//  The FILETIME structure is a 64-bit value representing the number of
//  100-nanosecond intervals since January 1, 1601.
  Result:=Int64(ft) div 10000; //Результат в миллисекундах
end;

function CalcProgress(aDuration: int64): int64;
var
  Duration, Time:int64;
begin
  // Поскольку оценить общий объём задачи заранее не представляется возможным, считаем прогресс выполнения через логарифм.
  // Чем ближе к 100%, тем больше время выполнения шага.
  Duration := aDuration  div 1000;
  if Duration < 1 then Duration := 1;
  Result := Trunc(30 * Log10(Duration));
  if Result > 100 then Result := 100;
end;

procedure TShellCommandTask.Execute;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  hOutputRead, hOutputWrite: THandle;
  Buffer: array[0..4095] of AnsiChar;
  BytesRead: DWORD;
  Running: Boolean;
  StartTime, LastTime, Time, Duration:int64;
begin
  Duration := 1;
  LastTime := GetTime;
  StartTime := LastTime;
  FStatus := tsRunning;
  FProgress := 1;

  FResults.Clear;
  FLog.Clear;
  FStatus := tsRunning;
  FLog.Add(DateTimeToStr(Now) + ' - Начало выполнения команды: ' + FParameters.Values['Команда']);

  SecurityAttr.nLength := SizeOf(TSecurityAttributes);
  SecurityAttr.bInheritHandle := True;
  SecurityAttr.lpSecurityDescriptor := nil;

  if not CreatePipe(hOutputRead, hOutputWrite, @SecurityAttr, 0) then
    RaiseLastOSError;
  try

    ZeroMemory(@StartupInfo, SizeOf(TStartupInfo));
    StartupInfo.cb := SizeOf(TStartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow := SW_HIDE;
    StartupInfo.hStdOutput := hOutputWrite;
    StartupInfo.hStdError := hOutputWrite;

    // Запуск процесса
    if not CreateProcess(nil, PChar(FParameters.Values['Команда']), nil, nil, True, 0, nil, nil, StartupInfo, ProcessInfo) then
      RaiseLastOSError;
    try
      CloseHandle(hOutputWrite);
      // Чтение вывода процесса
      Running := True;
      while Running do
      begin
        Running := WaitForSingleObject(ProcessInfo.hProcess, 50) = WAIT_TIMEOUT;
        while PeekNamedPipe(hOutputRead, nil, 0, nil, @BytesRead, nil) and (BytesRead > 0) do
        begin
          ReadFile(hOutputRead, Buffer, SizeOf(Buffer) - 1, BytesRead, nil);
          Buffer[BytesRead] := #0;
          FResults.Text := FResults.Text + string(Buffer);

          Time := GetTime;
          if Time > LastTime + 100 then
          begin
            Duration := Duration + (Time - LastTime);
            LastTime := Time;
            FProgress := CalcProgress(Duration);

            if Assigned(FOnSynchronize) then OnSynchronize(TaskUI);
          end;
          //sleep(500);
        end;

        if Assigned(FOnSynchronize) then OnSynchronize(TaskUI);

      end;
      if FCommand = tcCancel then
      begin
        FCommand := tcNone;
        TerminateProcess(ProcessInfo.hProcess, 1);
        FStatus := tsCancelled;
        FLog.Add(DateTimeToStr(Now) + ' - Выполнение прервано пользователем');
      end else
      begin
        FStatus := tsCompleted;
        FProgress := 100;
        FLog.Add(DateTimeToStr(Now) + ' - Команда выполнена успешно');
      end;

    finally
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
  finally
    CloseHandle(hOutputRead);
    if Assigned(FOnSynchronize) then
      OnSynchronize(TaskUI);
  end;
end;

function TShellCommandTask.GetName: PChar;
begin
  Result := PChar(FName);
end;

function TShellCommandTask.GetDescription: PChar;
begin
  Result := PChar(FDescription);
end;

function TShellCommandTask.GetParameters: TStrings;
begin
  Result := FParameters;
end;

function TShellCommandTask.GetOnSynchronize: TOnSynchronize;
begin
  Result := FOnSynchronize;
end;

procedure TShellCommandTask.SetOnSynchronize(Value: TOnSynchronize);
begin
  FOnSynchronize := Value;
end;

function TShellCommandTask.GetTaskUI: Pointer;
begin
  Result := FTaskUI;
end;

procedure TShellCommandTask.SetTaskUI(Value: Pointer);
begin
  FTaskUI := Value;
end;

function TShellCommandTask.GetProgress: integer;
begin
  Result := FProgress;
end;

function TShellCommandTask.GetStatus: TTaskStatus;
begin
  Result := FStatus;
end;

procedure TShellCommandTask.SetStatus(Value: TTaskStatus);
begin
  FStatus := Value;
end;

function TShellCommandTask.GetResults: TStrings;
begin
  Result := FResults;
end;

function TShellCommandTask.GetLog: TStrings;
begin
  Result := FLog;
end;

function TShellCommandTask.GetCommand: TTaskCommand;
begin
  Result := FCommand;
end;

procedure TShellCommandTask.SetCommand(Value: TTaskCommand);
begin
  FCommand := Value;
end;

function TShellCommandTask.GetCurrentFile: PChar;
begin
  Result := '';
end;

procedure TShellCommandTask.SetCurrentFile(Value: PChar);
begin
  FCurrentFile := Value;
end;


{ TShellCommandProvider }

constructor TShellCommandProvider.Create(aName: string);
var
  ShellCommandTask: TShellCommandTask;
begin
  FName := aName;
  FParameters := TStringList.Create;
end;

destructor TShellCommandProvider.Destroy;
begin
  FParameters.Free;
  inherited;
end;

function TShellCommandProvider.GetName: PChar;
begin
  Result := PChar(FName);
end;

function TShellCommandProvider.GetParameters: TStrings;
begin
  Result := FParameters;
end;

function TShellCommandProvider.NewTask: ITask;
var
  ShellCommandTask: TShellCommandTask;
  Task: ITask;
begin
  ShellCommandTask := TShellCommandTask.Create(FName);
  ShellCommandTask.Parameters.Assign(Parameters);
  Task := ITask(ShellCommandTask);
  Result := Task;
end;

end.
