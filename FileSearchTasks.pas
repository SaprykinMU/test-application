unit FileSearchTasks;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs, AccessToDLLIntf,
  Windows, System.Generics.Collections;

type

   TFoundPosition = record Name: string; Position: Int64; end;
   TSearchArrays = TArray<TBytes>;
   TFoundPositions = TList<TFoundPosition>;
   TSearchType = (stFolder, stFile);

  TFileSearchTask = class(TInterfacedObject, ITask)
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
    FMaskList: TStringList;
    FSearchStringList: TStringList;
    FCurrentFile: PChar;
    FSearchType: TSearchType;
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
    property SearchType: TSearchType read FSearchType write FSearchType;
  end;

  TFileSearchProvider = class(TInterfacedObject, ITaskProvider)
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

var
  CriticalSection: TCriticalSection;

procedure RegisterTaskProvider(const Providers: TInterfaceList); stdcall;

exports
  RegisterTaskProvider;

implementation

uses
  IOUtils, Masks;

procedure RegisterTaskProvider(const Providers: TInterfaceList); stdcall;
var
  TaskProvider: TFileSearchProvider;
begin
  TaskProvider := TFileSearchProvider.Create('Поиск файлов по маске');;
  TaskProvider.Parameters.AddObject('Стартовая папка=.\..\..', TObject(ptFolder));
  TaskProvider.Parameters.AddObject('Маска=*.pas;*.dpr;*.dll', TObject(ptString));
  Providers.Add(ITaskProvider(TaskProvider));

  TaskProvider := TFileSearchProvider.Create('Поиск файлов по маске и тексту');;
  TaskProvider.Parameters.AddObject('Стартовая папка=.\..\..', TObject(ptFolder));
  TaskProvider.Parameters.AddObject('Маска=*.pas; "*.dpr"', TObject(ptString));
  TaskProvider.Parameters.AddObject('Текст="TTaskUI", IntToStr', TObject(ptString));
  Providers.Add(ITaskProvider(TaskProvider));

  TaskProvider := TFileSearchProvider.Create('Поиск в файле');;
  TaskProvider.Parameters.AddObject('Файл=.\..\..\tm.res', TObject(ptFile));
  TaskProvider.Parameters.AddObject('Текст=assembly,dependency', TObject(ptString));
  Providers.Add(ITaskProvider(TaskProvider));
end;







{ TFileSearchTask }

constructor TFileSearchTask.Create(aName: string);
begin
  inherited Create;
  FName := aName;
  FLog := TStringList.Create;
  FParameters := TStringList.Create;
  FResults := TStringList.Create;
  FMaskList := TStringList.Create;
  FSearchStringList := TStringList.Create;
  FStatus := tsWaiting;
end;

destructor TFileSearchTask.Destroy;
begin
  FSearchStringList.Free;
  FMaskList.Free;
  FResults.Free;
  FParameters.Free;
  FLog.Free;
  inherited;
end;

function GetTime: int64;
var
  ft: FILETIME;
begin
  GetSystemTimeAsFileTime(ft);
  // The FILETIME structure is a 64-bit value representing the number of
  // 100-nanosecond intervals since January 1, 1601.
  Result := int64(ft) div 10000; // Результат в миллисекундах
end;

function CalcProgress(aCurrent, aTotal:Int64): integer;
begin
  if aTotal <= 0 then aTotal := 1;

  Result := aCurrent * 100 div aTotal;
  if Result < 3 then Result := 3;
  if Result > 97 then Result := 97;
end;

function MatchesMaskList(aFileName: string; aMaskList: TStrings): boolean;
begin
  Result := False;
  for var i := 0 to aMaskList.Count - 1 do
    if MatchesMask(aFileName, aMaskList[i]) then
    begin
      Result := True;
      break;
    end;
end;

procedure TFileSearchTask.Execute;
const
    BufSize = 5 * 1024 * 1024;
 type
   TSearchArrays = TArray<TBytes>;
var
  LastTime, Time: int64;
  Buffer: PByte;
  SearchArrays: TSearchArrays;
  SearchStringLength, MaxSearchStringLength: Integer;
  TotalFolders, CountFolders: Integer;
  TotalFilesFound: Integer;
  FoundPositions: TFoundPositions;
  SearchString: string;
  CurrentFileLength, CurrentFilePos: Int64;


  procedure AddToLog(const s: string);
  begin
    Log.Add(s);
    Log.Add(SysErrorMessage(GetLastError));
    Log.Add('');
  end;

  procedure DoSynchronize;
  begin
    if FCommand = tcPause then
      while not(FCommand in [tcResume, tcCancel]) do
      begin
        Status := tsWaiting;
        Sleep(100);
        LastTime := GetTime;
        if Assigned(FOnSynchronize) then
          FOnSynchronize(TaskUI);
      end;

      FStatus := tsRunning;

      Time := GetTime;
      if Time > LastTime + 100 then
      begin
        LastTime := Time;
        if FSearchType = stFolder then
          FProgress := CalcProgress(CountFolders, TotalFolders)
        else
          FProgress := CalcProgress(CurrentFilePos, CurrentFileLength);
        if Assigned(FOnSynchronize) then
          FOnSynchronize(TaskUI);
      end;
  end;


  procedure FindText(const aFileName: string);
  var
    Found, AddFileNameToResults: boolean;
    i, j: Int64;
    FilePos: Int64;
    hFile: THandle;
    BytesRead: Integer;
    HighBound: Integer;
    FoundPosition: TFoundPosition;
    SomethingFound, AddFoundPositionToResults: boolean;
  begin
    hFile := FileOpen(aFileName, fmOpenRead);

    if hFile <> INVALID_HANDLE_VALUE then
    begin
      try
        AddFileNameToResults := True;
        FilePos := 0;
        SomethingFound := False;
        FoundPositions.Clear;
        CurrentFileLength :=  FileSeek(hFile, 0, 2) + 1;
        FileSeek(hFile, 0, 0);
        repeat
          BytesRead := FileRead(hFile, Buffer^, BufSize);

          for var IndexOfSearchArray := 0 to High(SearchArrays) do
          begin

            if BytesRead = BufSize then
              HighBound := MaxSearchStringLength - 1
            else
              HighBound := Length(FSearchStringList[IndexOfSearchArray]) - 1;

            for i := 0 to BytesRead - 1 - HighBound do
            begin
              if FCommand = tcCancel then break;
              DoSynchronize;

              Found := True;
              for j := 0 to High(SearchArrays[IndexOfSearchArray]) do
              begin
                if Buffer[i + j] <> SearchArrays[IndexOfSearchArray][j] then
                begin
                  Found := False;
                  break;
                end;
              end;

              if Found then
              begin
                if AddFileNameToResults then
                begin
                  SomethingFound := True;
                  Inc(TotalFilesFound);
                  AddFileNameToResults := False;
                  FResults.Add(aFileName);
                end;
                FoundPosition.Name := FSearchStringList[IndexOfSearchArray];
                FoundPosition.Position := FilePos + i;
                FoundPositions.Add(FoundPosition);
              end;
            end;
          end;
          FilePos := FileSeek(hFile, -int64(MaxSearchStringLength - 1), 1);
          CurrentFilePos := FilePos;

        until BytesRead < BufSize;

        // Выводим список найденых позиций
        if SomethingFound then
        begin
          for var IndexOfSearchArray := 0 to High(SearchArrays) do
          begin
            SearchString :=  FSearchStringList[IndexOfSearchArray];
            AddFoundPositionToResults := True;
            for var Index := 0 to FoundPositions.Count - 1 do
            begin
              if FoundPositions[Index].Name = SearchString then
              begin
                if AddFoundPositionToResults then
                begin
                  AddFoundPositionToResults := False;
                  FResults.Add('Найденные позиции для "' + SearchString + '"');
                end;

                FResults.Add(IntToStr(FoundPositions[Index].Position));
              end;
            end;
          end;
          FResults.Add('');
        end;

      finally
        FileClose(hFile);
      end;
    end
    else
      AddToLog('Не удалось открыть файл "' + aFileName + '"');
  end;


  procedure ScanFolder(aPath: string; aCountTotalFolders: boolean);
  var
    FileName, FullFileName: string;
    FileData: TWIN32FindData;
    SH: THandle;
  begin
    SH := FindFirstFile(PWideChar(IncludeTrailingBackslash(aPath) + '*.*'), FileData);
    if SH <> INVALID_HANDLE_VALUE then
    begin
      repeat
        FileName := FileData.cFileName;
        if (FileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
        begin
          if aCountTotalFolders then Inc(TotalFolders) else Inc(CountFolders);

          if not((FileName = '.') or (FileName = '..')) then
            ScanFolder(IncludeTrailingBackslash(aPath) + FileName, aCountTotalFolders);
        end
        else
        begin
          if (not aCountTotalFolders) and MatchesMaskList(FileName, FMaskList) then
            begin
              FullFileName := IncludeTrailingBackslash(aPath) + Filename;
              FCurrentFile := PChar(FullFileName);

              if (FSearchStringList.Count = 0) then
              begin
                Inc(TotalFilesFound);
                FResults.Add(FullFileName)
              end else
                FindText(FullFileName);
            end;
        end;

        if FCommand = tcCancel then break;

        DoSynchronize;

      until not FindNextFile(SH, FileData);
      Windows.FindClose(SH);
    end;

  end;

  procedure ScanFile(aPath: string);
  begin
    FindText(aPath);
  end;

begin
  TotalFilesFound := 0;
  FoundPositions := TFoundPositions.Create;
  CountFolders := 0;
  TotalFolders := 0;
  FSearchStringList.Clear;
  FResults.Clear;
  FLog.Clear;
  FLog.Add(DateTimeToStr(Now) + ' - Начало выполнения');
  FCommand := tcNone;
  FStatus := tsRunning;
  FCurrentFile := '';
  if Assigned(FOnSynchronize) then
    FOnSynchronize(TaskUI);

  // Функции FindFirstFile и FindNextFile не являются потокобезопасными.
  // Поэтому, необходимо запретить их одновременное выполнение в разных потоках.
  CriticalSection.Enter;

  Buffer := System.AllocMem(BufSize);
  try
    LastTime := GetTime;
    FProgress := 1;

    ExtractStrings([';', ','], [' '], PChar(FParameters.Values['Маска']), FMaskList);
    for var Index := 0 to FMaskList.Count - 1 do FMaskList[Index] := AnsiDequotedStr(FMaskList[Index],'"');

    ExtractStrings([';', ','], [' '], PChar(FParameters.Values['Текст']), FSearchStringList);
    for var Index := 0 to FSearchStringList.Count - 1 do FSearchStringList[Index] := AnsiDequotedStr(FSearchStringList[Index],'"');

    SetLength(SearchArrays,FSearchStringList.Count);
    MaxSearchStringLength := 0;

    for var Index := 0 to FSearchStringList.Count - 1 do
    begin
      FSearchStringList[Index] := AnsiDequotedStr(FSearchStringList[Index],'"');
      SearchStringLength := Length(FSearchStringList[Index]);
      if SearchStringLength > MaxSearchStringLength then MaxSearchStringLength := SearchStringLength;

      SearchArrays[Index] := TEncoding.ANSI.GetBytes(FSearchStringList[Index]);
    end;

    if FParameters.Values['Стартовая папка'] <> '' then SearchType := stFolder else SearchType := stFile;


    if SearchType = stFolder then
    begin
      ScanFolder(FParameters.Values['Стартовая папка'],True);   // Считаем общее количество папок, чтобы на втором проходе вычислять прогресс выполнения
      ScanFolder(FParameters.Values['Стартовая папка'],False);
      FResults.Add('Найдено файлов - '+ IntToStr(TotalFilesFound) );
    end else
      ScanFile(FParameters.Values['Файл']);


    if FCommand = tcCancel then
    begin
      FCommand := tcNone;
      FStatus := tsCancelled;
      FLog.Add(DateTimeToStr(Now) + ' - Задача прервана пользователем');
      FResults.Add('Задача прервана пользователем');
    end else
    begin
      FCurrentFile := '';
      FStatus := tsCompleted;
      FProgress := 100;
      FLog.Add(DateTimeToStr(Now) + ' - Задача выполнена');
    end;

    if Assigned(FOnSynchronize) then
      OnSynchronize(TaskUI);
  finally
    FoundPositions.Free;
    FreeMem(Buffer);
    CriticalSection.Leave;
  end;
end;

function TFileSearchTask.GetName: PChar;
begin
  Result := PChar(FName);
end;

function TFileSearchTask.GetDescription: PChar;
begin
  Result := PChar(FDescription);
end;

function TFileSearchTask.GetParameters: TStrings;
begin
  Result := FParameters;
end;

function TFileSearchTask.GetOnSynchronize: TOnSynchronize;
begin
  Result := FOnSynchronize;
end;

procedure TFileSearchTask.SetOnSynchronize(Value: TOnSynchronize);
begin
  FOnSynchronize := Value;
end;

function TFileSearchTask.GetTaskUI: Pointer;
begin
  Result := FTaskUI;
end;

procedure TFileSearchTask.SetTaskUI(Value: Pointer);
begin
  FTaskUI := Value;
end;

function TFileSearchTask.GetProgress: integer;
begin
  Result := FProgress;
end;

function TFileSearchTask.GetStatus: TTaskStatus;
begin
  Result := FStatus;
end;

procedure TFileSearchTask.SetStatus(Value: TTaskStatus);
begin
  FStatus := Value;
end;

function TFileSearchTask.GetResults: TStrings;
begin
  Result := FResults;
end;

function TFileSearchTask.GetLog: TStrings;
begin
  Result := FLog;
end;

function TFileSearchTask.GetCommand: TTaskCommand;
begin
  Result := FCommand;
end;

procedure TFileSearchTask.SetCommand(Value: TTaskCommand);
begin
  FCommand := Value;
end;

function TFileSearchTask.GetCurrentFile: PChar;
begin
  Result := FCurrentFile;
end;

procedure TFileSearchTask.SetCurrentFile(Value: PChar);
begin
  FCurrentFile := Value;
end;


{ TFileSearchProvider }

constructor TFileSearchProvider.Create(aName: string);
begin
  FName := aName;
  FParameters := TStringList.Create;
end;

destructor TFileSearchProvider.Destroy;
begin
  FParameters.Free;
  inherited;
end;

function TFileSearchProvider.GetName: PChar;
begin
  Result := PChar(FName);
end;

function TFileSearchProvider.GetParameters: TStrings;
begin
  Result := FParameters;
end;

function TFileSearchProvider.NewTask: ITask;
var
  FileSearchTask: TFileSearchTask;
  Task: ITask;
begin
  FileSearchTask := TFileSearchTask.Create(FName);
  FileSearchTask.Parameters.Assign(Parameters);

  Task := ITask(FileSearchTask);
  Result := Task;
end;

initialization
  CriticalSection := TCriticalSection.Create;
finalization
  CriticalSection.Free;
end.
