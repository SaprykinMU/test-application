unit AccessToDLLIntf;

interface
uses
  System.Classes;

type

  TParamType = (ptFolder, ptFile, ptString);

  TTaskCommand = (
    tcNone,
    tcResume,
    tcPause,
    tcCancel
    );

  // Статусы задачи
  TTaskStatus = (
    tsWaiting,    // Ожидает выполнения
    tsRunning,    // Выполняется
    tsCompleted,  // Успешно завершена
    tsFailed,     // Завершена с ошибкой
    tsCancelled   // Отменена пользователем
  );



//  TOnComleteTask = reference to procedure;
  TOnSynchronize = reference to procedure(aTaskUI: pointer);

  ITask = interface
    ['{F13D3898-7839-418E-8327-5625FE775A26}']
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




  ITaskProvider = interface
    ['{2DA374D7-FCC7-43AB-B93F-7ADB1070B793}']
    function GetName: PChar;
    function GetParameters: TStrings;
    function NewTask: ITask;
    property Name: PChar read GetName;
    property Parameters: TStrings read GetParameters;
  end;

  // Тип для экспортируемой из DLL функции регистрации
  TRegisterTaskProvider = procedure(const Providers: TInterfaceList) stdcall;


implementation

end.
