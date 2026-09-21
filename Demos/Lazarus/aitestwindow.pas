unit aitestwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, OpenAIClient,
  OpenAIDtos;

type

  { TAIThread }

  TAIThread = class(TThread)
  private
    FLock: TRTLCriticalSection;
    FEvent: PRTLEvent;
    FRunning: boolean;
    FClient: IOpenAIClient;
    FMsgReady: boolean;
    FMessage: string;
    FMsgList: TChatCompletionRequestMessageList;
    FPrompt, FModel, FURL: string;
    function mkmsg(role, content: string): TChatCompletionRequestMessage;
    procedure AddMessage(role, content: string);
    procedure SendChat;
  protected
    procedure Execute; override;
  public
    constructor Create(prompt, model, url: string);
    property MessageReady: Boolean read FMsgReady;
    property Message: string read FMessage;
    procedure SendMessage(msg: string);
    procedure StopThread;
  end;

  { TAITestForm }

  TAITestForm = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FThread: TAIThread;
  public

  end;

var
  AITestForm: TAITestForm;

implementation

const
  SYSTEM_PROMPT = '';
  OPENAI_URL = '';
  MODEL = '';

{$R *.lfm}

{ TAIThread }

function TAIThread.mkmsg(role, content: string): TChatCompletionRequestMessage;
begin
  Result:=TChatCompletionRequestMessage.Create;
  Result.Role:=role;
  Result.Content:=content;
end;

procedure TAIThread.AddMessage(role, content: string);
begin
  FMsgList.Add(mkmsg(role, content));
end;

procedure TAIThread.SendChat;
var
  req: TCreateChatCompletionRequest;
  resp: TCreateChatCompletionResponse;
  msg: TChatCompletionRequestMessage;
begin
  resp:=Nil;
  req:=TCreateChatCompletionRequest.Create;
  try
    req.Model:=FModel;
    AddMessage('system', FPrompt);
    EnterCriticalSection(FLock);
    AddMessage('user', FMessage);
    FMessage:='';
    LeaveCriticalSection(FLock);
    for msg in FMsgList do
      req.Messages.Add(mkmsg(msg.Role, msg.Content));
    resp:=FClient.OpenAI.CreateChatCompletion(req);
    if Assigned(resp.Choices) and (resp.Choices.Count > 0) then
    begin
      FMessage:=resp.Choices[0].Message.Content;
      AddMessage(resp.Choices[0].Message.Role, FMessage);
    end;
  finally
    req.Free;
    resp.Free;
    FMsgReady:=True;
  end;
end;

procedure TAIThread.Execute;
begin
  InitCriticalSection(FLock);
  FEvent:=RTLEventCreate;
  FClient:=TOpenAIClient.Create;
  FClient.Config.BaseUrl:=FURL;
  FMsgList:=TChatCompletionRequestMessageList.Create;
  FRunning:=True;
  try
    repeat
      RTLEventResetEvent(FEvent);
      RTLEventWaitFor(FEvent);
      if FMessage <> '' then
        SendChat;
    until not FRunning;
  finally
    FMsgList.Free;
    RTLEventDestroy(FEvent);
    DoneCriticalSection(FLock);
  end;
end;

constructor TAIThread.Create(prompt, model, url: string);
begin
  inherited Create(True);
  if prompt = '' then
    FPrompt:='You are a helpful assistant.'
  else
    FPrompt:=prompt;
  if model = '' then
    FModel:='google/gemma-3-4b'
  else
    FModel:=model;
  if url = '' then
    FURL:= 'http://localhost:1234/v1'
  else
    FURL:=url;
end;

procedure TAIThread.SendMessage(msg: string);
begin
  EnterCriticalSection(FLock);
  FMsgReady:=False;
  FMessage:=msg;
  LeaveCriticalSection(FLock);
  RTLEventSetEvent(FEvent);
end;

procedure TAIThread.StopThread;
begin
  FRunning:=False;
  FMessage:='';
  RTLEventSetEvent(FEvent);
end;

{ TAITestForm }

procedure TAITestForm.FormCreate(Sender: TObject);
begin
  FThread:=TAIThread.Create(SYSTEM_PROMPT, MODEL, OPENAI_URL);
  FThread.Start;
end;

procedure TAITestForm.Button1Click(Sender: TObject);
begin
  Button1.Enabled:=False;
  Edit1.Enabled:=False;
  Application.ProcessMessages;
  FThread.SendMessage(edit1.Text);
  Edit1.Text:='Please wait...';
  repeat
    Application.ProcessMessages;
  until FThread.MessageReady;
  Edit1.Text:='';
  Memo1.Text:=FThread.Message;
  Edit1.Enabled:=True;
  Button1.Enabled:=True;
end;

procedure TAITestForm.FormDestroy(Sender: TObject);
begin
  FThread.StopThread;
  FThread.WaitFor;
  FThread.Free;
end;

end.

