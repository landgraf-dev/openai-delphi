program ChatGPT;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Generics.Collections,
  OpenAIClient,
  OpenAIDtos;

const
  CApiKeyVar = 'OPENAI_API_KEY';

type
  TChatter = class abstract
  strict private
    FClient: IOpenAIClient;
    function GetAPIKey: string;
    function GetClient: IOpenAIClient;
  strict protected
    procedure WriteHeader; virtual;
    function AskQuestion(const Question: string): string; virtual; abstract;
  public
    procedure Run; virtual;
    procedure DoChat; virtual;
    property Client: IOpenAIClient read GetClient;
  end;

  TChatter35 = class(TChatter)
  strict protected
    function AskQuestion(const Question: string): string; override;
  end;

  TChatter40 = class(TChatter)
  strict private
    FMessages: TObjectList<TChatCompletionRequestMessage>;
    procedure AddMessage(const Role, Content: string);
  strict protected
    function AskQuestion(const Question: string): string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoChat; override;
  end;

{ TChatter }

procedure TChatter.DoChat;
var
  Question: string;
  Answer: string;
begin
  repeat
    Write('Write your question: ');
    ReadLn(Question);
    if Question <> '' then
    begin
      Answer := AskQuestion(Question);
      if Answer <> '' then
        WriteLn('Answer: ' + Answer)
      else
        WriteLn('Could not retrieve an answer.');
      WriteLn('');
    end;
  until Question = '';
end;

function TChatter.GetAPIKey: string;
begin
  Result := GetEnvironmentVariable(CApiKeyVar);
  if Result <> '' then
    WriteLn(Format('API Key loaded from environment variable %s.', [CApiKeyVar]))
  else
  begin
    WriteLn('API key not found in system.');
    WriteLn(Format('It is strongly recommended that you set the API key using environvment variable %s.', [cApiKeyVar]));
    Write('Please enter your API key manually: ');
    ReadLn(Result);
    if Result = '' then
      raise Exception.Create('API key not provided.');
  end;
  WriteLn('');
end;

function TChatter.GetClient: IOpenAIClient;
begin
  if FClient = nil then
  begin
    FClient := TOpenAIClient.Create;
    FClient.Config.AccessToken := GetAPIKey;
  end;
  Result := FClient;
end;

procedure TChatter.Run;
begin
  WriteHeader;
  Client; // init client
  DoChat;
  WriteLn('Goodbye.');
end;

procedure TChatter.WriteHeader;
begin
  WriteLn('OpenAI ChatGPT Sample Application version 1.0');
  WriteLn('Using OpenAI API Client for Delphi and Lazarus/FPC');
  WriteLn('https://github.com/landgraf.dev/openai-delphi');
  WriteLn('Copyright (c) Landgraf.dev - all rights reserved.');
  WriteLn('');
end;

{ TChatter35 }

function TChatter35.AskQuestion(const Question: string): string;
var
  Request: TCreateCompletionRequest;
  Response: TCreateCompletionResponse;
begin
  Response := nil;
  Request := TCreateCompletionRequest.Create;
  try
    Request.Prompt := Question;
    Request.Model := 'text-davinci-003';
    Request.MaxTokens := 2048; // Be careful as this can quickly consume your API quota.
    Response := Client.OpenAI.CreateCompletion(Request);
    if Assigned(Response.Choices) and (Response.Choices.Count > 0) then
      Result := Response.Choices[0].Text
    else
      Result := '';
  finally
    Request.Free;
    Response.Free;
  end;
end;

{ TChatter40 }

procedure TChatter40.AddMessage(const Role, Content: string);
var
  Msg: TChatCompletionRequestMessage;
begin
  Msg := TChatCompletionRequestMessage.Create;
  FMessages.Add(Msg);
  Msg.Role := Role;
  Msg.Content := Content;
end;

function TChatter40.AskQuestion(const Question: string): string;
var
  Request: TCreateChatCompletionRequest;
  Response: TCreateChatCompletionResponse;
  SourceMsg, TargetMsg: TChatCompletionRequestMessage;
begin
  Response := nil;
  Request := TCreateChatCompletionRequest.Create;
  try
    AddMessage('user', Question);
    Request.Model := 'gpt-3.5-turbo';
    Request.MaxTokens := 2048; // Be careful as this can quickly consume your API quota.
    for SourceMsg in FMessages do
    begin
      TargetMsg := TChatCompletionRequestMessage.Create;
      Request.Messages.Add(TargetMsg);
      TargetMsg.Role := SourceMsg.Role;
      TargetMsg.Content := SourceMsg.Content;
    end;

    Response := Client.OpenAI.CreateChatCompletion(Request);
    if Assigned(Response.Choices) and (Response.Choices.Count > 0) then
    begin
      Result := Response.Choices[0].Message.Content;
      AddMessage('assistant', Result);
    end
    else
      Result := '';
  finally
    Request.Free;
    Response.Free;
  end;
end;

constructor TChatter40.Create;
begin
  inherited Create;
  FMessages := TObjectList<TChatCompletionRequestMessage>.Create;
end;

destructor TChatter40.Destroy;
begin
  FMessages.Free;
  inherited;
end;

procedure TChatter40.DoChat;
begin
  AddMessage('system', 'You are a helpful assistant.');
  inherited;
end;

var
  Chatter: TChatter;
begin
  try
    Chatter := TChatter40.Create;
    try
      Chatter.Run;
    finally
      Chatter.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

{$IFDEF DEBUG}
{$IFDEF MSWINDOWS}
{$WARNINGS OFF}
  if (DebugHook <> 0) then ReadLn;
{$WARNINGS ON}
{$ENDIF}
{$ENDIF}
end.
