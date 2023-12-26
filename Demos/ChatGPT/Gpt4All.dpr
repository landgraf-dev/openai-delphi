// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program Gpt4All;
{ Chat with local server of Gpt4All }
{ Giandomenico De Sanctis - 11/2023}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Generics.Collections,
  OpenAIClient,
  OpenAIDtos;


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
  {Gpt4All doesn't have a key}
  Result := 'xxx';
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
  WriteLn('Gpt4All Sample Application version 1.0');
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
    Request.Model := 'Mistral OpenOrca';
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


var
  Chatter: TChatter;
begin
  try
    Chatter := TChatter35.Create;
    Chatter.Client.Config.BaseUrl:='http://localhost:4891/v1/';
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
