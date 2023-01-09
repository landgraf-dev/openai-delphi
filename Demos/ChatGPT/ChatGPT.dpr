program ChatGPT;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  OpenAIClient,
  OpenAIDtos;

const
  CApiKeyVar = 'OPENAI_API_KEY';

var
  Client: IOpenAIClient;

procedure WriteHeader;
begin
  WriteLn('OpenAI ChatGPT Sample Application version 1.0');
  WriteLn('Using OpenAI API Client for Delphi and Lazarus/FPC');
  WriteLn('https://github.com/landgraf.dev/openai-delphi');
  WriteLn('Copyright (c) Landgraf.dev - all rights reserved.');
  WriteLn('');
end;

function GetAPIKey: string;
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

function AskQuestion(const Question: string): string;
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

procedure Run;
var
  Question: string;
  Answer: string;
begin
  WriteHeader;
  Client := TOpenAIClient.Create;
  Client.Config.AccessToken := GetAPIKey;

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
  WriteLn('Goodbye.');
end;

begin
  try
    Run;
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
