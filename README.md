# OpenAI for Delphi and Lazarus/FPC.

OpenAI for Delphi is a client library to connect to the [OpenAI API](https://openai.com/api/) from Delphi and Lazarus/FPC (Free Pascal Compiler). 
From this library you can use popular OpenAI services, like [ChatGPT](https://openai.com/blog/chatgpt/), directly from your Delphi or Lazarus application.

*This is an unofficial library. OpenAI does not provide any official library for Delphi.*

## Installation

To use the library, just add the [Source](/Source) folder to the IDE library path, or your project source path.

## Usage

To use the client, add units `OpenAIClient` and `OpenAIDtos` to your unit uses clause.

```delphi
uses 
  OpenAIClient, OpenAIDtos;
```

### Creating the client

The library needs to be configured with your account's secret API key, which is available on the [website](https://beta.openai.com/account/api-keys). We recommend setting it as an environment variable named `OPENAI_API_KEY`. Once you have the API key, just create the client and set the key as the following:

```delphi
var
  Client: IOpenAIClient;
{...}
  Client := TOpenAIClient.Create;
  Client.Config.AccessToken := GetEnvironmentVariable('OPENAI_API_KEY');
```

### Executing methods

Most of [OpenAI API endpoints](https://beta.openai.com/docs/api-reference) are available in the client, under the interface provided in the `OpenAI` property. The following example shows how to ask a question and receive an answer using the CreateCompletion endpoint:

```delphi
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
```

All objects created and passed as input parameteres, as well all objects created by the client and provided as function results, must be destroyed by you, otherwise you will get a memory leak.

You can use Delphi *code completion* to check the available methods, parameters, types and read documentation comments.

## Requirements

This library does not require any 3rd party library. It works on all recent Delphi versions that provides unit (`System.Net.HttpClient`) and Lazarus/FPC. Althought not fully tested, it should also work on all supported platforms (Windows, Linux, macOS, etc.). 

Since the library requires your secret API key, it's not recommended you use it on client applications, as your secret key will be exposed, unless you are sure about the security risks.
