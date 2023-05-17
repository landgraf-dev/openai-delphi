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

All objects created and passed as input parameters, as well all objects created by the client and provided as function results, must be destroyed by you, otherwise you will get a memory leak.

You can use Delphi *code completion* to check the available methods, parameters, types and read documentation comments.

## Requirements

This library does not require any 3rd party library. It works on all recent Delphi versions that provides unit (`System.Net.HttpClient`) and Lazarus/FPC. Althought not fully tested, it should also work on all supported platforms (Windows, Linux, macOS, etc.). 

Since the library requires your secret API key, it's not recommended you use it on client applications, as your secret key will be exposed, unless you are sure about the security risks.

## Request customization

You can customize the request for your own need by using your own request factory. Just implement the interface `IRestRequestFactory` and set it in the property `Config.RequestFactory`.

For example, if you want to setup a custom organization (which is provided in an HTTP header), this is a sample code:

```delphi
uses {...}, OpenApiRest;

type
  TMyRequestFactory = class(TInterfacedObject, IRestRequestFactory)
  strict private
    FFactory: IRestRequestFactory;
  public
    constructor Create(AFactory: IRestRequestFactory);
    function CreateRequest: IRestRequest;
  end;

constructor TMyRequestFactory.Create(AFactory: IRestRequestFactory);
begin
  FFactory := AFactory;
end;

function TMyRequestFactory.CreateRequest: IRestRequest;
begin
  Result := FFactory.CreateRequest;
  Result.AddHeader('OpenAI-Organization', 'org-nUilalMOTvqGjGeAopTDIsSB');
end;

// Set it after creating the client
    FClient := TOpenAIClient.Create;
    FClient.Config.RequestFactory := TMyRequestFactory.Create(FClient.Config.RequestFactory);
```

### Integrating with WhatsApp in Delphi
Example of using openai-delphi with [WPP4Delphi](https://github.com/wppconnect-team/WPP4Delphi) in [WPPConnect-Team](https://github.com/wppconnect-team)

Component Opensource for sending messages integrated into whatsapp web using Delphi

Example:  [Demo View](https://www.youtube.com/watch?v=zwA2KYIoxiM&t=2s&ab_channel=WPPConnect)
![image](https://user-images.githubusercontent.com/26030963/216602518-3a497347-6fb4-4c99-af61-92ffb3949993.png)


Code Example in Delphi:
```
if SwtChatGPT.IsOn then
begin
if Question <> '' then
begin
	//Credits --> https://github.com/landgraf-dev/openai-delphi
	Answer      := AskQuestion(Question, AChat.id);
	phoneNumber := Copy(Answer, 1, pos('#', Answer)-1);
	Answer      := StringReplace(Answer, phoneNumber + '#', '',[]);

	if Trim(Answer) <> '' then
	frDemo.TWPPConnect1.SendTextMessageEx(phoneNumber, '🤖' + ' *ChatGPT* ' + Answer, 'createChat: true', '123')
	else
	frDemo.TWPPConnect1.SendTextMessageEx(phoneNumber, '🤖' + ' *ChatGPT* ' + 'Could not retrieve an answer.', 'createChat: true', '123');

end;
end;
``` 
Complete Code in [WPP4Delphi](https://github.com/wppconnect-team/WPP4Delphi)

