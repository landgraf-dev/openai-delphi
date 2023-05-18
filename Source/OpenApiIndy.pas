unit OpenApiIndy;

interface

uses
  SysUtils, Classes, OpenApiRest, IdHTTP;

type
  TIndyHTTP = class(TIdHTTP)
  end;

  TClientCreatedEvent = procedure(Client: TIdHttp) of object;

  TIndyRestRequest = class(TRestRequest)
  strict private
    FOnClientCreated: TClientCreatedEvent;
  public
    constructor Create(AOnClientCreated: TClientCreatedEvent);
    function Execute: IRestResponse; override;
  end;

  TIndyRestResponse = class(TInterfacedObject, IRestResponse)
  strict private
    FClient: TIndyHTTP;
    FResponseBody: TBytes;
  public
    constructor Create(Client: TIndyHTTP; const ResponseBody: TBytes);
    destructor Destroy; override;
    function StatusCode: Integer;
    function ContentAsString: string;
    function ContentAsBytes: TBytes;
    function GetHeader(const Name: string): string;
  end;

  TIndyRestRequestFactory = class(TInterfacedObject, IRestRequestFactory)
  private
    FOnClientCreated: TClientCreatedEvent;
  public
    function CreateRequest: IRestRequest;
    property OnClientCreated: TClientCreatedEvent read FOnClientCreated write FOnClientCreated;
  end;

implementation

{ TIndyRestRequestFactory }

function TIndyRestRequestFactory.CreateRequest: IRestRequest;
begin
  Result := TIndyRestRequest.Create(FOnClientCreated);
end;

{ TIndyRestRequest }

constructor TIndyRestRequest.Create(AOnClientCreated: TClientCreatedEvent);
begin
  inherited Create;
  FOnClientCreated := AOnClientCreated;
end;

function TIndyRestRequest.Execute: IRestResponse;
var
  Client: TIndyHTTP;
  I: Integer;
  RequestBody: TStringStream;
  ResponseBody: TBytesStream;
begin
  Client := TIndyHTTP.Create;
  try
    if Assigned(FOnClientCreated) then
      FOnClientCreated(Client);

    Client.HTTPOptions := Client.HTTPOptions + [hoNoProtocolErrorException, hoWantProtocolErrorContent];
    RequestBody := nil;
    if Body <> '' then
      RequestBody := TStringStream.Create(Body, TEncoding.UTF8, False);
    try
      ResponseBody := TBytesStream.Create;
      try
        Client.Request.Accept := '';
        for I := 0 to Headers.Count - 1 do
          Client.Request.CustomHeaders.AddValue(Headers.Names[I], Headers.ValueFromIndex[I]);
        Client.DoRequest(Self.Method, BuildUrl, RequestBody, ResponseBody, []);
        Result := TIndyRestResponse.Create(Client, Copy(ResponseBody.Bytes, 0, ResponseBody.Size));
        Client := nil;
      finally
        ResponseBody.Free;
      end;
    finally
      RequestBody.Free;
    end;
  finally
    Client.Free;
  end;
end;

{ TIndyRestResponse }

constructor TIndyRestResponse.Create(Client: TIndyHTTP; const ResponseBody: TBytes);
begin
  inherited Create;
  FClient := Client;
  FResponseBody := ResponseBody;
end;

destructor TIndyRestResponse.Destroy;
begin
  FClient.Free;
  inherited;
end;

function TIndyRestResponse.GetHeader(const Name: string): string;
begin
  Result := FClient.Response.RawHeaders.Values[Name];
end;

function TIndyRestResponse.StatusCode: Integer;
begin
  Result := FClient.ResponseCode;
end;

function TIndyRestResponse.ContentAsBytes: TBytes;
begin
  Result := FResponseBody;
end;

function TIndyRestResponse.ContentAsString: string;
begin
  Result := TEncoding.UTF8.GetString(ContentAsBytes);
end;

end.
