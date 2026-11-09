/// The only network code in tessera: fetching an external BPA rule file a model lists, used only
/// when the user passes --allow-external-rules. The request carries nothing about the model: no
/// cookies, no credentials, no body, only a generic User-Agent.
module Tessera.BpaFetch

open System
open System.IO
open System.Net
open System.Net.Http
open System.Threading.Tasks

/// How long a fetch may take, connection included.
let timeout = TimeSpan.FromSeconds 15.0

/// The largest rule file fetched (the community rule files are about 100 KB).
let maxBytes = 4L * 1024L * 1024L

let private client =
    lazy
        (let handler =
            new HttpClientHandler(
                UseCookies = false,
                UseDefaultCredentials = false,
                Credentials = null,
                PreAuthenticate = false,
                AllowAutoRedirect = true,
                MaxAutomaticRedirections = 5,
                AutomaticDecompression = DecompressionMethods.All
            )

         // Buffering the whole response (the default) puts the body under the timeout too, and
         // HttpClient refuses content over the buffer size.
         let c =
             new HttpClient(handler, Timeout = timeout, MaxResponseContentBufferSize = maxBytes)

         c.DefaultRequestHeaders.UserAgent.ParseAdd "tessera"
         c)

/// GET an https:// URL as text, refusing anything over `maxBytes` and redirects away from https.
let fetch (url: Uri) : Result<string, string> =
    match Bpa.refuseUrl url with
    | Some reason -> Error reason
    | None ->
        try
            use request = new HttpRequestMessage(HttpMethod.Get, url)

            use response = client.Value.Send request
            let final = response.RequestMessage.RequestUri

            if not (isNull final) && final.Scheme <> Uri.UriSchemeHttps then
                Error $"redirected to a non-https URL ({final})"
            elif not response.IsSuccessStatusCode then
                Error $"HTTP {int response.StatusCode} {response.ReasonPhrase}"
            else
                use stream = response.Content.ReadAsStream()
                use reader = new StreamReader(stream, Text.Encoding.UTF8, true)
                Ok(reader.ReadToEnd())
        with
        | :? TaskCanceledException
        | :? OperationCanceledException -> Error $"timed out after {int timeout.TotalSeconds} s"
        | :? HttpRequestException as e -> Error e.Message
        | e -> Error e.Message
