# Third-party notices

tessera's own source code is MIT-licensed (see `LICENSE`). The release binaries and the `tessera` NuGet tool
package also contain third-party components, which are licensed to you under their own terms, listed below.
**The MIT license in `LICENSE` does not apply to them.**

## Microsoft Analysis Management Objects (AMO / TOM)

`Microsoft.AnalysisServices` 19.x (`Microsoft.AnalysisServices.*.dll`). © Microsoft Corporation. All rights
reserved.

This component is licensed under the **Microsoft Software License Terms for Microsoft Analysis Management
Objects (AMO)**. The full text is in `licenses/Microsoft-AMO-license-terms.rtf`, as Microsoft publishes it at
<https://go.microsoft.com/fwlink/?linkid=852989>. tessera includes the component unmodified, in object code
form. By using tessera's binaries you agree to those terms for that component. In particular, you may not
reverse engineer, decompile or disassemble it, you may not remove Microsoft's notices from it, and you may not
redistribute it except as those terms allow.

## Microsoft ADOMD.NET

`Microsoft.AnalysisServices.AdomdClient` 19.x (`Microsoft.AnalysisServices.AdomdClient.dll`). © Microsoft
Corporation. All rights reserved.

This component is licensed under the **Microsoft Software License Terms for Microsoft Analysis Services
ADOMD.NET**. The full text is in `licenses/Microsoft-ADOMD.NET-license-terms.rtf`, as Microsoft publishes it at
<https://go.microsoft.com/fwlink/?linkid=852895>. tessera includes the component unmodified, in object code
form. By using tessera's binaries you agree to those terms for that component, on the same points as for AMO
above.

### Data collection

Both sets of Microsoft terms say the software "may collect information about you and your use of the software
and send that to Microsoft". tessera turns on no such feature. It makes no network connection unless you ask
for one: the commands that work on model files and fixture data (`query --data`, `bpa`, `validate`, and the
others) open no network socket at all, and connected mode (`xmla:` sources, `connect`, `deploy`, `refresh`)
talks only to the server you name.

## Components under the MIT license

| Component | Copyright |
|---|---|
| Microsoft.Identity.Client (MSAL.NET), Microsoft.Identity.Client.Broker, Microsoft.Identity.Client.NativeInterop, Microsoft.IdentityModel.Abstractions | © Microsoft Corporation |
| System.Configuration.ConfigurationManager, System.Security.Cryptography.ProtectedData | © Microsoft Corporation (.NET Foundation) |
| .NET runtime, in the self-contained binaries (its own notices: <https://github.com/dotnet/runtime/blob/main/THIRD-PARTY-NOTICES.TXT>) | © .NET Foundation and Contributors |
| FSharp.Core | © Microsoft Corporation (.NET Foundation) |
| Argu | © 2019 Argu contributors (<https://github.com/fsprojects/Argu>) |
| YamlDotNet | © Antoine Aubry and contributors (<https://github.com/aaubry/YamlDotNet>) |
| Tabular Editor 2: small portions adapted in `src/Tessera.Core` (for example the BPA token codes in `BpaDaxToken.fs`), marked in the source | © 2017 Daniel Otykier (<https://github.com/TabularEditor/TabularEditor>) |

The MIT license text:

> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
> documentation files (the "Software"), to deal in the Software without restriction, including without limitation
> the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and
> to permit persons to whom the Software is furnished to do so, subject to the following conditions:
>
> The above copyright notice and this permission notice shall be included in all copies or substantial portions
> of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
> THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
> AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
> CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
> IN THE SOFTWARE.

## Components under the Apache License 2.0

| Component | Copyright |
|---|---|
| System.Linq.Dynamic.Core (<https://dynamic-linq.net/>) | © ZZZ Projects |

Licensed under the Apache License, Version 2.0; the full text is in `licenses/Apache-2.0.txt`
(<https://www.apache.org/licenses/LICENSE-2.0>). Distributed unmodified.

## Used only by the tests (not shipped)

`tests/Tessera.Tests/sarif/sarif-schema-2.1.0.json` is the OASIS SARIF 2.1.0 (errata 01) JSON schema, copied
unmodified from <https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/schemas/sarif-schema-2.1.0.json>.
Copyright © OASIS Open 2023. All Rights Reserved. The tests use it to check the SARIF `tess` writes; it is not in
the binaries or the package. OASIS's notice, which permits copies that keep it, is next to the file in
`tests/Tessera.Tests/sarif/NOTICE.txt`.

## Not affiliated

tessera is an independent project. It is not affiliated with, endorsed by or sponsored by Microsoft or Tabular
Editor ApS. "Power BI", "Analysis Services" and "Microsoft" are trademarks of Microsoft Corporation.
