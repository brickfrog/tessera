# Third-party notices

tessera's own source code is MIT-licensed (see `LICENSE`). The release binaries and the `tessera` NuGet tool
package also contain third-party components, which are licensed to you under their own terms, listed below.
**The MIT license in `LICENSE` does not apply to them.**

## Microsoft Analysis Management Objects (AMO / TOM)

`Microsoft.AnalysisServices` 19.x (`Microsoft.AnalysisServices.*.dll`). © Microsoft Corporation. All rights
reserved.

This component is licensed under the **Microsoft Software License Terms for Microsoft Analysis Management
Objects (AMO)**: <https://go.microsoft.com/fwlink/?linkid=852989>. tessera redistributes it, unmodified and in
object code form, as distributable code under section 2 of those terms. By using tessera's binaries you agree to
those terms for that component. In particular, you may not reverse engineer, decompile or disassemble it, and
you may not remove Microsoft's notices from it.

## Components under the MIT license

| Component | Copyright |
|---|---|
| Microsoft.Identity.Client (MSAL.NET), Microsoft.Identity.Client.Broker, Microsoft.Identity.Client.NativeInterop, Microsoft.IdentityModel.Abstractions | © Microsoft Corporation |
| System.Configuration.ConfigurationManager, System.Security.Cryptography.ProtectedData | © Microsoft Corporation (.NET Foundation) |
| .NET runtime, in the self-contained binaries (its own notices: <https://github.com/dotnet/runtime/blob/main/THIRD-PARTY-NOTICES.TXT>) | © .NET Foundation and Contributors |
| FSharp.Core | © Microsoft Corporation (.NET Foundation) |
| Argu | © 2019 Argu contributors (<https://github.com/fsprojects/Argu>) |
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

Licensed under the Apache License, Version 2.0: <https://www.apache.org/licenses/LICENSE-2.0>. Distributed
unmodified.

## Not affiliated

tessera is an independent project. It is not affiliated with, endorsed by or sponsored by Microsoft or Tabular
Editor ApS. "Power BI", "Analysis Services" and "Microsoft" are trademarks of Microsoft Corporation.
