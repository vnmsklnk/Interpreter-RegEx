---
hide:
  - navigation
---
# Installation

You can install the package with dotnet by following this steps:

* Add a source and PAT in your nuget.config file:
```
dotnet nuget add source --username <YOUR_USERNAME> --password <PAT> --store-password-in-clear-text --name github "https://nuget.pkg.github.com/IvanMoskalenko/index.json"
```

* Install the package by using this command:
```
dotnet add PROJECT package Interpreter-RegEx --version <version>
```