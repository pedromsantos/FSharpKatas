#!/bin/bash
#mono --runtime=v4.0 .nuget/nuget.exe install FAKE -Version 4.25.4 -OutputDirectory packages
mono --runtime=v4.0 packages/FAKE/tools/FAKE.exe build.fsx $@
