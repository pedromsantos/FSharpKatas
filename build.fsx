// include Fake lib
#r @"./packages/FAKE.3.5.4/tools/FakeLib.dll"
open Fake

RestorePackages()

// Properties
let buildDir = "./build/"

// Targets
Target "Clean" (fun _ ->
    CleanDir buildDir
)

Target "Default" (fun _ ->
    !! "**/*.fsproj"
    |> MSBuildRelease buildDir "Build"
    |> Log "AppBuild-Output: "
)

// Dependencies
"Clean"
   ==> "Default"

// start build
RunTargetOrDefault "Default"
