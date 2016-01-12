namespace FSharpKatas.GitHubAnimator

    module GitHubAnimator =
        open Octokit
        open System
        open System.IO
        open System.Net
        
        type ChangesFor = {Owner:string; Repository:string; File:string}

        let createClient = 
            let githubClient = new GitHubClient(
                                    new ProductHeaderValue("GitHubAnimator"),
                                    new Uri("https://github.com/"))
            
            let githubToken = Environment.GetEnvironmentVariable("githubtoken")
            githubClient.Credentials <- Credentials(githubToken)
            githubClient

        let commit commitSha parameters (gitHubClient:GitHubClient) =
            async {
                return! gitHubClient.Repository.Commits.Get(
                    parameters.Owner, 
                    parameters.Repository, 
                    commitSha)
                |> Async.AwaitTask
            }
        
        let commits parameters (gitHubClient:GitHubClient) =
            async {
                let! commits = 
                    gitHubClient.Repository.Commits.GetAll(
                        parameters.Owner, 
                        parameters.Repository)
                    |> Async.AwaitTask

                return! commits
                    |> Seq.map (fun c -> commit c.Sha parameters gitHubClient)
                    |> Async.Parallel
            }

        let fileCommits parameters gitHubClient =
            commits parameters gitHubClient
            |> Async.RunSynchronously
            |> Seq.filter (fun c -> 
                            c.Files 
                            |> Seq.exists(fun f -> f.Filename = parameters.File))

        let fileChanges parameters gitHubClient = 
            fileCommits parameters gitHubClient
            |> Seq.map (fun c -> c.Files)
            |> Seq.map (fun files -> 
                files |> Seq.filter(fun f -> 
                    f.Filename = parameters.File) |> Seq.head)
            
        let rawUrlFileChanges parameters gitHubClient =
            fileChanges parameters gitHubClient
            |> Seq.map (fun f -> f.RawUrl)

        let fetchUrlAsync url =        
            async {                             
                let req = WebRequest.Create(Uri(url)) 
                use! resp = req.AsyncGetResponse()
                use stream = resp.GetResponseStream() 
                use reader = new IO.StreamReader(stream) 
                return reader.ReadToEnd() 
            }

        let rawFileChanges parameters gitHubClient =
            async {
                return! rawUrlFileChanges parameters gitHubClient
                    |> Seq.map fetchUrlAsync
                    |> Async.Parallel
            }    

        let createSlidesForChanges parameters gitHubClient =
            rawFileChanges parameters gitHubClient
            |> Async.RunSynchronously
            |> List.ofArray
            |> List.rev
            |> List.map (fun change -> 
                            System.Environment.NewLine
                            + "<section><pre><code class='language-fsharp' data-trim data-noescape>"
                            + System.Environment.NewLine 
                            + change 
                            + System.Environment.NewLine
                            + "</code></pre></section>"
                            + System.Environment.NewLine)
            |> List.fold (+) ""

        let createPresentation parameters gitHubClient =           
           "<!doctype html><html lang='en'><head><meta charset='utf-8'><title>Slides</title><link rel='stylesheet' href='./css/reveal.css'><link rel='stylesheet' href='./css/theme/black.css' id='theme'><link rel='stylesheet' href='./css/prism.css' /><script src='./js/prism.js'></script><!--[if lt IE 9]><script src='./lib/js/html5shiv.js'></script><![endif]--></head>"
           + "<body><div class='reveal'><div class='slides'>"
           + createSlidesForChanges parameters gitHubClient
           + "</div></div><script src='./lib/js/head.js'></script><script src='./js/reveal.js'></script><script>Reveal.initialize({});</script></body>"
           + "</html>"
                    
        let rec directoryCopy srcPath dstPath copySubDirs =

            if not <| System.IO.Directory.Exists(srcPath) then
                let msg = System.String.Format("Source directory does not exist or could not be found: {0}", srcPath)
                raise (System.IO.DirectoryNotFoundException(msg))

            if not <| System.IO.Directory.Exists(dstPath) then
                System.IO.Directory.CreateDirectory(dstPath) |> ignore

            let srcDir = new System.IO.DirectoryInfo(srcPath)

            for file in srcDir.GetFiles() do
                let temppath = System.IO.Path.Combine(dstPath, file.Name)
                file.CopyTo(temppath, true) |> ignore

            if copySubDirs then
                for subdir in srcDir.GetDirectories() do
                    let dstSubDir = System.IO.Path.Combine(dstPath, subdir.Name)
                    directoryCopy subdir.FullName dstSubDir copySubDirs

        let savePresentation slidesTemplatePath slidesPath (presentation:string) =
            directoryCopy slidesTemplatePath slidesPath true

            use outFile = new StreamWriter(slidesPath + "\\Index.html")

            outFile.Write(presentation)

    module GitHubAnimatorTests =
        open NUnit.Framework
        open FsUnit
        open GitHubAnimator
        open Octokit

        let parameters = { Owner = "pedromsantos"; Repository = "FSharpKatas"; File="Bowling.fs" }

        [<Test>]
        [<Ignore("To avoid hitting github API request limits")>]
        let ``Should create git hub client from token``() =
            createClient |> should be ofExactType<GitHubClient>

        [<Test>]
        [<Ignore("To avoid hitting github API request limits")>]
        let ``Should get all commits for repository``() =
            createClient
            |> commits parameters
            |> Async.RunSynchronously
            |> should not' (be Empty) 

        [<Test>]
        [<Ignore("To avoid hitting github API request limits")>]
        let ``Should get all commits for repository that touch a file``() =
            createClient
            |> fileCommits parameters
            |> should not' (be Empty)

        [<Test>]
        [<Ignore("To avoid hitting github API request limits")>]
        let ``Should get all changes for a file``() =
            createClient
            |> fileChanges parameters
            |> should not' (be Empty)

        [<Test>]
        [<Ignore("To avoid hitting github API request limits")>]
        let ``Should get all raw url for changes in a file``() =
            createClient
            |> rawUrlFileChanges parameters
            |> should not' (be Empty)

        [<Test>]
        [<Ignore("To avoid hitting github API request limits")>]
        let ``Should get all file changes``() =
            createClient
            |> rawFileChanges parameters
            |> Async.RunSynchronously
            |> should not' (be Empty)

        [<Test>]
        [<Ignore("To avoid hitting github API request limits")>]
        let ``Should create presentation``() =
            createClient
            |> createPresentation parameters
            |> printfn "%s"

        [<Test>]
        let ``Should save presentation``() =
            createClient
            |> createPresentation parameters
            |> savePresentation 
                "C:\\src\\Katas\\August2015\\FSharpKatas\\reveal.js" 
                "C:\\src\\Katas\\August2015\\FSharpKatas\\Presentation"