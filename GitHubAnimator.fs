namespace FSharpKatas.GitHubAnimator

    module GitHubAnuimator =
        open Octokit
        open System.Net
        open System
        open System.IO
        open System.Collections.Generic

        let createClient = 
            let githubClient = new GitHubClient(
                                    new ProductHeaderValue("GitHubAnimator"),
                                    new Uri("https://github.com/"))

            githubClient.Credentials <- Credentials(Environment.GetEnvironmentVariable("githubtoken"))
            githubClient

        let commit (gitHubClient:GitHubClient) owner repository reference =
            async {
                return! gitHubClient.Repository.Commits.Get(owner, repository, reference)
                    |> Async.AwaitTask
            }
        
        let commits (gitHubClient:GitHubClient) owner repository =
            async {
                let! commits = 
                    gitHubClient.Repository.Commits.GetAll(owner, repository)
                    |> Async.AwaitTask

                return! commits
                    |> Seq.map (fun c -> commit gitHubClient owner repository c.Sha)
                    |> Async.Parallel
            }

        let fileCommits (gitHubClient:GitHubClient) owner repository file =
            commits gitHubClient owner repository
            |> Async.RunSynchronously
            |> Seq.filter (fun c -> c.Files |> Seq.exists(fun f -> f.Filename = file))

        let fileChanges (gitHubClient:GitHubClient) owner repository file = 
            fileCommits gitHubClient owner repository file
            |> Seq.map (fun c -> c.Files)
            |> Seq.map (fun files -> files |> Seq.filter(fun f -> f.Filename = file) |> Seq.head)
            

        let rawUrlFileChanges (gitHubClient:GitHubClient) owner repository file =
            fileChanges gitHubClient owner repository file
            |> Seq.map (fun f -> f.RawUrl)


        let fetchUrlAsync url =        
            async {                             
                let req = WebRequest.Create(Uri(url)) 
                use! resp = req.AsyncGetResponse()
                use stream = resp.GetResponseStream() 
                use reader = new IO.StreamReader(stream) 
                return reader.ReadToEnd() 
            }

        let rawFileChanges (gitHubClient:GitHubClient) owner repository file =
            async {
                return! rawUrlFileChanges gitHubClient owner repository file
                    |> Seq.map fetchUrlAsync
                    |> Async.Parallel
            }    

        let createPresentation (gitHubClient:GitHubClient) owner repository file =
            let presenttation = new List<string>()
            presenttation.Add("<!doctype html><html lang='en'><head><meta charset='utf-8'><title>Slides</title><link rel='stylesheet' href='./css/reveal.css'><link rel='stylesheet' href='./css/theme/black.css' id='theme'><link rel='stylesheet' href='./css/prism.css' /><script src='./js/prism.js'></script><!--[if lt IE 9]><script src='./lib/js/html5shiv.js'></script><![endif]--></head>")
            presenttation.Add("<body><div class='reveal'><div class='slides'>")
            
            let fileChanges = 
                rawFileChanges gitHubClient owner repository file 
                |> Async.RunSynchronously

            let slides = new List<string>() 

            for change in fileChanges do
                slides.Add(System.Environment.NewLine 
                            + "<section><pre><code class='language-fsharp' data-trim data-noescape>"
                            + System.Environment.NewLine + change + System.Environment.NewLine
                            + "</code></pre></section>"
                            + System.Environment.NewLine
                            )

            slides.Reverse()
            presenttation.AddRange(slides)

            presenttation.Add("</div></div><script src='./lib/js/head.js'></script><script src='./js/reveal.js'></script><script>Reveal.initialize({});</script></body>")
            presenttation.Add("</html>")
            
            presenttation |> Seq.cast |> Seq.fold (+) ""

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

    module GitHubAnuimatorTests =
        open NUnit.Framework
        open FsUnit
        open GitHubAnuimator
        open Octokit

        [<Test>]
        [<Ignore("To avoid hitting github API request limits")>]
        let ``Should create git hub client from token``() =
            let githubClient = createClient


            githubClient |> should be ofExactType<GitHubClient>

        [<Test>]
        [<Ignore("To avoid hitting github API request limits")>]
        let ``Should get all commits for repository``() =
            let githubClient = createClient

            let repositoryCommits =  
                commits githubClient "pedromsantos" "FSharpKatas"
                 |> Async.RunSynchronously

            repositoryCommits |> should not' (be Empty) 

        [<Test>]
        [<Ignore("To avoid hitting github API request limits")>]
        let ``Should get all commits for repository that touch a file``() =
            let githubClient = createClient

            let commits = fileCommits githubClient "pedromsantos" "FSharpKatas" "Bowling.fs"

            commits |> should not' (be Empty)

        [<Test>]
        [<Ignore("To avoid hitting github API request limits")>]
        let ``Should get all changes for a file``() =
            let githubClient = createClient

            let files = fileChanges githubClient "pedromsantos" "FSharpKatas" "Bowling.fs" 
            
            files |> should not' (be Empty)

        [<Test>]
        [<Ignore("To avoid hitting github API request limits")>]
        let ``Should get all raw url for changes in a file``() =
            let githubClient = createClient

            let urls = rawUrlFileChanges githubClient "pedromsantos" "FSharpKatas" "Bowling.fs" 
            
            urls |> should not' (be Empty)

        [<Test>]
        [<Ignore("To avoid hitting github API request limits")>]
        let ``Should get all file changes``() =
            let githubClient = createClient

            let fileChanges = 
                rawFileChanges githubClient "pedromsantos" "FSharpKatas" "Bowling.fs" 
                |> Async.RunSynchronously
                
            fileChanges |> should not' (be Empty)

        [<Test>]
        let ``Should create presentation``() =
            let githubClient = createClient

            let presentation = createPresentation githubClient "pedromsantos" "FSharpKatas" "Bowling.fs"

            printfn "%s" presentation

        [<Test>]
        let ``Should save presentation``() =
            let githubClient = createClient

            let presentation = createPresentation githubClient "pedromsantos" "FSharpKatas" "Bowling.fs"

            savePresentation 
                "C:\\src\\Katas\\August2015\\FSharpKatas\\reveal.js" 
                "C:\\src\\Katas\\August2015\\FSharpKatas\\Presentation"
                presentation
