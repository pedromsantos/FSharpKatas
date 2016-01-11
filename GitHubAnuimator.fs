namespace FSharpKatas.GitHubAnimator

    module GitHubAnuimator =
        open Octokit
        open System.Net
        open System
        open System.IO

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
                let text = reader.ReadToEnd() 
                printfn "finished downloading %s" url
                printfn "%s" text
            }

        let rawFileChanges (gitHubClient:GitHubClient) owner repository file =
            async {
                return! rawUrlFileChanges gitHubClient owner repository file
                    |> Seq.map fetchUrlAsync
                    |> Async.Parallel
            }    

        let createPresentation =
            0 |> ignore
            

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
        let ``Should get all file changes``() =
            let githubClient = createClient

            let fileChanges = 
                rawFileChanges githubClient "pedromsantos" "FSharpKatas" "Bowling.fs" 
                |> Async.RunSynchronously
                
            fileChanges |> should not' (be Empty)

        [<Test>]
        let ``Should create presentation``() =
            createPresentation |> ignore