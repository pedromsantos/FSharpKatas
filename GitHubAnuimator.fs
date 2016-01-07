namespace FSharpKatas.GitHubAnimator

    module GitHubAnuimator =
        open Octokit
        open System

        let createClientWithToken token = 
            let githubClient = new GitHubClient(
                                    new ProductHeaderValue("FSharpKatas"),
                                    new Uri("https://github.com/pedromsantos/FSharpKatas"))

            githubClient.Credentials <- Credentials(token)
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
            commits gitHubClient "pedromsantos" "FSharpKatas"
            |> Async.RunSynchronously
            |> Seq.filter (fun c -> c.Files |> Seq.exists(fun f -> f.Filename = file))

        let fileChanges (commits:GitHubCommit seq) file = 
            commits
            |> Seq.map (fun c -> c.Files)
            |> Seq.map (fun files -> files |> Seq.filter(fun f -> f.Filename = file) |> Seq.head)
            

    module FizzBuzzTests =
        open NUnit.Framework
        open FsUnit
        open GitHubAnuimator
        open Octokit

        [<Test>]
        let ``Should create git hub client from token``() =
            let githubClient = createClientWithToken "38842f632eb161188010c87221368100d47dc2af"


            githubClient |> should be ofExactType<GitHubClient>


        [<Test>]
        let ``Should get all commits for repository``() =
            let githubClient = createClientWithToken "38842f632eb161188010c87221368100d47dc2af"

            let repositoryCommits =  
                commits githubClient "pedromsantos" "FSharpKatas"
                 |> Async.RunSynchronously

            repositoryCommits |> should not' (be Empty) 

        [<Test>]
        let ``Should get all commits for repository that touch a file``() =
            let githubClient = createClientWithToken "38842f632eb161188010c87221368100d47dc2af"

            let commits = fileCommits githubClient "pedromsantos" "FSharpKatas" "Bowling.fs"

            commits |> should not' (be Empty)

        [<Test>]
        let ``Should get all changes for a file``() =
            let githubClient = createClientWithToken "38842f632eb161188010c87221368100d47dc2af"

            let commits = fileCommits githubClient "pedromsantos" "FSharpKatas" "Bowling.fs"

            let files = fileChanges commits "Bowling.fs" 
            
            files |> should not' (be Empty)