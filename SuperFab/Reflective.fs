module SuperFab.Reflective
open System.Reflection
type RefInfo = {Name:string;Location: Result<string,exn>}

module ReflectiveImpl =
    let getName (x:Assembly) =
        x.FullName
        |> Option.ofObj
        |> Option.map (fun n -> n.Split(',').[0])
        |> Option.defaultValue x.FullName

    type LocationStage =
        | AN of AssemblyName
        | A of Assembly

    let getRefAssemblies () =
        // items that must be force loaded in case they haven't loaded yet
        let needsTypeReferenceToLoad =
            [
            ]
            |> Seq.map (fun (a:Assembly) -> getName a, a)
            |> dict

        Assembly.GetExecutingAssembly().GetReferencedAssemblies()
        |> Seq.sortBy(fun ra -> ra.Version)
        |> List.ofSeq
        |> List.rev
        |> Seq.map (fun a ->
            a.Name,
                    if needsTypeReferenceToLoad.ContainsKey a.Name then
                        A needsTypeReferenceToLoad.[a.Name]
                    else AN a
        )

    let getLocation =
        function
            | AN an ->
                try
                    if isNull an.CodeBase then Ok <| Assembly.ReflectionOnlyLoad(an.FullName).Location else Ok an.CodeBase
                with ex -> Result.Error ex
            | A a -> Result.Ok a.Location
open ReflectiveImpl

let getRefs() =
    getRefAssemblies()
    |> Seq.map(fun (n,a)-> {Name=n;Location= getLocation a})
