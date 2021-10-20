[<AutoOpen>]
module TestTools

open System
open AutoFixture
open AutoFixture.Kernel
open AutoFixture.Xunit2

let (|List|_|) candidate =
    match candidate |> box with
    | :? Type as outerType
        when outerType.IsGenericType
             && outerType.GetGenericTypeDefinition() = typedefof<list<_>>
        -> Some <| List(outerType.GetGenericArguments().[0])
    | _ -> None

type ReflectiveList =
    static member Build<'a>(args : obj list) =
        [ for a in args do yield a :?> 'a ]
    
    static member BuildTyped t args =
        typeof<ReflectiveList>
            .GetMethod("Build")
            .MakeGenericMethod([| t |])
            .Invoke(null, [| args |])

let listBuilder (fixture : IFixture) =
    { new ISpecimenBuilder with
            member this.Create(request, context) =
                match request, context with
                | List innerType, _ ->
                    [ for _ in 1..fixture.RepeatCount -> context.Resolve innerType ]
                    |> ReflectiveList.BuildTyped innerType
                | _ -> box <| NoSpecimen() }

[<Sealed>]
type ListCustomization() =
    interface ICustomization with
        member this.Customize fixture =
            fixture |> listBuilder |> fixture.Customizations.Add

let withValue value (fixture : IFixture) =
    fixture.Inject value
    fixture

let withList (fixture : IFixture) =
    fixture.Customize(ListCustomization())

[<Sealed>]
type AutoDataExtendedAttribute() =
    inherit AutoDataAttribute(fun () -> Fixture() |> withList)