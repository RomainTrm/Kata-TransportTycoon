module Tests

open Xunit

type Destination = A | B
type TransportId = int
type Place = Factory | Port | Destination_A | Destination_B
type PackagePosition = InTransport of TransportId | Place of Place
type Package = { Destination: Destination; Position: PackagePosition }

type EAT = int
type PathSegment = { Start: Place; End: Place; EAT: EAT }

type AtHomeBase = { TransportId: TransportId; Place: Place }
type MovingToPlace = { TransportId: TransportId; HomeBase: Place; Destination: Place; EAT: EAT; HomeBaseEAT: EAT }
type ReturningAtHomeBase = { TransportId: TransportId; HomeBase: Place; EAT: EAT }
type Transport =
    | AtHomeBase of AtHomeBase
    | MovingToPlace of MovingToPlace
    | ReturningAtHomeBase of ReturningAtHomeBase

let initial_transports = [
    AtHomeBase { TransportId = 1; Place = Factory }
    AtHomeBase { TransportId = 2; Place = Factory }
    AtHomeBase { TransportId = 3; Place = Port }
]

let paths = dict[
    A, [{ Start = Factory; End = Port; EAT = 1 }; { Start = Port; End = Destination_A; EAT = 4 }]
    B, [{ Start = Factory; End = Destination_B; EAT = 5 }]
]

let rec moveTransport transport packages =
    match transport, packages with
    | _, [] -> transport, []
    
    | AtHomeBase atHomeBase, {Destination = destination; Position = Place place}::tail when place = atHomeBase.Place ->
        let pathStep = paths.[destination] |> List.find (fun step -> step.Start = place)
        let movingTransport = MovingToPlace { TransportId = atHomeBase.TransportId; HomeBase = atHomeBase.Place; Destination = pathStep.End; EAT = pathStep.EAT; HomeBaseEAT = 0 }
        let movingPackage = { Destination = destination; Position = InTransport atHomeBase.TransportId }
        movingTransport, movingPackage::tail
        
    | AtHomeBase _, head::tail ->
        let transport, tail = moveTransport transport tail
        transport, head::tail
    
    | MovingToPlace moving, _ when moving.EAT = 1 ->
        let movingTransport = ReturningAtHomeBase { TransportId = moving.TransportId; HomeBase = moving.HomeBase; EAT = moving.HomeBaseEAT + 1 }
        let unloadPackage (package: Package) =
            { package with Position = match package.Position with
                                       | InTransport id when id = moving.TransportId -> Place moving.Destination
                                       | _ -> package.Position
            }
        let packages = packages |> List.map unloadPackage
        movingTransport, packages
    
    | MovingToPlace moving, _ ->
        let movingTransport = MovingToPlace { moving with EAT = moving.EAT - 1; HomeBaseEAT = moving.HomeBaseEAT + 1 }
        movingTransport, packages
        
    | ReturningAtHomeBase moving, {Destination = destination; Position = Place place}::tail when place = moving.HomeBase && moving.EAT = 1 ->
        let pathStep = paths.[destination] |> List.find (fun step -> step.Start = place)
        let movingTransport = MovingToPlace { TransportId = moving.TransportId; HomeBase = moving.HomeBase; Destination = pathStep.End; EAT = pathStep.EAT; HomeBaseEAT = 0 }
        let movingPackage = { Destination = destination; Position = InTransport moving.TransportId }
        movingTransport, movingPackage::tail
        
    | ReturningAtHomeBase moving, head::tail when moving.EAT = 1 ->
        let transport, tail = moveTransport transport tail
        transport, head::tail
        
    | ReturningAtHomeBase moving, [] when moving.EAT = 1 ->
        let atHomeBase = AtHomeBase { TransportId = moving.TransportId; Place = moving.HomeBase }
        atHomeBase, packages

    | ReturningAtHomeBase moving, _ ->
        let movingTransport = ReturningAtHomeBase { moving with EAT = moving.EAT - 1 }
        movingTransport, packages

let rec moveTransports transports packages =
    match transports with
    | [] -> [], packages
    | head::tail ->
        let transport, packages = moveTransport head packages
        let tail, packages = moveTransports tail packages
        transport::tail, packages
        
let areAllPackagesAtDestination packages =
    let isAtDestination = function
        | { Position = Place Destination_A } -> true
        | { Position = Place Destination_B } -> true
        | _ -> false
    packages |> List.forall isAtDestination 

let deliver (packages: Destination list) : int =
    let packages = packages |> List.map (fun destination -> { Destination = destination; Position = Place Factory })
    
    let rec deliver_inner packages transports generation =
        let transports, packages = moveTransports transports packages
        
        if areAllPackagesAtDestination packages
        then generation
        else deliver_inner packages transports (generation + 1)
    
    deliver_inner packages initial_transports 0
    
    
[<Fact>]
let ``Should compute delivery duration (A)`` () = Assert.Equal(5, deliver [A])
    
[<Fact>]
let ``Should compute delivery duration (A; B)`` () = Assert.Equal(5, deliver [A; B])
    
[<Fact>]
let ``Should compute delivery duration (B; B)`` () = Assert.Equal(5, deliver [B; B])
    
[<Fact>]
let ``Should compute delivery duration (A; B; B)`` () = Assert.Equal(7, deliver [A; B; B])
    
[<Fact>]
let ``Should compute delivery duration (A; A; B; A; B; B; A; B)`` () = Assert.Equal(29, deliver [A; A; B; A; B; B; A; B])
 