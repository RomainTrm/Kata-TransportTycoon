module Tests

open Xunit

type Destination = A | B
type TransportId = int
type Place = Factory | Port | Destination_A | Destination_B
type PackagePosition =
    | InTransport of TransportId
    | Place of Place
type Package = { Destination: Destination; Position: PackagePosition }

type EAT = int
type PathSegment = { Start: Place; End: Place; EAT: EAT }

type AtHomeBase = { TransportId: TransportId; Place: Place }
type MovingToPlace = { TransportId: TransportId; HomeBase: Place; Destination: Place; EAT: EAT; EAT_ToHomeBase: EAT }
type ReturningAtHomeBase = { TransportId: TransportId; HomeBase: Place; EAT: EAT }
type Transport =
    | AtHomeBase of AtHomeBase
    | MovingToPlace of MovingToPlace
    | ReturningAtHomeBase of ReturningAtHomeBase

let deliver (packages: Destination list) : int =
    let packages = packages |> List.map (fun destination -> { Destination = destination; Position = Place Factory })
    
    let transports = [
        AtHomeBase { TransportId = 1; Place = Factory }
        AtHomeBase { TransportId = 2; Place = Factory }
        AtHomeBase { TransportId = 3; Place = Port }
    ]
    
    let paths = dict[
        A, [{ Start = Factory; End = Port; EAT = 1 }; { Start = Port; End = Destination_A; EAT = 4 }]
        B, [{ Start = Factory; End = Destination_B; EAT = 5 }]
    ]

    0
    
[<Fact>]
let ``Should compute delivery duration (A)`` () = Assert.Equal(5, deliver [A])
    
[<Fact>]
let ``Should compute delivery duration (A; B)`` () = Assert.Equal(5, deliver [A; B])
    
[<Fact>]
let ``Should compute delivery duration (B; B)`` () = Assert.Equal(5, deliver [B; B])
    
[<Fact>]
let ``Should compute delivery duration (A; B; B)`` () = Assert.Equal(7, deliver [A; B; B])
    
 