
let sigmoid x =
    1. / (1. + exp -x)

let sigmoidDerivative x =
    let fx = sigmoid(x)
    fx * (1. - fx)

let errorCost actual predicted =
    pown (actual - predicted) 2

let errorCostDerivative actual predicted =
    2. * (actual - predicted)

let arrayToFloat arr =
    arr |> Array.map (fun (x1, x2) -> (float x1, float x2))

let trainingData = arrayToFloat [|(180, 80); (170, 75); (200, 110); (189, 80); (100, 30); (70, 20); (90, 30); (60, 20)|];
let expectedOutput = [|1.; 1.; 1.; 1.; 0.; 0.; 0.; 0.|];

let rand = System.Random(2);
let mutable w1 = -rand.NextDouble();
let mutable w2 = rand.NextDouble();

let train iterations step =
    //let step = 0.001;

    for i = 0 to iterations do
        trainingData |> Array.iteri (fun i (height, weight) ->

            let sumValue = height * w1 + weight * w2;
            let output = sigmoid sumValue;
            let errorDerivative = errorCostDerivative output expectedOutput.[i];

            w1 <- w1 - step * errorDerivative;
            w2 <- w2 - step * errorDerivative;
            ()
        )

    printfn "%f %f" w1 w2 

    trainingData |> Array.iter (fun (height, weight) ->
        let sumValue = height * w1 + weight * w2
        let output = sigmoid sumValue

        printfn "%f" output
    )

    printfn "\n"

    let testSet = arrayToFloat [|(180, 90); (60, 20); (190, 100); (40, 10)|];

    testSet |> Array.iter (fun (height, weight) ->
        let sumValue = height * w1 + weight * w2
        let output = sigmoid sumValue

        printfn "%f" output
    )

    0

let clasify (height, weight) =
    let sumValue = height * w1 + weight * w2;
    let output = sigmoid sumValue;
    printfn "output = %f" output
    match output with
    | value when value >= 0.5 -> printfn "Adult"
    | _ -> printfn "Child"


train 10000 0.001

clasify (175, 75)

clasify (80, 30)

[|(180, 90); (60, 20); (190, 100); (40, 10)|] |> Array.iter (fun (height, weight) -> clasify (height, weight))