# Schulze flavor of the Condorcet method for voting

## Prototype
So far I have a prototype that checks against the wikipedia entry for the [Schulze_method](https://en.wikipedia.org/wiki/Schulze_method)

To run:

    mvn package
    java -jar prototype/target/schulze.jar data/wikipedia-schulze-sample.txt

The output:

    Matrix of pairwise preferences
    ╔═══════╤═════╤════╤═══════╤═════╤════╗
    ║       │alpha│beta│charlie│delta│echo║
    ╟───────┼─────┼────┼───────┼─────┼────╢
    ║alpha  │    0│  20│     26│   30│  22║
    ╟───────┼─────┼────┼───────┼─────┼────╢
    ║beta   │   25│   0│     16│   33│  18║
    ╟───────┼─────┼────┼───────┼─────┼────╢
    ║charlie│   19│  29│      0│   17│  24║
    ╟───────┼─────┼────┼───────┼─────┼────╢
    ║delta  │   15│  12│     28│    0│  14║
    ╟───────┼─────┼────┼───────┼─────┼────╢
    ║echo   │   23│  27│     21│   31│   0║
    ╚═══════╧═════╧════╧═══════╧═════╧════╝
    Strengths of the strongest paths
    ╔═══════╤═════╤════╤═══════╤═════╤════╗
    ║       │alpha│beta│charlie│delta│echo║
    ╟───────┼─────┼────┼───────┼─────┼────╢
    ║alpha  │    0│  28│     28│   30│  24║
    ╟───────┼─────┼────┼───────┼─────┼────╢
    ║beta   │   25│   0│     28│   33│  24║
    ╟───────┼─────┼────┼───────┼─────┼────╢
    ║charlie│   25│  29│      0│   29│  24║
    ╟───────┼─────┼────┼───────┼─────┼────╢
    ║delta  │   25│  28│     28│    0│  24║
    ╟───────┼─────┼────┼───────┼─────┼────╢
    ║echo   │   25│  28│     28│   31│   0║
    ╚═══════╧═════╧════╧═══════╧═════╧════╝
