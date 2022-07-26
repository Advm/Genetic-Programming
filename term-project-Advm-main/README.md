# PushGP

Term project for my genetic programming class. The project takes in a database that signifies a random assortment of data. The program will then evolve overtime to find a usefull query based on set conditions. For example, my project evolves to determine a query that returns all persons who are at a high probability of developing cancer. It uses conidtions such as age, smoking, sex, whether or not they had cancer,. etc. as a fitness evaluation

## Usage

There are two ways to run the main PushGP function:

1. Load `core.clj` into the interpreter, and then run `(-main)`.
2. From the command line, run `lein run`.

