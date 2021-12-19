# Advent of code 2021

My solutions to 2020 Advent of Code. For Haskell files I have not uploaded all the stack gubbins to the repository instead just the `Main.hs` file. 

## Update

Yes we are lagging behind! My dissertation takes priority and I cannot for the life of me fix this one bug in my back-end interpreter. I'm sure we will finish this someday.
### *The update's update.
My dissy is going much better so I am back to not doing it... probably not the best decision but we move

## Run Instructions

To create the stack file install `stack` using

```
curl -sSL https://get.haskellstack.org/ | sh
```

Then create a new stack file for the day using:

```
stack new day[x]
```
Replace the contents of `app/Main.hs` with the Haskell code in this repository and run with

```
stack run
```

If a specific function wants to be tested out please use 

```
stack repl
```

Sometimes the solutions are pretty shoddy - on day 5 I had a Pasanda cooking away and so didn't have time to implement the most efficient solution. The curry was delish though!

## Disclosure
The code in this git repository is the copyright of Joe Moore and distribution or use is not allowed without explicit permission and without giving full credit
