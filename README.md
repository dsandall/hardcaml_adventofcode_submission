This is my solution for Day 1, part 1 of advent of code 2025! (that is, it will only count when the dial lands on 0, rather than the number of times it passes zero)
It's also my introduction to ocaml and hardcaml. Ocaml syntax was a little different than what I am used to, but pretty neat. It makes a lot of sense as a metaprogramming language for RTL design (far better than systemverilog macros), and I plan on using it again.

Usage:
- Change test/input.txt, and run ``dune build bin/generate.exe @runtest``
- Result can be viewed in the waveform viewer, or in terminal output. You know the deal!
