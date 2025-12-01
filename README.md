# Advent of Code

These are my solutions to [Advent of Code](https://adventofcode.com/) (2015–Present), an annual Advent calendar of programming puzzles created by Eric Wastl.

**Author of these solutions:** Golapri Rose Ila Dasgupta (formerly Pritika Dasgupta)

*Sidenote:* I'll probably just put "GRID" for the author in my scripts since that's my very convenient and cute acronym for myself.

**Collaborators:** I'd rather not have any other collaborators at the moment. This is really a sandbox/playground for me to do a fun coding activity and to learn. Also, I realize that this is an awkwardly intense thing to say, but in the past, my ex helped a bit (if you read this, please do not contribute. And don't talk to me ever again. Thanks.).

**Programming Languages:** R, Python, MATLAB, and whatever I'm learning at the moment

## What's inside

Solutions are organized by year and day:

```
adventofcode/
├── 2015/
│   ├── Day1/
│   ├── Day2/
│   └── ...
├── 2020/
│   └── ...
├── 2025/
│   └── Day1/
└── ...
```

Each day typically includes the solution script(s) and my puzzle input. I don't include puzzle text per Advent of Code's request; you can find that at the official link above.

## My Progress

Each puzzle has two parts, earning one ⭐ each. You can see your progress [here](https://adventofcode.com/2025/events). Here's my progress:

| Year | Stars | Notes |
|------|-------|-------|
| 2025 | ⬜ / 24 | Starts Dec 1! (only 12 puzzles starting this year) |
| 2024 | ⬜ / 50 | |
| 2023 | 2⭐ / 50 | |
| 2022 | ⬜ / 50 | |
| 2021 | 50⭐ / 50 | ✅ Complete! |
| 2020 | 50⭐ / 50 | ✅ Complete! (AoC++) |
| 2019 | 12⭐ / 50 | |
| 2018 | ⬜ / 50 | |
| 2017 | ⬜ / 50 | |
| 2016 | ⬜ / 50 | |
| 2015 | 41⭐ / 50 | |

**Total: 155⭐**

## Running the R solutions

**Older solutions (2015-2021):** These can be run by sourcing from the repo root:

```r
source("2020/Day19/monsterMessages_part1.R")
```

**2025+ solutions:** These use `template.R` and take the input file as an argument:

```bash
Rscript 2025/Day1/day01.R 2025/Day1/input.txt
```

Or interactively in R:

```r
source("2025/Day1/day01.R")
main("2025/Day1/input.txt")
```

Common dependencies include `tidyverse`, `data.table`, `glue`, `here`, and `digest`. New 2025+ R code is going to use only a minimal subset per puzzle, declared at the top of each script.

## The 2025 event

The 2025 event starts December 1, 2025 at midnight EST. Puzzles unlock daily through mid-December. (There are only 12 puzzles this year!) I'll be updating this repo as I work through them.

## Style notes

My older solutions (2015-2021) were written for speed, often late at night while finishing my PhD. They're functional but hacky - like, my variable names are inconsistent, and I prioritized "does it work" over "is it pretty."

Starting in 2025, I'm using a cleaner template (`template.R`) with:

- Proper function structure (`main()`, `solve_part1()`, `solve_part2()`)
- Logging and input validation
- `snake_case` naming
- Reusable parsing helpers

We'll see how long that lasts....

**Retrofitting an old solution:** 

As an experiment, I refactored `2021/Day4/day04.R` (the Giant Squid bingo puzzle) using the new template structure. It's a good example of what the cleaner format looks like if you want to compare it against my original hacky versions.

## About Advent of Code

Advent of Code is a cute Christmas-themed series of small programming puzzles for a variety of skill levels. I'd love to see people try Microsoft Excel, Google Sheets, bash one-liners, or whatever hacky unconventional method that sparks joy. You don't necessarily need a CS background, just programming basics and problem-solving. Every puzzle has a solution that completes in under 15 seconds on modest hardware.

People use these puzzles for interview prep, learning new languages, coursework, or just for fun. The creator asks that participants solve puzzles themselves rather than relying on LLMs and generative AI. The point is to learn and grow as a programmer and thinker.

Some people even just make art from the puzzle text!!! I haven't yet (maybe this year with my nonexistent time?), but it sounds like a fun thing to do.

Sometimes, people compete with each other [(private leaderboards or the global leaderboard!).](https://adventofcode.com/2025/leaderboard/private)

## Personal Notes

I used to share my solutions in the Twitter community back in the day, but I no longer use my Twitter. Perhaps I will share on my [Substack](https://drrosedasgupta.substack.com/) or [Bluesky](https://bsky.app/profile/drrosedasgupta.bsky.social). I used to use [Carbon](https://carbon.now.sh/) to make images of my source code so I could post on social media.

As I mentioned earlier, a lot of my previous solutions were in attempts to finish as fast as I could. I often did these with very limited sleep and capacity as I was finishing my PhD at the time. My previous solutions were very hacky (I mean! I did try to edit them when I could, lol).

Anyway, learn more: https://adventofcode.com/2025/about