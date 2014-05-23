---
title: Loopy
date: 2010-01-31
category: games
---

	
Also called Slither Link or Loop The Loop, Loopy is one of the many
types of puzzles that came out of Japan. It is part of [Simon Tatham's
Puzzle
Collection](http://www.chiark.greenend.org.uk/~sgtatham/puzzles/), and
can be played online on [his
site](http://www.chiark.greenend.org.uk/~sgtatham/puzzles/java/loopy.html),
at [Nikoli](http://www.nikoli.com/en/puzzles/slitherlink/) and on
[puzzle-loop.com](http://puzzle-loop.com/).

![](/images/loopy/loopy_trim.gif)

## Rules

The goal of the game is to draw a single loop that never crosses
itself, with a simple restriction: squares containing a number must
have exactly that many sides that are part of the loop.

## Simple Properties

Some simple properties can be easily derived from the rules:

* Every dot touches exactly zero or two segments of the loop.
* Any line or squiggle that crosses the board will intersect the loop an even amount of times.

## Patterns

Tatham's downloadable version of the game is the best version I could
find, but it does not include a timer nor does it keep score like
minesweeper does on windows. Speed runs are still possible, but
intimate knowledge of many types of patterns is required.

## Corner Patterns

Corner patterns usually appear in the corners, but some of them also
apply when the numbered square has a corner in common with a 0. The
patterns for a 1 and a 3 are easy, but it is a little less obvious for
2.

![](/images/loopy/corners-1.png)

In Tatham's version and in other versions with a unique solution, when
a 2 is completely alone in a corner, 6 segments can be filled in.

![](/images/loopy/corners-2.png)

## Side Pattern

When a 3 and a 1 are together on the side, another simple deduction can be made.

![](/images/loopy/side-1.png)

## Patterns with 3

The 3 square is very special because all but one of its sides are in
the loop. It is often easy to predict two of it's sides.

A dot with two diagonal 1s and a 3 will always be part of the
loop. The 1s always shoot inward.

![](/images/loopy/3-1-1.png)

When 3s touch diagonally, they shoot at each other and fly away from
each other.

![](/images/loopy/3-3.png)

When they share a side, they make a parallel formation. The two
segments that continue the middle segment are never part of the loop.

![](/images/loopy/3-3-3.png)

## A little vocabulary

I invented my own vocabulary to make discussing loopy easier.

Shooting:
  When two adjacent sides of a square contain one and
  exactly one segment of the loop.

Flying:
  When two adjacent sides of a square are both part of
  the loop. The shape looks like an arrow, a plane or a bird.

Turret:
  A 2 with a side that is known not to be part of the
  loop.

This vocabulary allows me to say that 2s shoot sideways when they fly
and that 3s fly away when they are shot.

## Patterns with 2

The interesting thing with 2s is that shooting and flying propagates
along diagonal 2s.

For example, two 3s connected by 2s can still shoot at each other and
fly away.

![](/images/loopy/3-2-2-3.png)


A 2 in a corner or diagonally across from a 0 is a flying 2. It flies
towards the corner or away from it, and it shoots sideways.

It is impossible to fly into a 3, so flying 2s always fly away from 3s
that are directly in their path.

![](/images/loopy/2-3-Fly.png)


Flying 2s shoot sideways at 3s that are on the side.

![](/images/loopy/2-3-Shoot.png)

A 3 that has a side in common with a flying 2 will fly away, at a 90
degree angle with the flight path.

![](/images/loopy/0-2-2-3.png)

## Turrets

It is common to have a 2 with a side that is not part of the loop. I
call them turrets. Turrets behave like 3s because all but one of their
unknown sides are part of the loop.

Like 3s, turrets love to shoot at each other and at 3s.

Unlike 3s, turrets cannot fly away. When they shoot on one side they
get a leg on the other side.

Turrets automatically appear when a 2 shares a side with a 0. In this
example, a turret and a 3 are shooting at each other.

![](/images/loopy/2-3-Turret.png)

A 3 on top of a turret (that is, on the opposite side of the turret's
empty side) always has a segment of the loop on the side opposite of
the turret.

![](/images/loopy/0-2-3.png)

## Combining Patterns

A parallel formation of 3s next to a few well placed 2s often creates
a nice avalanche of turrets.

![](/images/loopy/3-3-2-2-2.png)

## Other inferred rules

There are a lot of other patterns, I won't mention hem all. They can
all be discovered by simply applying the rules or by using brute
force.

For example:

Shooting a wall turns the wall into a segment of the loop.

Flying into a wall or away from a wall removes the side on the wall
  from the loop.

Shooting a 1 removes it's opposite sides from the loop.

Flying into a 1 or away from a 1 makes it shoot away from you.

When a segment is in line to shoot a turret or a 3, it always
  does. The other side is not part of the loop.

When a segment is in line with a flying 2, it flies towards the 2 and
  the 2 flies away.

Have fun!