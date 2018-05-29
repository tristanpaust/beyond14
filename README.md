# Beyond 14
Haskell clone of the great mobile game "Beyond 14" ( [iOS](https://itunes.apple.com/tt/app/beyond-14/id1097469600?mt=8) / [Android](https://play.google.com/store/apps/details?id=com.mojoforest.beyondfourteen&hl=en_US) ), using Gloss.

![Beyond 14 sample](https://github.com/tristanpaust/beyond14/blob/master/assets/beyond14_sample.png "Beyond 14 sample")

Run with 

```bash
cabal sandbox init
cabal install --dependencies-only
cabal run
```

TODO: 

- New numbers are currently generated only from numbers that are on the board already. 
  This makes it a bit too easy, as no small numbers show up randomly.

- There is an infinite amount of destroy, clone, rewind and reshuffle, which also makes it very easy.

- I did not have the time to figure out how different numbers are counted differently, how much combos additionally count, etc. 
  (Currently everything is counted as 10, no matter what the value is, and whether it is a combo or not)

- The top progess bar, which gives additional special moves after a while is missing.

- Bigger numbers should be scaled down to make sure they aren't getting larger than the tile itself.

- The bottom buttons for special moves should have images instead of letters