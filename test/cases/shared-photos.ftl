# Simple things are simple.
hello-user = Hello, {$userName}!

possessive = {$userGender ->
     [male] his
     [female] her
    *[other] their
}

# Complex things are possible.
shared-photos =
    {$userName} {$photoCount ->
        [one] added a new photo
       *[other] added {$photoCount} new photos
    } to {possessive} stream.

nth-photo =
    {$userName} added {possessive} {$photoCount ->
         [one] first
        *[other] {ORDINAL($photoCount) ->
            [one] {$photoCount}st
            [two] {$photoCount}nd
            [few] {$photoCount}rd
           *[other] {$photoCount}th
        }
    } photo
