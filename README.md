[![Build Status](https://secure.travis-ci.org/JohnLato/increments.png?branch=master)](http://travis-ci.org/JohnLato/increments)

increments
===========

Generically calculate incremental updates to data types.

What it does
============

Suppose you have a record type holding some state:

    {-# LANGUAGE DeriveGeneric #-}

    import GHC.Generics

    data Config = Config
      { descr     :: String    -- ^ changes rarely
      , startPos  :: (Int,Int) -- ^ constant
      -- these change more often
      , level     :: Maybe Int
      , looksOk   :: Bool
      } deriving (Generic)

    instance Incremental Config

Using increments, you can calculate a difference between values of the same
type:

    config1 = Config
      { descr = "config 1"
      , startPos = (0,0)
      , level = Nothing
      , looksOk = True
      }
    
    config2 = config1
      { level = Just 9001
      , looksOk = False
      }

    configDiff = changes config1 config2

    config3 = applyChanges config1 configDiff

At this point, `config2` and `config3` are exactly equal.

Why you'd use it
================

If you have a lot of state with many small changes over time, incremental
updates can save a lot of bandwidth by sending just small changes instead of
entire records.

How to use it
=============

Increments uses GHC Generics, so you'll need a GHC that supports the
`DeriveGeneric` extension.  Most of the time, all you need to do is `derive
Generic` for your types and add `instance Incremental`

Default-defined increments are automatically serializable via the `beamable`
package, which provides minimal-bandwidth encoding and decoding.

If you want to bypass the incremental updates for some records, an Incremental
instance can be manually-defined using the primitive functions exported from
`Data.Increments.Internal`.  Many built-in instances are written this way:

    instance Incremental Int where
        type Increment Int = DPrim Int
        changes = iprimDiff
        applyChanges = iprimApply

Caveats
=======

Incremental updates are not generally commutative due to sum types.  For
example:

    a :: Either Config Int
    a = Left config1

    b = Left config2
    c = Right 0

    problemDiff = changes a b

    -- this is an error
    applyChanges c problemDiff

Here `problemDiff` is an incremental update of a Config, and is expecting to be
applied to a `Left config` value.  If we instead apply it to a `Right int`,
there isn't a config the diff can be applied to.  The same problem can result
if an intermediate incremental update is lost.  If a problem requires
commutativity in the presence of sum types, one approach would be to create a
manual instance using primitive support as described above.
