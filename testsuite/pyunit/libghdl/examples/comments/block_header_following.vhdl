package p is
  type another_column is record  -- first line
    -- for x
    x : bit;
  end record;

  type empty_comment is record  -- first line
                                --
                                -- third line
    x : bit;
  end record;

  type empty_line is record  -- first line

                             -- for x
    x : bit;
  end record;

  type block_rule is record  -- first line
    -- followed by an empty line

    -- for x
    x : bit;
  end record;

  type interrupted is record  -- first line
    -- another column
                              -- not a continuation
    x : bit;
  end record;
end package;
