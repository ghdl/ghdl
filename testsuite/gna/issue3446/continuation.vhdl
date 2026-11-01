entity continuation is
  port (
    a1 : in bit; -- first documentation line of a1
                 -- second documentation line of a1
    b1 : in bit; -- first documentation line of b1

                 -- not part of the documentation of b1
    c1 : in bit; -- first documentation line of c1
    -- some comment
                 -- some other comment
    d1 : in bit; -- first documentation line of d1
                 --
                 -- third documentation line of d1
    e1 : in bit; -- first documentation line of e1

    -- first documentation line for f1
    f1 : in bit; -- first documentation line of f1
                   -- first documentation line of g1 (another column)
    g1 : in bit
  );
end entity;
