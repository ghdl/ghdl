entity linecont_same_column is
  port (
    a : in bit; -- first documentation line
                -- second documentation line
    b : in bit; -- first documentation line
                  -- not a continuation (another column)
    c : in bit
  );
end entity;
