entity linecont_column is
  generic (
    g : natural := 1;  --  Comment for :g:
                       --  Also for :g: (indented after g)
    --  Comment for :h: (not indented after g)
    h : natural := 2
  );
end entity;
