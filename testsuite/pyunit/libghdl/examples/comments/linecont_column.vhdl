entity linecont_column is
  generic (
    g : natural := 1;  --  Comment for :g:
                       --  Also for :g: (same column)
    --  Comment for :h: (another column)
    h : natural := 2
  );
end entity;
