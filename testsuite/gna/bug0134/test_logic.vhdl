entity test_logic is
   port(
      a, b, c, d : in  bit;
      g : out bit
   );
end test_logic;

architecture structure of test_logic is
    -- function "not" (i: bit) return bit is
    -- begin
    --     if i = '1' then
    --         return '0';
    --     else
    --         return '1';
    --     end if;
  -- end function "not";
  component AND1
    port(s, t : in bit;
         u : out bit
         );
  end component;
  component OR1
    port(x, y, z : in bit;
         n : out bit
         );
  end component;
  signal e, k, h : bit;
begin
    x1: AND1 port map(s => "not"(a),
          t => "not"(d),
          u => e);
    x2: AND1 port map(s => "not"(b),
          t => "not"(d),
          u => k);
    x3: AND1 port map(s => a,
          t => d,
          u => h);
    x4: OR1 port map(x => e,
           y => k,
           z => h,
           n => g);
end structure;
