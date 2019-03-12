entity a is

  port (
    i : in boolean := true
    );

end entity a;

architecture functional of a is

begin  -- architecture functional

  b1 : entity work.b
    port map (i => i);

end architecture functional;
