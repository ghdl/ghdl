entity ent is
end entity;

architecture a of ent is
  procedure set(x : integer; value : integer := 0) is
  begin
  end procedure;

  procedure set(x : integer; y : integer; value : integer := 0) is
  begin
  end procedure;

  procedure set(x : integer; y : integer; z : integer; value : integer := 0) is
  begin
  end procedure;
begin
  main : process
  begin
    set(0, value => 1); -- Works
    set(0, 1, value => 1); -- Does not work
  end process;
end architecture;
