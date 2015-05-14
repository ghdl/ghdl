entity ent is
end entity;

architecture a of ent is
begin
  main : process is
    constant str : string(1 to 3) := "abc";
    type line is access string;
    variable l : line;
  begin
    l := new str(1 to 2); -- Crashes
    l := new string'(str(1 to 2)); -- Works
  end process;
end architecture;
