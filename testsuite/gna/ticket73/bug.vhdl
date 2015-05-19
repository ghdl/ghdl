entity ent is
end entity;

architecture a of ent is
  procedure proc(s : string) is
  begin
    report integer'image(s'left);
    report integer'image(s'right);
    report s;
  end procedure;
begin
  main : process
  begin
    proc(s(4 to 15) => "Hello world!");
    wait;
  end process;
end architecture;
