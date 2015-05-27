entity ent is
end entity;

architecture a of ent is
begin
  main : process
    function foo return string is
      variable s : string(1 to 1); -- Causes exception 
    begin
      return (0 => '0');
    end function;
  begin
    wait;
  end process;
end architecture;
