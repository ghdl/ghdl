entity paren is
end paren;

architecture behav of paren is
  constant cst : natural := 5;
begin
  process
  begin
    if (cst = 3 then
          null;
    end if;
  end process;
end behav;
