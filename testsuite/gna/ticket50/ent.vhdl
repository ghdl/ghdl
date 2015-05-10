entity ent is
end entity;

architecture a of ent is
  type enum_t is (cond);
  impure function cond return boolean is
  begin
    return false;
  end function;
begin
  main : process
  begin
    if cond then
    end if;
  end process;
end architecture;
