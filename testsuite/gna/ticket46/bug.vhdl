entity ent is
end entity;

architecture a of ent is
  type boolean_vec_t is array (integer range <>) of boolean;

  function resolved(vec : boolean_vec_t) return boolean is
  begin
    return true;
  end function;

  subtype resolved_boolean_t is resolved boolean;

  signal sig : resolved_boolean_t;

begin
  main : process
  begin
    if sig then
    end if;

    wait until sig;
  end process;
end architecture;
