entity ent2 is
end entity;

architecture a of ent2 is
begin
  main : process is
    type enum_t is (value1, value2);
    variable var : enum_t;
  begin
    var := enum_t'rightof(value1); -- Works
    var := enum_t'leftof(value2); -- Works
    var := enum_t'rightof(var); -- cannot handle IIR_KIND_RIGHTOF_ATTRIBUTE
    var := enum_t'leftof(var);  -- cannot handle IIR_KIND_LEFTOF_ATTRIBUTE
    wait;
  end process;
end architecture;
