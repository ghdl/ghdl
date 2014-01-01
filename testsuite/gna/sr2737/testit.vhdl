entity testit is

end entity;

architecture behave of testit is 

  subtype shiftrange is integer range -8 to 8;
  signal input: bit_vector (1 to 5) := "11100";
  signal ror_val: bit_vector (1 to 5);
  signal rol_val: bit_vector (1 to 5);
  signal shft: shiftrange;

begin

process
begin
    for i in shiftrange loop 
        ror_val  <= input ror i;
        rol_val  <= input rol i;
        shft <= i;
        wait for 20 ns; -- a convenient length of time
    end loop;
    wait;
    
end process;

end architecture;
