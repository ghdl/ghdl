

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--HDLRegression:TB
entity demo_tb is
end entity;

architecture tb of demo_tb is

  type t_unsigned_vector is array (natural range <>) of unsigned;
  type t_unsigned_array is array (natural range <>) of t_unsigned_vector;

  procedure test_procedure_1(
    constant value     : in unsigned;
    constant range_vec : in t_unsigned_vector) is
  begin
    report "Executing test_procedure_1";
  end procedure;

  procedure test_procedure_2(
    constant value     : in unsigned;
    constant range_vec : in t_unsigned_array) is
  begin
    report "Executing test_procedure_2";
  end procedure;

begin

  p_main : process
    variable v_unsigned        : unsigned(3 downto 0);
    variable C_UNSIGNED_VECTOR : t_unsigned_vector(0 to 1)(3 downto 0) := (x"0", x"3");
    variable C_UNSIGNED_ARRAY  : t_unsigned_array(0 to 1)(0 to 1)(3 downto 0) :=  ((x"0", x"3"), (x"0", x"3"));
  begin

    test_procedure_1(v_unsigned, (x"0", x"3"));      -- OK
    test_procedure_1(v_unsigned, C_UNSIGNED_VECTOR); -- OK

    test_procedure_2(v_unsigned, (0 => (x"0", x"3")));      -- OK
    test_procedure_2(v_unsigned, (0 => C_UNSIGNED_VECTOR)); -- OK

    test_procedure_2(v_unsigned, (0 => (x"0", x"3"), 1 => (x"0", x"3")));           -- ERROR
    test_procedure_2(v_unsigned, (0 => C_UNSIGNED_VECTOR, 1 => C_UNSIGNED_VECTOR)); -- OK

    test_procedure_2(v_unsigned, ((x"0", x"3"), (x"0", x"3"))); -- ERROR
    test_procedure_2(v_unsigned, C_UNSIGNED_ARRAY);             -- OK

    report "END TB";
    std.env.stop;
    wait;
  end process p_main;

end architecture tb;
