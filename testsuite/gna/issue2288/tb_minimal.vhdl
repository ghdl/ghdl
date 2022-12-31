library ieee;
use ieee.std_logic_1164.all;

entity tb_minimal is
end tb_minimal;

architecture rtl of tb_minimal is
begin
  MAIN_PROC : process

    function fun return std_ulogic_vector is
      variable v_return : std_ulogic_vector(0 to 3) := x"A";
    begin
      return v_return;
    end function fun;

    variable v_result       : std_ulogic_vector(0 to 7);
    variable v_some_data    : std_ulogic_vector(0 to 3) := x"B";
    variable v_fun_result : std_ulogic_vector(0 to 3);
  begin

    -- working:
    -- v_fun_result := fun;
    -- v_result     := std_ulogic_vector'(v_some_data, v_fun_result);

    -- not working:
    v_result       := std_ulogic_vector'(v_some_data, fun);

    report "result= 0x" & to_hstring(v_result);
    assert v_result = x"BA" severity failure;
    wait;
  end process;

end architecture;
