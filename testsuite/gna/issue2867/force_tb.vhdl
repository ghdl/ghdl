library ieee;
  context ieee.ieee_std_context;
  use ieee.math_real.all;

entity force_tb is
  generic (
    CLOCK_PERIOD : time   := 10 ns
  );
end entity;

architecture simulation of force_tb is
  signal clock : std_logic := '0';
  signal data : std_ulogic_vector(7 downto 0) := (others => '0');
begin

  clock <= not clock after CLOCK_PERIOD/2;
  data <= (others => '1');

  test_ctrl: process

    procedure ForceToZero(signal output: in std_ulogic_vector) is
    begin
      output <= force (output'range => '0');
      wait for 100 ns;
      output <= release;
    end procedure;

  begin
    ForceToZero(data);
  end process;

end architecture;
