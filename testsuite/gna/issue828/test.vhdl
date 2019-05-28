library ieee;
use ieee.std_logic_1164.all;

entity test is
  port(
    tx : out std_logic);
end entity;

architecture tb of test is
begin
  process

    procedure transmit(data : std_logic_vector;
                       signal tx : out std_logic) is

      variable norm : std_logic_vector(data'length - 1 downto 0) := data;

      procedure send(value : std_logic) is
      begin
        tx <= value;
        wait for 10 ns;
      end procedure;
      variable count : natural := 0;
    begin
      report natural'image (norm'left);
      report natural'image (norm'right);
      for i in norm'reverse_range loop
        send(norm(i));
        report to_string(i);
        count := count + 1;
      end loop;
      assert count = 8 severity failure;
    end procedure;

  begin

    transmit(x"55", tx);
    wait;

  end process;
end architecture;
