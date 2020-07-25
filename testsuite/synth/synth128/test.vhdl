library ieee;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity test is
  port (
    resb: 		in std_logic;
    clk_FF:		in std_logic;
    ADD_FF: 		in unsigned(1 downto 0);
    CONFIG: 		in std_logic;
    D_FF: 		in std_logic;
    WE:			in std_logic;
    EN_signal:		out std_logic
    );
end test;

architecture test_a of test is
signal Q_FF: std_logic_vector(0 to 1);
begin
    process(resb, clk_FF)
    begin
	if resb = '0' then
	    Q_FF <= "00";
	elsif clk_FF'event and clk_FF = '1' then
		if WE = '1' then
			Q_FF(to_integer(ADD_FF)) <= D_FF;
		end if;
	end if;
     end process;

    process(CONFIG, Q_FF)
    begin
        if CONFIG = '1'  then
            EN_signal <=  Q_FF(0);
         else
            EN_signal <= '0';
         end if;
      end process;
end;
