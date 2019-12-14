library ieee;
use ieee.std_logic_1164.all;

entity test2 is
  port (
    tx : out std_logic);
end entity;

architecture tb of test2 is
begin
    process
        -- variable boolobj: boolean; -- ADDED
        procedure transmit(data: std_logic_vector; signal tx: out std_logic) is
            variable norm: std_logic_vector(data'length - 1 downto 0) := data;

            procedure send(value: std_logic) is
            begin
                tx <= value;
                wait for 10 ns;
            end procedure;
        begin
        -- report " boolobj = " & boolean'image(boolobj); -- ADDED
            for i in norm'reverse_range loop
                send(norm(i));
                report integer'image(i); --- to_string(i); -- CHANGED
            end loop;
        end procedure;
        variable norm: std_logic_vector(7 downto 0); -- ADDED
    begin
        transmit(x"55", tx);
        for i in norm'reverse_range loop     -- ADDED Loop statement
            report "i = " & integer'image(i);
        end loop;
        wait;
    end process;
end architecture;
