

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity temp is
    generic (
        bank_count_log2b : natural := 0
    );
end entity;

architecture behaviourial of temp is
    subtype age_type is unsigned(- 1 downto 0);
    constant max_age : age_type := (others => '1');
    constant min_age : age_type := (others => '0');
begin
    assert(max_age = min_age);
      process begin
        report "Hello world" severity note;
        wait;
      end process;
end architecture;
