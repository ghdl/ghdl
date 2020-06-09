library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity onehot_aggregate_pmux is
    Port(sel: in STD_LOGIC_VECTOR(5 downto 0);
        input_choice: in STD_LOGIC_VECTOR(11 downto 0);
        input_default: in STD_LOGIC_VECTOR(1 downto 0);
        output_selected: out STD_LOGIC_VECTOR(1 downto 0));
end entity;

architecture Behavioral of onehot_aggregate_pmux is
begin
    process(sel, input_choice, input_default) is
    begin
        case sel is
            when (0 => '1', others => '0') =>
                output_selected <= input_choice(1 downto 0);
            when (1 => '1', others => '0') =>
                output_selected <= input_choice(3 downto 2);
            when (2 => '1', others => '0') =>
                output_selected <= input_choice(5 downto 4);
            when (3 => '1', others => '0') =>
                output_selected <= input_choice(7 downto 6);
            when (4 => '1', others => '0') =>
                output_selected <= input_choice(9 downto 8);
            when (5 => '1', others => '0') =>
                output_selected <= input_choice(11 downto 10);
            when others =>
                output_selected <= input_default;
        end case;
    end process;
end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity tb is
end;

architecture behav of tb is
  signal sel             : STD_LOGIC_VECTOR(5 downto 0);
  signal input_choice    : STD_LOGIC_VECTOR(11 downto 0);
  signal input_default   : STD_LOGIC_VECTOR(1 downto 0);
  signal output_selected : STD_LOGIC_VECTOR(1 downto 0);
begin
  dut: entity work.onehot_aggregate_pmux
    port map (
      sel             => sel,
      input_choice    => input_choice,
      input_default   => input_default,
      output_selected => output_selected);

  process
  begin
    input_choice <= b"11_10_01_00_HH_LL";
    input_default <= "ZZ";

    sel <= "000001";
    wait for 1 ns;
    assert output_selected = "LL" severity failure;

    sel <= "000010";
    wait for 1 ns;
    assert output_selected = "HH" severity failure;

    sel <= "000100";
    wait for 1 ns;
    assert output_selected = "00" severity failure;

    sel <= "100100";
    wait for 1 ns;
    assert output_selected = "ZZ" severity failure;

    wait;
  end process;
end;
