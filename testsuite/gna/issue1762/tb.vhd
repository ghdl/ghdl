library ieee;
use ieee.numeric_bit.all;
use work.isa.all;

entity tb_register_bank is
end entity;

architecture test of tb_register_bank is
  signal s_result                   : data_word := (others => '0');
  signal s_select, b_select, a_select : bus_operand := r0;
  signal clk, write_enable          : bit := '0';
  signal rst                        : bit := '1';
  signal a_out, b_out               : data_word;

  procedure wr(signal cl : out bit) is
  begin
    cl <= '0'; wait for 10 ns; cl <= '1'; wait for 10 ns; cl <= '0'; wait for 10 ns;
  end procedure;
begin
  dut : entity work.register_bank
    port map (s_result => s_result, s_select => s_select, b_select => b_select,
              a_select => a_select, clk => clk, write_enable => write_enable,
              rst => rst, a_out => a_out, b_out => b_out);

  process
    variable v : data_word;
  begin
    -- write 0xBEEF into r1, 0x1234 into r5
    write_enable <= '1';
    s_result <= to_unsigned(16#BEEF#, 16); s_select <= r1; wr(clk);
    s_result <= to_unsigned(16#1234#, 16); s_select <= r5; wr(clk);
    write_enable <= '0';

    a_select <= r1; b_select <= r5; wait for 10 ns;

    assert a_out = to_unsigned(16#BEEF#, 16)
      report "FAIL a_out=" & integer'image(to_integer(a_out)) severity failure;
    assert b_out = to_unsigned(16#1234#, 16)
      report "FAIL b_out=" & integer'image(to_integer(b_out)) severity failure;

    report "PASS a_out=" & integer'image(to_integer(a_out)) &
           " b_out=" & integer'image(to_integer(b_out));
    wait;
  end process;
end architecture;
