-- Author: Patrick Lehmann
--
-- Labels as written in the source: they were returned lower-cased.
entity Leaf is end entity;
architecture a of Leaf is begin end architecture;
entity Top is end entity;
architecture rtl of Top is
  signal S : bit;
begin
  U1: entity work.Leaf;
  ProcA : process begin wait; end process;
  BlkX: block begin end block;
  GenIf: if True generate
  begin end generate;
  GenCase: case 1 generate
    when AltOne: 1 => begin end;
    when others => begin end;
  end generate;
  Asgn_Lbl: S <= '1';
  process begin
    SeqLoop: for i in 0 to 1 loop end loop;
    wait;
  end process;
end architecture;
