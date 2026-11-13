entity repro is
  generic (n : natural := 2);
  port (i : in bit := '0'; o : out bit_vector (n - 1 downto 0));
end repro;

architecture tb of repro is
  component repro_comp is
    port (i : in bit; o : out bit);
  end component;
begin
  g : for k in 0 to n - 1 generate
    u : repro_comp port map (i => i, o => o (k));
  end generate;
end tb;

--  The bound entity is on lines after the top-level entity, so the
--  elaboration subprogram starts on a line after its end.
--
--
--
--
--
--
--
--
entity repro_comp is
  port (i : in bit; o : out bit);
end repro_comp;

architecture rtl of repro_comp is
begin
  o <= i;
end rtl;
