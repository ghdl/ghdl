entity hello is
end hello;

architecture behav of hello is
  component comp is
    port (a, b, c : bit);
  end component;
begin
  dut: comp
    port map (
      '0',
<<<<<<< HEAD
  '0',
=======
  '1',
>>>>>>> bad (no comment)
  '1'
      );
end behav;
