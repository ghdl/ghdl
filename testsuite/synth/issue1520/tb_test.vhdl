entity tb_test is
end tb_test;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_test is
  signal slv           : std_logic_vector(7 downto 0);
  signal sl            : std_logic;
  signal int           : natural;
  signal vec_scal_and  : std_logic_vector(7 downto 0);
  signal vec_scal_nand : std_logic_vector(7 downto 0);
  signal vec_scal_or   : std_logic_vector(7 downto 0);
  signal vec_scal_nor  : std_logic_vector(7 downto 0);
  signal vec_scal_xor  : std_logic_vector(7 downto 0);
  signal vec_scal_xnor : std_logic_vector(7 downto 0);
  signal scal_vec_and  : std_logic_vector(7 downto 0);
  signal scal_vec_nand : std_logic_vector(7 downto 0);
  signal scal_vec_or   : std_logic_vector(7 downto 0);
  signal scal_vec_nor  : std_logic_vector(7 downto 0);
  signal scal_vec_xor  : std_logic_vector(7 downto 0);
  signal scal_vec_xnor : std_logic_vector(7 downto 0);
  signal slv_sll       : std_logic_vector(7 downto 0);
  signal slv_srl       : std_logic_vector(7 downto 0);
begin
  dut: entity work.test
    port map (
      slv           => slv,
      sl            => sl,
      int           => int,
      vec_scal_and  => vec_scal_and,
      vec_scal_nand => vec_scal_nand,
      vec_scal_or   => vec_scal_or,
      vec_scal_nor  => vec_scal_nor,
      vec_scal_xor  => vec_scal_xor,
      vec_scal_xnor => vec_scal_xnor,
      scal_vec_and  => scal_vec_and,
      scal_vec_nand => scal_vec_nand,
      scal_vec_or   => scal_vec_or,
      scal_vec_nor  => scal_vec_nor,
      scal_vec_xor  => scal_vec_xor,
      scal_vec_xnor => scal_vec_xnor,
      slv_sll       => slv_sll,
      slv_srl       => slv_srl);

  process
  begin
    slv <= x"c5";
    sl <= '0';
    int <= 2;

    wait for 1 ns;
    assert vec_scal_and = x"00" severity failure;
    assert vec_scal_nand = x"ff" severity failure;
    assert vec_scal_or = x"c5" severity failure;
    assert vec_scal_nor = x"3a" severity failure;
    assert vec_scal_xor = x"c5" severity failure;
    assert vec_scal_xnor = x"3a" severity failure;

    assert scal_vec_and = x"00" severity failure;
    assert scal_vec_nand = x"ff" severity failure;
    assert scal_vec_or = x"c5" severity failure;
    assert scal_vec_nor = x"3a" severity failure;
    assert scal_vec_xor = x"c5" severity failure;
    assert scal_vec_xnor = x"3a" severity failure;

    assert slv_sll = x"14" severity failure;
    assert slv_srl = x"31" severity failure;


    sl <= '1';
    int <= 3;

    wait for 1 ns;
    assert vec_scal_and = x"c5" severity failure;
    assert vec_scal_nand = x"3a" severity failure;
    assert vec_scal_or = x"ff" severity failure;
    assert vec_scal_nor = x"00" severity failure;
    assert vec_scal_xor = x"3a" severity failure;
    assert vec_scal_xnor = x"c5" severity failure;

    assert scal_vec_and = x"c5" severity failure;
    assert scal_vec_nand = x"3a" severity failure;
    assert scal_vec_or = x"ff" severity failure;
    assert scal_vec_nor = x"00" severity failure;
    assert scal_vec_xor = x"3a" severity failure;
    assert scal_vec_xnor = x"c5" severity failure;

    assert slv_sll = x"28" severity failure;
    assert slv_srl = x"18" severity failure;

    wait;
  end process;
end behav;
