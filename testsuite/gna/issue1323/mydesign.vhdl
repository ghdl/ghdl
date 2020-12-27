package mypackage is
  subtype constrained_bit_vector is bit_vector(4 downto 0);
end mypackage;

entity mysubentity is
  generic (
    genint: integer := 42;
    genstring: string := "fish"
    );
end entity;

architecture arch of mysubentity is

  constant const_bit : bit := '1';
  signal sig_bit : bit;

  type enum is (FIRST, SECOND, THIRD);
  constant const_enum : enum := SECOND;
  signal sig_enum : enum;

  constant const_bool : boolean := false;
  signal sig_bool : boolean;

  constant const_int : integer := 45;
  signal sig_int : integer;

  -- strings
  constant const_cs : string := "staticlength";
  constant const_gs : string := genstring;
  signal sig_cs : string(const_cs'range);
  signal sig_gs : string(const_gs'range);

  -- arrays
  subtype cbv is bit_vector(4 downto 0);
  subtype gbv is bit_vector(genint-1 downto 0);

  constant const_cbv : cbv := "01010";
  constant const_gbv : gbv := (others => '0');

  signal sig_cbv : cbv;
  signal sig_gbv : gbv;

  -- arrays of arrays
  type array_of_cbv is array(integer range <>) of cbv;
  type array_of_gbv is array(integer range <>) of gbv;
  subtype ca_cbv is array_of_cbv(3 downto 0);
  subtype ca_gbv is array_of_gbv(3 downto 0);
  subtype ga_cbv is array_of_cbv(genint+1 downto 0);
  subtype ga_gbv is array_of_gbv(genint+1 downto 0);

  constant const_ca_cbv : ca_cbv := (0 => "01010",
                                     1 => "11111",
                                     others => "00000");
  constant const_ca_gbv : ga_gbv := (0 => (others => '0'),
                                     others => (others => '1'));
  constant const_ga_cbv : ga_cbv := (others => "01100");
  constant const_ga_gbv : ga_gbv := (others => (others => '0'));

  signal sig_ca_cbv : ca_cbv;
  signal sig_ca_gbv : ga_gbv;
  signal sig_ga_cbv : ga_cbv;
  signal sig_ga_gbv : ga_gbv;

  -- records of arrays.
  type rcd is record 
    cbv : cbv;
    gbv : gbv;
  end record;

  constant const_rcd : rcd := (cbv => "01110",
                               gbv => (others => '1'));
  signal sig_rcd : rcd;

  -- arrays of records of arrays.
  type array_of_rcd is array(integer range <>) of rcd;
  subtype ca_rcd is array_of_rcd(3 downto 0);
  subtype ga_rcd is array_of_rcd(genint downto 0);

  constant const_ca_rcd : ca_rcd := (0 => (cbv => "00111", gbv => (others => '1')),
                                     1 => (cbv => "11000", gbv => (others => '0')),
                                     others => (others => (others => '0')));
  constant const_ga_rcd : ga_rcd := (others => (cbv => "00111", gbv => (others => '1')));
  signal sig_ca_rcd : ca_rcd;
  signal sig_ga_rcd : ga_rcd;
  
begin

  -- Just here so we get a meaningful dump.
  main_process: process
  begin
    wait for 10 ns;
    wait;
  end process;      

  sig_bit <= const_bit;
  sig_enum <= const_enum;
  sig_bool <= const_bool;
  sig_int <= const_int;

  sig_cs <= const_cs;
  sig_gs <= const_gs;

  sig_cbv <= const_cbv;
  sig_gbv <= const_gbv;
  sig_ca_cbv <= const_ca_cbv;
  sig_ca_gbv <= const_ga_gbv;
  sig_ga_cbv <= const_ga_cbv;
  sig_ga_gbv <= const_ga_gbv;
  sig_rcd <= const_rcd;
  sig_ca_rcd <= const_ca_rcd;
  sig_ga_rcd <= const_ga_rcd;
end arch; 


entity myentity is
  generic (
    genint: integer := 3;
    genstring: string := "fish"
    );
end myentity;

architecture arch of myentity is
begin

  instance_one : entity work.mysubentity 
    generic map(
      genint => genint,
      genstring => genstring
      );

  genloop1: for mygenvar1 in 0 to 4 generate
    instance_two : entity work.mysubentity 
      generic map(
        genint => mygenvar1,
        genstring => genstring
        );
  end generate;

  genloop2: for mygenvar2 in 0 to genint generate
    instance_three : entity work.mysubentity 
      generic map(
        genint => mygenvar2,
        genstring => genstring
        );
    genloop3: for mygenvar3 in 0 to mygenvar2 generate
      instance_three : entity work.mysubentity 
        generic map(
          genint => mygenvar3,
          genstring => genstring
          );
    end generate;
  end generate;

  
end architecture;
