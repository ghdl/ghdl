entity repro is
end repro;

architecture behav of repro is
  subtype data_word_t is bit_vector (31 downto 0);

  function f1 (data : data_word_t) return bit;

  function f1 (data : bit_vector(31 downto 0)) return bit is
  begin
    return '0';
  end f1;

begin
end  behav;
