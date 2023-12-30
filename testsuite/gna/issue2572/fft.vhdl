library ieee;
use work.fft_types.all;

entity fft is
  generic (
    POW : natural  := 4;
    DIM : positive := 2**POW);
  port (
    x, w : in  fft_data_vector (0 to DIM-1);
    y    : out fft_data_vector (0 to DIM-1));
end entity fft;

architecture recursive of fft is
  subtype half_array is fft_data_vector (0 to DIM/2-1);
  signal even, odd, x_e, x_o, w_e : half_array := (others => (0.0, 0.0));
begin
  stage :
  if base_case: POW = 0 generate
    y <= x;
  else general_case: generate
    split_even_odd :
    for i in half_array'range generate
      x_e(i) <= x(2*i);
      x_o(i) <= x(2*i+1);
      w_e(i) <= w(2*i);
    end generate;

    even_fft :
      entity fft
        generic map (
          POW => POW-1)
        port map (
          x => x_e, w => w_e, y => even);

    odd_fft :
      entity fft
        generic map (
          POW => POW-1)
        port map (
          x => x_o, w => w_e, y => odd);

    y(0 to DIM/2-1)   <= even + w(0 to DIM/2-1)*odd;
    y(DIM/2 to DIM-1) <= even + w(DIM/2 to DIM-1)*odd;
  end generate stage;
end architecture recursive;

