library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_op is
    generic (
        NBITS                       : natural := 1;
        NBR_OF_CHROMA               : natural := 1;
        NBR_OF_ROW                  : natural := 1;
        NBR_OF_COL                  : natural := 1;
        NBR_OF_MATRIX               : natural := 1);
    port (
        signal clock, rst             : in  std_logic;
        signal in_data                : in  std_logic_vector(NBR_OF_MATRIX*NBR_OF_COL*NBR_OF_ROW*NBR_OF_CHROMA*NBITS-1 downto 0));
end entity test_op;

architecture rtl of test_op is

    package local_pixel_pkg is new work.pixel_pkg
      generic map (
            NBITS                       => NBITS,
            NBR_OF_CHROMA               => NBR_OF_CHROMA
      );

    package local_pixel_column_pkg is new work.pixel_column_pkg
      generic map (
            NBITS                       => NBITS,
            NBR_OF_CHROMA               => NBR_OF_CHROMA,
            NBR_OF_ROW                  => NBR_OF_ROW,
            local_pixel_pkg                => local_pixel_pkg
      );

    package local_pixel_matrix_pkg is new work.pixel_matrix_pkg
      generic map (
            NBITS                       => NBITS,
            NBR_OF_CHROMA               => NBR_OF_CHROMA,
            NBR_OF_ROW                  => NBR_OF_ROW,
            NBR_OF_COL                  => NBR_OF_COL,
            local_pixel_column_pkg         => local_pixel_column_pkg
      );

    use local_pixel_matrix_pkg.all;
    signal input_pixel_matrix : TYPE_PIXEL_MATRIX;
begin

    -- As soon as a function from the local_pixel_matrix_pkg is used it breaks
    input_pixel_matrix <= std_logic_vector_to_pixel_matrix(in_data(NBR_OF_COL*NBR_OF_ROW*NBR_OF_CHROMA*NBITS-1 downto 0));
end architecture rtl;
