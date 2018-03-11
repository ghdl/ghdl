library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_op is
    generic (
        NBITS_IN                       : natural := 1;
        NBR_OF_CHROMA_IN               : natural := 1;
        NBR_OF_ROW_IN                  : natural := 1;
        NBR_OF_COL_IN                  : natural := 1;
        NBITS_OUT                      : natural := 2;
        NBR_OF_CHROMA_OUT              : natural := 1;
        NBR_OF_ROW_OUT                 : natural := 1;
        NBR_OF_COL_OUT                 : natural := 1;
        NBR_OF_MATRIX_IN               : natural := 1;
        NBR_OF_MATRIX_OUT              : natural := 1);
    port (
        signal clock, rst             : in  std_logic;
        signal in_data                : in  std_logic_vector(NBR_OF_MATRIX_IN*NBR_OF_COL_IN*NBR_OF_ROW_IN*NBR_OF_CHROMA_IN*NBITS_IN-1 downto 0);
        signal out_data               : out std_logic_vector(NBR_OF_MATRIX_OUT*NBR_OF_COL_OUT*NBR_OF_ROW_OUT*NBR_OF_CHROMA_OUT*NBITS_OUT-1 downto 0));
end entity test_op;

architecture rtl of test_op is

    package local_pixel_pkg is new work.pixel_pkg
      generic map (
            NBITS_IN                       => NBITS_IN,
            NBR_OF_CHROMA_IN               => NBR_OF_CHROMA_IN,
            NBITS_OUT                      => NBITS_OUT,
            NBR_OF_CHROMA_OUT              => NBR_OF_CHROMA_OUT
      );

    package local_pixel_column_pkg is new work.pixel_column_pkg
      generic map (
            NBITS_IN                       => NBITS_IN,
            NBR_OF_CHROMA_IN               => NBR_OF_CHROMA_IN,
            NBR_OF_ROW_IN                  => NBR_OF_ROW_IN,
            NBITS_OUT                      => NBITS_OUT,
            NBR_OF_CHROMA_OUT              => NBR_OF_CHROMA_OUT,
            NBR_OF_ROW_OUT                 => NBR_OF_ROW_OUT,
            local_pixel_pkg                => local_pixel_pkg
      );

    package local_pixel_matrix_pkg is new work.pixel_matrix_pkg
      generic map (
            NBITS_IN                       => NBITS_IN,
            NBR_OF_CHROMA_IN               => NBR_OF_CHROMA_IN,
            NBR_OF_ROW_IN                  => NBR_OF_ROW_IN,
            NBR_OF_COL_IN                  => NBR_OF_COL_IN,
            NBITS_OUT                      => NBITS_OUT,
            NBR_OF_CHROMA_OUT              => NBR_OF_CHROMA_OUT,
            NBR_OF_ROW_OUT                 => NBR_OF_ROW_OUT,
            NBR_OF_COL_OUT                 => NBR_OF_COL_OUT,
            local_pixel_column_pkg         => local_pixel_column_pkg
      );

    use local_pixel_matrix_pkg.all;
    signal input_pixel_matrix : TYPE_PIXEL_MATRIX_IN;
    signal output_pixel_matrix : TYPE_PIXEL_MATRIX_OUT;
begin

    -- As soon as a function from the local_pixel_matrix_pkg is used it breaks
    input_pixel_matrix <= std_logic_vector_to_pixel_matrix_in(in_data(NBR_OF_COL_IN*NBR_OF_ROW_IN*NBR_OF_CHROMA_IN*NBITS_IN-1 downto 0));

    -- Note: Commented out more complex operation to show that the error is generated regardless
    --       Uncomment to have more "complete" code
    --output_pixel_matrix <= not input_pixel_matrix;
    out_data <= (others => '0'); --pixel_matrix_out_to_std_logic_vector(output_pixel_matrix);
    --out_data <= in_data(NBR_OF_MATRIX_OUT*NBR_OF_COL_OUT*NBR_OF_ROW_OUT*NBR_OF_CHROMA_OUT*NBITS_OUT-1 downto 0);

end architecture rtl;
