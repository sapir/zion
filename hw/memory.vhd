library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity memory is
    generic (width_bits, depth_words, addr_size : integer);
    port (
        clka    : in std_logic;
        ena     : in std_logic;
        wea     : in std_logic_vector(0 downto 0);
        addra   : in std_logic_vector(addr_size - 1 downto 0);
        dina    : in std_logic_vector(width_bits - 1 downto 0);
        douta   : out std_logic_vector(width_bits - 1 downto 0);

        clkb    : in std_logic;
        enb     : in std_logic;
        web     : in std_logic_vector(0 downto 0);
        addrb   : in std_logic_vector(addr_size - 1 downto 0);
        dinb    : in std_logic_vector(width_bits - 1 downto 0);
        doutb   : out std_logic_vector(width_bits - 1 downto 0));
end memory;


architecture Behavioral of memory is
    subtype word_type is std_logic_vector(width_bits - 1 downto 0);
    type mem_type is array(depth_words - 1 downto 0) of word_type;

    shared variable data : mem_type;

    -- tmp addr signals so equiv. reg. removal can be disabled
    subtype addr_type is std_logic_vector(addr_size - 1 downto 0);
    signal tmp_addra, tmp_addrb : addr_type;

    attribute equivalent_register_removal : string;
    attribute equivalent_register_removal of tmp_addra : signal is "no";
    attribute equivalent_register_removal of tmp_addrb : signal is "no";

begin

    tmp_addra <= addra;
    tmp_addrb <= addrb;

    port_a_proc : process (clka)
    begin
        if rising_edge(clka) then
            if ena = '1' then
                if wea = "1" then
                    data(to_integer(unsigned(tmp_addra))) := dina;
                end if;

                douta <= data(to_integer(unsigned(tmp_addra)));
            end if;
        end if;
    end process;

    port_b_proc : process (clkb)
    begin
        if rising_edge(clkb) then
            if enb = '1' then
                if web = "1" then
                    data(to_integer(unsigned(tmp_addrb))) := dinb;
                end if;

                doutb <= data(to_integer(unsigned(tmp_addrb)));
            end if;
        end if;
    end process;

end Behavioral;
