library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity zion is
    Port (
        leds : out STD_LOGIC_VECTOR(7 downto 0);
        clk  : in  STD_LOGIC);
end zion;

architecture Behavioral of zion is
    -- Digital clock manager
    COMPONENT dcm1
    PORT(
        CLKIN_IN : IN std_logic;
        CLKFX_OUT : OUT std_logic;
        CLKIN_IBUFG_OUT : OUT std_logic;
        CLK0_OUT : OUT std_logic
        );
    END COMPONENT;

    -- BRAM component
    COMPONENT memory
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
    END COMPONENT;
    -- IRAM input/output signals
    signal iram_addra   : MemWordAddr;
    signal iram_douta   : Instr_Type;
    signal iram_enb     : std_logic;
    signal iram_web     : lvbit;
    signal iram_addrb   : MemWordAddr;
    signal iram_dinb    : Instr_Type;
    signal iram_doutb   : Instr_Type;
    -- DRAM input/output signals
    signal dram_ena     : std_logic;
    signal dram_addra   : DataByteAddr;
    signal dram_douta   : Logic_Byte;
    signal dram_wea     : lvbit;
    signal dram_dina    : Logic_Byte;
    signal dram_enb     : std_logic;
    signal dram_addrb   : DataByteAddr;
    signal dram_dinb    : Logic_Byte;
    signal dram_web     : lvbit;
    signal dram_doutb   : Logic_Byte;

    COMPONENT cpu_core
    Port (
        clk         : in std_logic;
        iram_addra  : out MemWordAddr;
        iram_douta  : in  Instr_Type;
        iram_enb    : out std_logic;
        iram_web    : out lvbit;
        iram_addrb  : out MemWordAddr;
        iram_dinb   : out Instr_Type;
        iram_doutb  : in  Instr_Type;
        dram_ena    : out std_logic;
        dram_addra  : out DataByteAddr;
        dram_douta  : in  Logic_Byte;
        dram_wea    : out lvbit;
        dram_dina   : out Logic_Byte;
        dram_enb    : out std_logic;
        dram_addrb  : out DataByteAddr;
        dram_dinb   : out Logic_Byte;
        dram_web    : out lvbit;
        dram_doutb  : in  Logic_Byte;
        io_leds_we  : out lvbit;
        io_leds_inp : out Logic_Byte);
    END COMPONENT;


    -- dcm output
    signal dcm_clk              : std_logic;


    -- I/O memory

    -- byte-sized I/O register. cur gets set to inp if we is "1".
    type IO_Register_Byte is
        record
            cur : Logic_Byte;
            inp : Logic_Byte;
            -- using a vector to make initialization easier
            we  : lvbit;
        end record;

    signal io_leds_reg : IO_Register_Byte
        := (others => (others => '0'));

begin

    inst_dcm1: dcm1 PORT MAP(
        CLKIN_IN        => clk,
        CLKFX_OUT       => dcm_clk,
        CLKIN_IBUFG_OUT => open,
        CLK0_OUT        => open
    );

    iram : memory
    GENERIC MAP( width_bits=>18, addr_size=>13, depth_words=>8192 )
    PORT MAP (
        clka    => dcm_clk,
        ena     => '1',
        addra   => iram_addra,
        douta   => iram_douta,
        wea     => "0",
        dina    => "000000000000000000",

        clkb    => dcm_clk,
        enb     => iram_enb,
        addrb   => iram_addrb,
        doutb   => iram_doutb,
        web     => iram_web,
        dinb    => iram_dinb);

    dram : memory
    GENERIC MAP( width_bits=>8, addr_size=>11, depth_words=>2048 )
    PORT MAP (
        clka    => dcm_clk,
        ena     => dram_ena,
        addra   => dram_addra,
        douta   => dram_douta,
        wea     => dram_wea,
        dina    => dram_dina,

        clkb    => dcm_clk,
        enb     => dram_enb,
        addrb   => dram_addrb,
        doutb   => dram_doutb,
        web     => dram_web,
        dinb    => dram_dinb);

    inst_cpu_core : cpu_core
    PORT MAP (
        clk         => dcm_clk,
        iram_addra  => iram_addra,
        iram_douta  => iram_douta,
        iram_enb    => iram_enb,
        iram_web    => iram_web,
        iram_addrb  => iram_addrb,
        iram_dinb   => iram_dinb,
        iram_doutb  => iram_doutb,
        dram_ena    => dram_ena,
        dram_addra  => dram_addra,
        dram_douta  => dram_douta,
        dram_wea    => dram_wea,
        dram_dina   => dram_dina,
        dram_enb    => dram_enb,
        dram_addrb  => dram_addrb,
        dram_dinb   => dram_dinb,
        dram_web    => dram_web,
        dram_doutb  => dram_doutb,
        io_leds_we  => io_leds_reg.we,
        io_leds_inp => io_leds_reg.inp);


    -------------------------------------------------
    -- I/O
    -------------------------------------------------

    io_leds_proc : process(dcm_clk, io_leds_reg.inp, io_leds_reg.we)
    begin
        if rising_edge(dcm_clk) then
            if io_leds_reg.we = "1" then
                io_leds_reg.cur <= io_leds_reg.inp;
            end if;
        end if;
    end process;

    leds <= io_leds_reg.cur;

end Behavioral;
