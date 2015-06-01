library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity pl_stage0 is
    Port (
        clk             : in std_logic;

        -- outputs
        iram_addr       : out MemWordAddr;
        st0out          : out Stage_0_1_Interface;

        -- inputs back from stage 1
        branch_flag     : in std_logic;
        branch_dest     : in MemWordAddr);
end pl_stage0;


architecture Behavioral of pl_stage0 is
    -- value of $pc, $pc+2 for IRAM output
    signal pc           : MemWordAddr
        -- will be incremented to 0 on first cycle
        := (others => '1');
    signal pc_plus_2    : MemWordAddr;
    -- value of $pc for next instruction
    signal next_pc      : MemWordAddr;
begin

    sync_proc : process(clk)
    begin
        if rising_edge(clk) then
            pc <= next_pc;
        end if;
    end process;

    -- IRAM addrs omit lowest bit, so +1 is effectively +2
    pc_plus_2 <= std_logic_vector(unsigned(pc) + 1);

    -- no output flip-flop, so value in st0out matches value of $pc + 2 for
    -- currently output instruction
    st0out.pc_plus_2 <= pc_plus_2;

    next_pc_proc : process(pc, pc_plus_2, branch_flag, branch_dest)
    begin
        if branch_flag = '1' then
            next_pc <= branch_dest;
        else
            next_pc <= pc_plus_2;
        end if;
    end process;

    -- IRAM output will be updated on next clock cycle
    iram_addr <= next_pc;

end Behavioral;
