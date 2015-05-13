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
        branch_dest     : in MemWordAddr;

        -- inputs from pipeline hazard logic

        -- if '1', pc won't be updated to next_pc, so instruction in stage 1
        -- will be repeated. (stage 1 should of course also be invalidated using
        -- st1out.invalid_flag.) in case of branch from stage 2, stall of stage
        -- 1 will be ignored; it should have been invalidated anyway.
        st1_stall_flag   : in std_logic);
end pl_stage0;


architecture Behavioral of pl_stage0 is
    -- value of $pc, $pc+2 for IRAM output
    signal pc           : MemWordAddr;
    signal pc_plus_2    : MemWordAddr;
    -- value of $pc for next instruction
    signal next_pc      : MemWordAddr;
    signal cur_opcode   : Opcode_Type;
begin

    sync_proc : process(clk)
    begin
        if rising_edge(clk) then
            pc <= next_pc;
        end if;
    end process;

    -- IRAM addrs omit lowest bit, so +1 is effectively +2
    pc_plus_2 <= std_logic_vector(unsigned(pc) + 1);

    -- IRAM output will be updated on next clock cycle
    iram_addr <= next_pc;

    -- no output flip-flop, so value in st0out matches value of $pc + 2 for
    -- currently output instruction
    st0out.pc_plus_2 <= pc_plus_2;

    next_pc_proc : process(pc, pc_plus_2, st1_stall_flag,
        branch_flag, branch_dest)
    begin
        if branch_flag = '1' then
            next_pc <= branch_dest;

        elsif st1_stall_flag = '1' then
            -- stage 1 is stalling. it'll want the same instruction next cycle
            next_pc <= pc;

        else
            next_pc <= pc_plus_2;
        end if;
    end process;

end Behavioral;
