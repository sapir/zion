library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


-- Basically, we have to worry about the following data hazards:
-- 1. instr. in stage 1 preparing to use value (in stage 2) that is being
--      modified by instr. currently in stage 2
-- 2. instr. in stage 1 preparing to use value (in stage 2) that is being
--      modified by instr. currently in stage 3
--
-- Case 2 is simply handled by the register file returning the modified value
-- in the same clock cycle.
--
-- Note that any data hazard encountered by stage 2 will already have been
-- handled when instruction was in stage 1.
--
-- In addition, we have a control hazard on the instruction following a taken
-- branch. Those are currently handled by stage 1 itself, by invalidating that
-- instruction while taking the branch.


entity hazardctl is
    Port (
        clk                 : in std_logic;

        -- registers that stage 1 is reading
        st1_reg1_idx        : in Reg_Index;
        st1_reg2_idx        : in Reg_Index;

        -- register that stage 2 is writing (and how)
        st2_wr_reg_en       : in std_logic;
        st2_wr_reg_src      : in RegWriteSrc_Type;
        st2_wr_reg_idx      : in Reg_Index;
        -- whether stage 2 was invalided (making writes irrelevant)
        st2_invalid_flag    : in std_logic;

        -- value that just arrived at stage 3
        st3_alu_res         : in Logic_Word;

        -- outputs:
        -- tell stage 2 whether to override its inputs (due to data
        -- hazard detected in previous instruction's stage 1)
        st2_reg1_fwd        : out FwdValue;
        st2_reg2_fwd        : out FwdValue;
        -- tell stage 1 (and 0) whether to stall
        st1_stall_flag      : out std_logic);
end hazardctl;


architecture Behavioral of hazardctl is
    -- whether forwarding is planned for reg 1 / 2 in stage 1
    signal st1_plan_fwd : std_logic_vector(1 to 2);
    -- same for stage 2
    signal st2_plan_fwd : std_logic_vector(1 to 2);
begin

    sync_proc : process(clk)
    begin
        if rising_edge(clk) then
            -- set values for stage 2 from values decided for previous cycle's
            -- stage 1
            st2_plan_fwd <= st1_plan_fwd;
        end if;
    end process;

    st2_reg1_fwd.use_fwd <= st2_plan_fwd(1);
    st2_reg2_fwd.use_fwd <= st2_plan_fwd(2);

    st2_reg1_fwd.fwd_val <= st3_alu_res;
    st2_reg2_fwd.fwd_val <= st3_alu_res;


    st1_data_hazard_proc : process(st1_reg1_idx, st1_reg2_idx,
        st2_wr_reg_en, st2_wr_reg_src, st2_wr_reg_idx, st2_invalid_flag)
    begin
        -- default: use values from stage 1 unless there's a data hazard,
        -- and don't stall.
        st1_plan_fwd <= (others => '0');
        st1_stall_flag <= '0';

        -- forward values from stage 2.
        -- ignore writes for invalidated instructions
        if st2_wr_reg_en = '1' and st2_invalid_flag = '0' then
            case st2_wr_reg_src is
                when rws_mem =>
                    -- if stage 2 wants to read value from memory, no way we can
                    -- handle it in time. stage 1 will have to wait one cycle.
                    if st2_wr_reg_idx = st1_reg1_idx
                        or st2_wr_reg_idx = st1_reg2_idx then

                        st1_stall_flag <= '1';

                    end if;

                when rws_alu =>
                    -- if stage 2 is calculating value to be written with ALU,
                    -- then instruction in stage 1 should use that value once it
                    -- (the instr.) reaches stage 2 and the value reaches
                    -- stage 3.

                    if st1_reg1_idx = st2_wr_reg_idx then
                        st1_plan_fwd(1) <= '1';
                    end if;

                    if st1_reg2_idx = st2_wr_reg_idx then
                        st1_plan_fwd(2) <= '1';
                    end if;

                when others => null; -- impossible
            end case;
        end if; -- if st2_wr_reg_en, st2_invalid_flag
    end process;

end Behavioral;
