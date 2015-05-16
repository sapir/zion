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

        -- values that just arrived at stage 3
        st3_alu_res         : in Logic_Word;
        st3_pc_plus_2       : in MemWordAddr;

        -- outputs:
        -- tell stage 2 whether to override its inputs (due to data
        -- hazard detected in previous instruction's stage 1)
        st2_reg1_fwd        : out FwdValue;
        st2_reg2_fwd        : out FwdValue;
        -- tell stage 1 (and 0) whether to stall
        st1_stall_flag      : out std_logic);
end hazardctl;


architecture Behavioral of hazardctl is

    -- source of forwarded values to use in stage 2
    -- (from perspective of instruction running in stage 2, though this value
    -- is set in stage 1)
    type FwdSrc is (
        -- use ALU res passed to stage 3 from previous instruction's stage 2
        fs_st3_alu,
        -- use $pc + 2 value passed to stage 3
        fs_st3_pc_plus_2);

    -- how to perform forwarding
    type FwdPlan is
        record
            use_fwd : std_logic;
            fwd_src : FwdSrc;
        end record;

    signal st1_reg1_plan, st1_reg2_plan : FwdPlan;
    signal st2_reg1_plan, st2_reg2_plan : FwdPlan;

    signal st3_pc_plus_2_as_word : Logic_Word;
begin

    sync_proc : process(clk)
    begin
        if rising_edge(clk) then
            -- set values for stage 2 from values decided for previous cycle's
            -- stage 1
            st2_reg1_plan <= st1_reg1_plan;
            st2_reg2_plan <= st1_reg2_plan;
        end if;
    end process;

    st3_pc_plus_2_as_word <= mem_addr_to_word(st3_pc_plus_2);

    st2_reg1_fwd.use_fwd <= st2_reg1_plan.use_fwd;
    st2_reg2_fwd.use_fwd <= st2_reg2_plan.use_fwd;

    with st2_reg1_plan.fwd_src select st2_reg1_fwd.fwd_val <=
        st3_alu_res             when fs_st3_alu,
        st3_pc_plus_2_as_word   when fs_st3_pc_plus_2,
        (others => '0')         when others;

    with st2_reg2_plan.fwd_src select st2_reg2_fwd.fwd_val <=
        st3_alu_res             when fs_st3_alu,
        st3_pc_plus_2_as_word   when fs_st3_pc_plus_2,
        (others => '0')         when others;


    st1_data_hazard_proc : process(st1_reg1_idx, st1_reg2_idx,
        st2_wr_reg_en, st2_wr_reg_src, st2_wr_reg_idx, st2_invalid_flag)

        variable fwd_src : FwdSrc;
    begin
        -- default: use values from stage 1 unless there's a data hazard,
        -- and don't stall.
        st1_reg1_plan.use_fwd <= '0';
        st1_reg1_plan.fwd_src <= fs_st3_alu; -- doesn't matter
        st1_reg2_plan.use_fwd <= '0';
        st1_reg2_plan.fwd_src <= fs_st3_alu; -- doesn't matter
        st1_stall_flag <= '0';

        -- forward values from stage 2.
        -- ignore writes for invalidated instructions
        if st2_wr_reg_en = '1' and st2_invalid_flag = '0' then
            if st2_wr_reg_src = rws_mem then
                -- if stage 2 wants to read value from memory, no way we can
                -- handle it in time. stage 1 will have to wait one cycle.
                if st2_wr_reg_idx = st1_reg1_idx
                    or st2_wr_reg_idx = st1_reg2_idx then

                    st1_stall_flag <= '1';

                end if;

            else -- not trying to read from memory

                case st2_wr_reg_src is
                    -- if stage 2 is calculating value to be written with ALU,
                    -- then instruction in stage 1 should use that value once it
                    -- (the instr.) reaches stage 2 and the value reaches
                    -- stage 3.
                    when rws_alu =>
                        fwd_src := fs_st3_alu;

                    -- like case of writing ALU result, but using $pc+2 instead
                    when rws_pc_plus_2 =>
                        fwd_src := fs_st3_pc_plus_2;

                    when others =>
                        -- impossible
                        fwd_src := fs_st3_alu;
                end case;

                if st2_wr_reg_idx = st1_reg1_idx then
                    st1_reg1_plan.use_fwd <= '1';
                    st1_reg1_plan.fwd_src <= fwd_src;
                end if;

                if st2_wr_reg_idx = st1_reg2_idx then
                    st1_reg2_plan.use_fwd <= '1';
                    st1_reg2_plan.fwd_src <= fwd_src;
                end if;
            end if; -- if rws_mem
        end if; -- if st2_wr_reg_en, st2_invalid_flag
    end process;

end Behavioral;
