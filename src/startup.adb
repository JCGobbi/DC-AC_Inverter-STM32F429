with Ada.Real_Time; use Ada.Real_Time;

with STM_Board;     use STM_Board;
with Inverter_ADC;  use Inverter_ADC;
with Inverter_PWM;  use Inverter_PWM;

package body StartUp is

   procedure Wait_Until_V_Battery;
   --  Wait until battery voltage is between minimum and maximum.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Initialize GPIO ports
      Initialize_GPIO;

      --  Select the AC frequency of the inverter
      if (Read_AC_Frequency) then -- 50 Hz
         PWM_Frequency_Hz := 25_000.0;
      else -- 60 Hz
         PWM_Frequency_Hz := 30_000.0;
      end if;

      --  Initialize sensors ADC
      Initialize_ADC;

      --  Do not start while the battery voltage is outside maximum and minimum
--      Wait_Until_V_Battery;

      --  Disable PWM gate drivers because some gate drivers enable with
      --  low level.
      Set_PWM_Gate_Power (False);

      --  Initialize PWM generator
      Initialize_PWM (Frequency => PWM_Frequency_Hz,
                      Deadtime  => PWM_Deadtime,
                      Alignment => Center);

      for P in PWM_Phase'Range loop
         Set_Duty_Cycle (This => P,
                         Value => 0.0);
         Enable_Phase (P);
      end loop;

      Initialized :=
         STM_Board.Is_Initialized and
         Inverter_ADC.Is_Initialized and
         Inverter_PWM.Is_Initialized;

   End Initialize;

   --------------------------
   -- Wait_Until_V_Battery --
   --------------------------

   procedure Wait_Until_V_Battery is
      Period : constant Time_Span := Milliseconds (1);
      Next_Release : Time := Clock;
      Counter : Integer := 0;
   begin
      loop
         exit when Test_V_Battery;
         Next_Release := Next_Release + Period;
         delay until Next_Release;
         Counter := Counter + 1;
         if (Counter > 1_000) then
            Set_Toggle (Red_LED);
            Counter := 0;
         end if;
      end loop;
      Turn_Off (Red_LED);
   end Wait_Until_V_Battery;

   ----------------------------
   -- Wait_Until_Initialized --
   ----------------------------

   procedure Wait_Until_Initialized is
      Period : constant Time_Span := Milliseconds (1);
      Next_Release : Time := Clock;
   begin
      loop
         exit when Is_Initialized;
         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end Wait_Until_Initialized;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized
      return Boolean is (Initialized);

end StartUp;
