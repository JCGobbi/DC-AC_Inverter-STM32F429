with STM32.Device; use STM32.Device;
with STM32.PWM;    use STM32.PWM;

package body Inverter_ADC is

   procedure Initialize_ADC_Timer;
   --  Initialize the timer to start ADCs convertions.

   --------------------
   -- Initialize_ADC --
   --------------------

   procedure Initialize_ADC is

      All_Regular_Conversions : constant Regular_Channel_Conversions :=
        (1 => (Channel     => ADC_Battery_V_Point.Channel,
               Sample_Time => Sample_480_Cycles),
         2 => (Channel     => ADC_Battery_I_Point.Channel,
               Sample_Time => Sample_480_Cycles),
         3 => (Channel     => ADC_Output_V_Point.Channel,
               Sample_Time => Sample_480_Cycles));

   begin

      --  Initialize GPIO for analog input
      for Reading of ADC_Reading_Settings loop
         STM32.Device.Enable_Clock (Reading.GPIO_Entry);
         Reading.GPIO_Entry.Configure_IO
           (Config => GPIO_Port_Configuration'(Mode => Mode_Analog, others => <>));
      end loop;

      --  Initialize ADC mode
      Enable_Clock (Sensor_ADC.all);
      Reset_All_ADC_Units;

      Configure_Unit
        (Sensor_ADC.all,
         Resolution => ADC_Resolution_12_Bits,
         Alignment  => Right_Aligned);

      --  Conversions are triggered by Sensor Timer.
      Configure_Regular_Conversions
        (Sensor_ADC.all,
         Continuous  => False,  --  False is ESSENTIAL when externally triggered!
         Trigger     => (Trigger_Rising_Edge, Event => Sensor_Trigger_Event),
         Enable_EOC  => True,
         Conversions => All_Regular_Conversions);
      --  Either rising or falling edge should work. Note that the Event must
      --  match the timer used!

      Configure_Common_Properties
        (Mode           => Independent,
         Prescalar      => PCLK2_Div_2,
         DMA_Mode       => Disabled,
         Sampling_Delay => Sampling_Delay_5_Cycles);  -- arbitrary

      Enable_Interrupts (Sensor_ADC.all, Regular_Channel_Conversion_Complete);
      --  End of sequence generates an interrupt signalling all conversions
      --  are complete.

      --  Finally, enable the used ADCs
      Enable (Sensor_ADC.all);

      --  Start the timer that trigger ADC conversions
      Initialize_ADC_Timer;

      Initialized := True;
   end Initialize_ADC;

   --------------------------
   -- Initialize_ADC_Timer --
   --------------------------

   procedure Initialize_ADC_Timer is

      ADC_Trigger : PWM_Modulator;

   begin

      Configure_PWM_Timer (Generator => Sensor_Timer'Access,
                           Frequency => UInt32 (Sensor_Frequency_Hz));

      ADC_Trigger.Attach_PWM_Channel (Generator => Sensor_Timer'Access,
                                      Channel   => Sensor_Timer_Channel,
                                      Point     => Sensor_Timer_Point,
                                      PWM_AF    => Sensor_Timer_AF);

      ADC_Trigger.Set_Duty_Cycle (Value => 50);
      ADC_Trigger.Enable_Output;

   end Initialize_ADC_Timer;

   ----------------
   -- Get_Sample --
   ----------------

   function Get_Sample (Reading : in ADC_Reading)
      return Voltage is
   begin
      --  Convert the UInt16 ADC value to Voltage (Float).
      return Voltage (Float (Sensor_Handler.Get_Regular_Samples (Reading)) * ADC_V_Per_Lsb);
   end Get_Sample;

   ------------------
   -- Battery_Gain --
   ------------------

   function Battery_Gain
     (V_Setpoint : Battery_V_Range := Battery_V_Range'First;
      V_Actual   : Voltage := Get_Sample (V_Battery))
   return Gain_Range is

   begin
      if (V_Actual / Battery_Relation < Battery_V_Range'First) or
        (V_Actual / Battery_Relation > Battery_V_Range'Last)
      then
         --  Protect the inverter disabling the power stage.
         Inverter_PWM.Safe_State;
         return 0.0;
         --  Call other routine to visually indicate that the power stage was
         --  disabled.
      else
         return V_Setpoint / V_Actual;
      end if;
   end Battery_Gain;

   --------------------
   -- Test_V_Battery --
   --------------------

   function Test_V_Battery return Boolean is
      V_Actual : constant Voltage := Get_Sample (Reading => V_Battery);
   begin
      if (V_Actual / Battery_Relation < Battery_V_Range'First or
          V_Actual / Battery_Relation > Battery_V_Range'Last)
      then
         return False;
      else
         return True;
      end if;
   end Test_V_Battery;

   --------------------
   -- Test_I_Battery --
   --------------------

   function Test_I_Battery return Boolean is
      V_Actual : constant Voltage := Get_Sample (Reading => I_Battery);
   begin
      if V_Actual > Battery_I_Range'Last then
         return False;
      else
         return True;
      end if;
   end Test_I_Battery;

   -------------------
   -- Test_V_Output --
   -------------------

   function Test_V_Output return Boolean is
      V_Actual : constant Voltage := Get_Sample (Reading => V_Output);
   begin
      if (V_Actual / Output_Relation < Output_V_Range'First or
          V_Actual / Output_Relation > Output_V_Range'Last)
      then
         return False;
      else
         return True;
      end if;
   end Test_V_Output;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized
      return Boolean is (Initialized);

   --------------------
   -- Sensor Handler --
   --------------------

   protected body Sensor_Handler is

      -------------------------
      -- Get_Regular_Samples --
      -------------------------
      function Get_Regular_Samples return Regular_Samples_Array is
      begin
         return Regular_Samples;
      end Get_Regular_Samples;

      ------------------------
      -- Sensor_ISR_Handler --
      ------------------------

      procedure Sensor_ISR_Handler is
      begin
         if Status (Sensor_ADC.all, Regular_Channel_Conversion_Complete) then
            if Interrupt_Enabled (Sensor_ADC.all, Regular_Channel_Conversion_Complete) then
               Clear_Interrupt_Pending (Sensor_ADC.all, Regular_Channel_Conversion_Complete);

               --  Save the ADC values into a buffer
               Regular_Samples (Rank) := Conversion_Value (Sensor_ADC.all);
               if Rank = ADC_Reading'Last then
                  Rank := ADC_Reading'First;

               --  Calculate the new Sine_Gain based on battery voltage
               --  Set_Sine_Gain (Battery_Gain);
               --  Actually is disabled because there is no signal at the ADC.

               else
                  Rank := ADC_Reading'Succ (Rank);
               end if;

               --  Testing the 5 kHz output with 1 Hz LED blinking. Because there
               --  are three regular channel conversions, this frequency will be
               --  three times greater.
               --  if Counter = 2_500 then
               --     Set_Toggle (Blue_LED);
               --     Counter := 0;
               --  end if;
               --  Counter := Counter + 1;

            end if;
         end if;
      end Sensor_ISR_Handler;

   end Sensor_Handler;

end Inverter_ADC;
