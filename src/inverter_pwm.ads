with STM32.GPIO;   use STM32.GPIO;
with STM32.Timers; use STM32.Timers;
with STM32.PWM;    use STM32.PWM;

with STM_Board;    use STM_Board;
with Inverter_ADC;

package Inverter_PWM is

   -----------------
   -- Definitions --
   -----------------

   type PWM_Phase is (A, B);
   --  Each phase of a full bridge circuit.

   type PWM_Alignment is
      (Edge, --  Positive edge
       Center --  Center of positive part
      );
   --  Describes where on the PWM waveform the signals shall be aligned.

   --  The final maximum amplitude for the sine voltage is defined by the
   --  maximum sine table value, that is 10_000.
   --  Considering that the battery nominal voltage is 12 Volts, this will
   --  be the peak AC value, which corresponds to a primary AC RMS voltage
   --  of 12 V / sqrt(2) = 8.485 V.
   --  With a minimum battery voltage of 10 V, the minimum AC RMS voltage
   --  will be 10 V / sqrt(2) = 7.07 V.
   --  The transformer voltage ratio between the primary and secondary, for
   --  a maximum output voltage of 230 V RMS, will be 230 V / 7.07 V = 32.5,
   --  so the turns ratio of the transformer will be (Ns / Np) = 33.

   subtype Table_Amplitude is Integer range 0 .. 10_000;
   --  Values inside 14 bits (0 - 16_384).

   subtype Duty_Cycle is Float range 0.0 .. 100.0;

   --  The upload frequency of the duty cycle is defined by the number of points
   --  for each semi-sinusoid.
   --  For 50 Hz we have 2 half senoids * 50 Hz * 250 points = 25000 Hz.
   --  For 60 Hz we have 2 half senoids * 60 Hz * 250 points = 30000 Hz.

   PWM_Frequency_Hz : Frequency_Hz := 30_000.0; -- for 60 Hz

   --  STM32F429 operates at 168 MHz with 84 MHz into Prescaler.
   --  For 50 Hz we have 84 MHz / 25 kHz = 3360 ticks by each 25 kHz period,
   --  so the minimum duty cycle is 100 / 3360 = 0.0298 %.
   --  For 60 Hz we have 84 MHz / 30 kHz = 2800 ticks by each 30 kHz period,
   --  so the minimum duty cycle is 100 / 2800 = 0.0357 %.

   subtype Deadtime_Range is Float range 0.0 .. 400.0e-9;
   --  Maximum deadtime permissible is 126 us.
   --  Maximum deadtime chosen is 1% of the PWM_Frequency_Hz = 0.01/25_000.
   PWM_Deadtime : constant Deadtime_Range := 166.7e-9;
   --  The delay exists in the rising edges.
   --  It depends on the electronic circuit rise and fall times.
   --  166.7e-9 * 30 kHz * 100 = 0.5% of the total period,
   --  0.5% of 2800 ticks = 14 ticks.

   -----------------------------
   -- Procedures and function --
   -----------------------------

   procedure Initialize_PWM
      (Frequency : Frequency_Hz;
       Deadtime  : Deadtime_Range;
       Alignment : PWM_Alignment);
   --  Initialize the timer peripheral for PWM.
   --  Each phase needs to be enabled manually after this.

   procedure Enable_Phase (This : PWM_Phase)
   with inline;
   --  Enable PWM generation for the specified phase.

   procedure Disable_Phase (This : PWM_Phase)
   with inline;
   --  Disable PWM generation for the specified phase.

   procedure Start_PWM
   with
      Pre => Is_Initialized;
   --  Start the generation of sinusoid wave by enabling interrupt.

   procedure Stop_PWM
   with
      Pre => Is_Initialized;
   --  Stop the generation of sinusoid wave by disabling interrupt.

   function Get_Duty_Resolution return Duty_Cycle;
   --  Return the minimum step that the duty can be changed, in percent.

   procedure Set_Duty_Cycle
      (This  : PWM_Phase;
       Value : Duty_Cycle);
   --  Sets the duty cycle in percent for the specified phase.

   procedure Set_Duty_Cycle
      (This      : PWM_Phase;
       Amplitude : Table_Amplitude;
       Gain      : Inverter_ADC.Gain_Range);
   --  Sets the duty cycle for the specified phase.

   procedure Set_PWM_Gate_Power (Enabled : in Boolean)
   with
      Pre => (if Enabled = False then STM_Board.Is_Initialized
              else Is_Initialized and STM_Board.Is_Initialized);
   --  Enable or disable the output of the gate drivers.

   procedure Reset_Sine_Step;
   --  Set the sine table step counter to the last position of the
   --  table, or 250, whose amplitude value is 0.

   procedure Safe_State;
   --  Forces the inverter into a state that is considered safe.
   --  Typically this disables the PWM generation (all switches off), and
   --  turns off the power to the gate drivers.

   function Is_Initialized return Boolean;
   --  Returns True if the board specifics are initialized.

   function Has_APB2_Frequency (This : Timer) return Boolean;
   --  Timers 1, 8, 9, 10, 11

   function Has_APB1_Frequency (This : Timer) return Boolean;
   --  Timers 3, 4, 6, 7, 12, 13, 14

private

   Initialized : Boolean := False;

   subtype Sine_Step_Range is Natural range 1 .. 250;
   --  Number of steps for the half sine table.

   Sine_Step : Sine_Step_Range := Sine_Step_Range'Last;
   --  The table last step value is 0.

   --  The table for sine generation is produced knowing the number of points
   --  to complete 1/4 sine period. The semi-sinusoid, or 1/2 sine period is
   --  completed with these same points in reverse order.
   --  The equation which defines each point is:
   --
   --  D = A * sin(pi/2 * x/N)
   --  D = Duty cycle at a given discrete point;
   --  A = Signal amplitude of the maximum duty cycle. We adopt 10_000.
   --  pi/2 = 1/4 of the sine period
   --  x = Step number
   --  N = Number of points = 125

   --  Sine table with 125 points from 0 to 10000, in direct and reverse order
   --  to create a semi-sinusoid with 250 points.
   Sine_Table : constant array (Sine_Step_Range) of Table_Amplitude :=
                (126, 251, 377, 502, 628, 753, 879, 1004, 1129, 1253,
                 1378, 1502, 1626, 1750, 1874, 1997, 2120, 2243, 2365, 2487,
                 2608, 2730, 2850, 2970, 3090, 3209, 3328, 3446, 3564, 3681,
                 3798, 3914, 4029, 4144, 4258, 4371, 4484, 4596, 4707, 4818,
                 4927, 5036, 5144, 5252, 5358, 5464, 5569, 5673, 5776, 5878,
                 5979, 6079, 6179, 6277, 6374, 6471, 6566, 6660, 6753, 6845,
                 6937, 7026, 7115, 7203, 7290, 7375, 7459, 7543, 7624, 7705,
                 7785, 7863, 7940, 8016, 8090, 8163, 8235, 8306, 8375, 8443,
                 8510, 8575, 8639, 8702, 8763, 8823, 8881, 8938, 8994, 9048,
                 9101, 9152, 9202, 9251, 9298, 9343, 9387, 9430, 9471, 9511,
                 9549, 9585, 9620, 9654, 9686, 9716, 9745, 9773, 9799, 9823,
                 9846, 9867, 9887, 9905, 9921, 9936, 9950, 9961, 9972, 9980,
                 9987, 9993, 9997, 9999, 10000, 9999, 9997, 9993, 9987, 9980,
                 9972, 9961, 9950, 9936, 9921, 9905, 9887, 9867, 9846, 9823,
                 9799, 9773, 9745, 9716, 9686, 9654, 9620, 9585, 9549, 9511,
                 9471, 9430, 9387, 9343, 9298, 9251, 9202, 9152, 9101, 9048,
                 8994, 8938, 8881, 8823, 8763, 8702, 8639, 8575, 8510, 8443,
                 8375, 8306, 8235, 8163, 8090, 8016, 7940, 7863, 7785, 7705,
                 7624, 7543, 7459, 7375, 7290, 7203, 7115, 7026, 6937, 6845,
                 6753, 6660, 6566, 6471, 6374, 6277, 6179, 6079, 5979, 5878,
                 5776, 5673, 5569, 5464, 5358, 5252, 5144, 5036, 4927, 4818,
                 4707, 4596, 4484, 4371, 4258, 4144, 4029, 3914, 3798, 3681,
                 3564, 3446, 3328, 3209, 3090, 2970, 2850, 2730, 2608, 2487,
                 2365, 2243, 2120, 1997, 1874, 1750, 1626, 1502, 1378, 1253,
                 1129, 1004, 879, 753, 628, 502, 377, 251, 126, 0);

   PWM_Timer_Ref : access Timer := PWM_Timer'Access;

   Modulators : array (PWM_Phase'Range) of PWM_Modulator;

   type Gate_Setting is record
      Channel   : Timer_Channel;
      Pin_H     : GPIO_Point;
      Pin_L     : GPIO_Point;
      Pin_AF    : STM32.GPIO_Alternate_Function;
   end record;

   type Gate_Settings is array (PWM_Phase'Range) of Gate_Setting;

   Gate_Phase_Settings : constant Gate_Settings :=
      ((A) => Gate_Setting'(Channel => PWM_A_Channel,
                            Pin_H   => PWM_A_H_Pin,
                            Pin_L   => PWM_A_L_Pin,
                            Pin_AF  => PWM_A_GPIO_AF),
       (B) => Gate_Setting'(Channel => PWM_B_Channel,
                            Pin_H   => PWM_B_H_Pin,
                            Pin_L   => PWM_B_L_Pin,
                            Pin_AF  => PWM_B_GPIO_AF));

   protected PWM_Handler is
      pragma Interrupt_Priority (PWM_ISR_Priority);

   private

      Counter : Integer := 0;
      --  For testing the output.

      Semi_Senoid : Boolean := False;
      --  Defines False = 1'st half sinusoid, True = 2'nd half sinusoid.

      procedure PWM_ISR_Handler with
        Attach_Handler => PWM_Interrupt;

   end PWM_Handler;

end Inverter_PWM;
