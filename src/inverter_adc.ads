with HAL;          use HAL;
with STM32.GPIO;   use STM32.GPIO;
with STM32.ADC;    use STM32.ADC;

with STM_Board;    use STM_Board;

package Inverter_ADC is
   --  Performs analog to digital conversions in a timed manner.
   --  The timer starts ADC conversions and, at the end of conversion, it
   --  produces an interrupt that actualizes the buffer of ADC values and
   --  corrects the duty cycle for variations in battery voltage.

   Sensor_Frequency_Hz : constant Frequency_Hz := 5_000.0;
   --  Timer PWM frequency that controls start of ADC convertion.

   subtype Voltage is Float;
   --  Represents an electric measure.

   ADC_Vref : constant Voltage := 3.3;
   --  ADC full scale voltage.

   Battery_V : constant Voltage := 12.0;
   --  Battery nominal voltage.
   subtype Battery_V_Range is Voltage range (Battery_V * 0.8) .. (Battery_V * 1.2);
   --  Battery voltage tolerance is Battery_V ± 20%.

   Battery_Relation : Float := 10_000.0 / 90_900.0; -- 10 kΩ / 90.9 kΩ
   --  Resistive relation between the measured ADC input and the battery
   --  voltage. This depends on the electronic circuitry.

   Inverter_Power : constant Voltage := 300.0;
   --  Inverter nominal electric power.

   Battery_I : constant Voltage := Inverter_Power / Battery_V_Range'First;
   --  Battery nominal current with maximum inverter power and
   --  minimum battery voltage.
   subtype Battery_I_Range is Voltage range 0.0 .. (Battery_I * 1.1);
   --  Battery current tolerance is Battery_I + 10%.

   Output_V : constant Voltage := 220.0;
   --  AC output RMS voltage.
   subtype Output_V_Range is Voltage range (Output_V * 0.9) .. (Output_V * 1.1);
   --  AC ouput voltage tolerance is Output_V ± 10%.

   Output_Relation : Float :=  10_000.0 / 90_900.0; -- 10 kΩ / 90.9 kΩ
   --  Resistive relation between the measured ADC input and the AC output
   --  voltage. This depends on the electronic circuitry.

   type ADC_Reading is
      (V_Battery, I_Battery, V_Output);
   --  Specifies the available readings.

   procedure Initialize_ADC;
   --  Initialize the ADCs.

   function Get_Sample (Reading : in ADC_Reading) return Voltage
   with
      Pre => Is_Initialized;
   --  Get the specified ADC reading.

   subtype Gain_Range is Float range 0.0 .. 1.0;
   --  For correcting battery voltage and AC output variation.
   Sine_Gain : Gain_Range := 0.0;

   function Battery_Gain
     (V_Setpoint : Battery_V_Range := Battery_V_Range'First;
      V_Actual   : Voltage := Get_Sample (V_Battery)) return Gain_Range;
   --  Calculate the gain of the sinusoid as a function of the
   --  battery voltage.

   function Test_V_Battery return Boolean
   with
      Pre => Is_Initialized;
   --  Test if battery voltage is between maximum and minimum.

   function Test_I_Battery return Boolean
   with
      Pre => Is_Initialized;
   --  Test if battery current is below maximum.

   function Test_V_Output return Boolean
   with
      Pre => Is_Initialized;
   --  Test if output voltage is between maximum and minimum.

   function Is_Initialized return Boolean;

private

   Initialized : Boolean := False;

   ADC_V_Per_Lsb : constant Float := ADC_Vref / 4_095.0; --  12 bit

   type Regular_Samples_Array is array (ADC_Reading'Range) of UInt16;
   for Regular_Samples_Array'Component_Size use 16;

   Regular_Samples : Regular_Samples_Array := (others => 0) with Volatile;

   type ADC_Settings is record
      GPIO_Entry   : GPIO_Point;
      ADC_Entry    : ADC_Point;
      Channel_Rank : Regular_Channel_Rank;
   end record;

   type ADC_Readings is array (ADC_Reading'Range) of ADC_Settings;

   ADC_Reading_Settings : constant ADC_Readings :=
      ((V_Battery) => (GPIO_Entry   => ADC_Battery_V_Pin,
                       ADC_Entry    => ADC_Battery_V_Point,
                       Channel_Rank => 1),
       (I_Battery) => (GPIO_Entry   => ADC_Battery_I_Pin,
                       ADC_Entry    => ADC_Battery_I_Point,
                       Channel_Rank => 2),
       (V_Output)  => (GPIO_Entry   => ADC_Output_V_Pin,
                       ADC_Entry    => ADC_Output_V_Point,
                       Channel_Rank => 3));

   protected Sensor_Handler is
      pragma Interrupt_Priority (Sensor_ISR_Priority);

   private

      Rank : ADC_Reading := ADC_Reading'First;

      Counter : Integer := 0;
      --  For testing the output.

      procedure Sensor_ISR_Handler with
        Attach_Handler => Sensor_Interrupt;

   end Sensor_Handler;

end Inverter_ADC;
