// Based on http://www.micahcarrick.com/tutorials/avr-microcontroller-tutorial/getting-started.html

#include <avr/io.h>
#include <util/delay.h>

int main ()
{
    DDRB |= _BV(DDB5);

    for (;;)
    {
        PORTB ^= _BV(PB5);
        _delay_ms(500);
    }
}
