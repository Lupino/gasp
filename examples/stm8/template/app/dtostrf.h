char *dtostrf_nan(char *result) {
    result[0] = 'n';
    result[1] = 'a';
    result[2] = 'n';
    return result;
}

char *dtostrf_inf(char *result) {
    result[0] = 'i';
    result[1] = 'n';
    result[2] = 'f';
    return result;
}

char *dtostrf_ovf(char *result) {
    result[0] = 'o';
    result[1] = 'v';
    result[2] = 'f';
    return result;
}

char *dtostrf(double number, int width, unsigned int digits, char *result) {
  size_t n = 0;
  uint8_t i;
  unsigned long int_part;
  double remainder, rounding;
  unsigned int toPrint;

  if (isnan(number)) return dtostrf_nan(result);
  if (isinf(number)) return dtostrf_inf(result);
  if (number > 4294967040.0) return dtostrf_ovf(result);  // constant determined empirically
  if (number <-4294967040.0) return dtostrf_ovf(result);  // constant determined empirically

  // Handle negative numbers
  if (number < 0.0)
  {
     result[n] = '-';
     n += 1;
     number = -number;
  }

  // Round correctly so that print(1.999, 2) prints as "2.00"
  rounding = 0.5;
  for (i=0; i<digits; ++i)
    rounding /= 10.0;

  number += rounding;

  // Extract the integer part of the number and print it
  int_part = (unsigned long)number;
  remainder = number - (double)int_part;
  result[n] = '\0';
  sprintf(result+n, "%d", int_part);
  n = strlen(result);

  // Print the decimal point, but only if there are digits beyond
  if (digits > 0) {
    result[n] = '.';
    n += 1;
  }

  // Extract digits from the remainder one at a time
  while (digits-- > 0)
  {
    remainder *= 10.0;
    toPrint = (unsigned int)(remainder);
    sprintf(result+n, "%d", toPrint);
    n += 1;
    remainder -= toPrint;
  }

  return result;
}
