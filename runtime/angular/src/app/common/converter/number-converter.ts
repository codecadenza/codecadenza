import { LOCALE_ID, Inject, Injectable } from '@angular/core';
import { NumberSymbol, getLocaleNumberSymbol } from '@angular/common';

/**
 * Converter for numeric values
 */
@Injectable({ providedIn: 'root' })
export class NumberConverter {

  /**
   * Inject the locale
   */
  constructor(@Inject(LOCALE_ID) public locale: string) {}

  /**
   * Convert the given value to a number
   */
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  convertToNumber(value: number | string | null): any {
    if (!value) {
      return null;
    }

    // If the input is a number a conversion won't be necessary!
    if (typeof value === 'number') {
      return value;
    }

    // An empty string will be converted to null!
    if (value.length === 0) {
      return null;
    }

    // Whitespace characters may cause problems when parsing the value in the backend!
    if (value.indexOf(' ') !== -1) {
      throw Error(`The string '${value}' could not be converted to a number!`);
    }

    const decimalSymbol = getLocaleNumberSymbol(this.locale, NumberSymbol.Decimal);
    const groupSymbol = getLocaleNumberSymbol(this.locale, NumberSymbol.Group);

    // Remove all group separators. We assume that the decimal symbol occurs only once.
    const numberValue = Number(value.split(groupSymbol).join('').replace(decimalSymbol, '.'));

    if (isNaN(numberValue)) {
      throw Error(`The string '${value}' could not be converted to a number!`);
    }

    return numberValue;
  }

  /**
   * Get the locale of this converter
   */
  getLocale() {
    return this.locale;
  }

}
