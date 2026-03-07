import { inject, Injectable } from '@angular/core';
import { LocaleService } from '../services/locale.service';

/**
 * Converter for numeric values
 */
@Injectable({ providedIn: 'root' })
export class NumberConverter {
  private readonly localeService = inject(LocaleService);

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

    const decimalSymbol = this.localeService.getDecimalSeparator();
    const groupSymbol = this.localeService.getGroupingSeparator();

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
    return this.localeService.getLocale();
  }

}
