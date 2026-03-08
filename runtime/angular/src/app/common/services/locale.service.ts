import { Injectable, inject } from '@angular/core';
import { LOCALE_ID } from '@angular/core';

/**
 * Service that provides the decimal and grouping separators based on the current locale
 */
@Injectable({
  providedIn: 'root'
})
export class LocaleService {
  private static readonly DEFAULT_NUMBER: number = 10000.00;

  private readonly locale = inject(LOCALE_ID);
  private decimalSeparator = '';
  private groupingSeparator = '';

  /**
   * Initializes the decimal and grouping separators based on the injected locale
   */
  constructor() {
    console.log('Determine settings for locale ' + this.locale);

    const numberFormat = new Intl.NumberFormat(this.locale, {
      minimumFractionDigits: 1
    });
    const parts = numberFormat.formatToParts(LocaleService.DEFAULT_NUMBER);

    for (const part of parts) {
      if (part.type === 'decimal' && part.value) {
        this.decimalSeparator = part.value;
      } else if (part.type === 'group' && part.value) {
        this.groupingSeparator = part.value;
      }
    }

    console.log('Using decimal separator: \'' + this.decimalSeparator + '\'');
    console.log('Using grouping separator: \'' + this.groupingSeparator + '\'');
  }

  /**
   * Return the decimal separator for the current locale
   */
  getDecimalSeparator(): string {
    return this.decimalSeparator;
  }

  /**
   * Return the grouping separator for the current locale
   */
  getGroupingSeparator(): string {
    return this.groupingSeparator;
  }

  /**
   * Return the current locale
   */
  getLocale(): string {
    return this.locale;
  }
}
